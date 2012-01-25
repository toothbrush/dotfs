module Core.FSActions where

import Core.Datatypes
import Util.Debug
import Core.Constants

import qualified Data.ByteString.Char8 as B
import System.Posix.Types
import System.Posix.Files
import System.FilePath.Posix
import System.Posix.IO
import System.Directory
import System.Console.GetOpt
import System.Exit
import System.Environment
import System.IO
import System.Fuse
import System(getArgs)
import Control.Monad
import Data.Maybe
import Data.List (nub)
import Data.ByteString.Char8 (pack,unpack)

dirContents :: FilePath -> IO [FilePath]
dirContents = fmap (filter (`notElem` [".",".."])) . getDirectoryContents

fileExists, dirExists :: FilePath -> FilePath -> IO Bool
fileExists path name = doesFileExist $ path </> name
dirExists  path name = doesDirectoryExist $ path </> name


getFileStats, getDirStats :: FilePath-> FilePath -> IO FunionFS
getFileStats path name = do p <- canonicalizePath $ path </> name
                            getStats RegularFile p
getDirStats  path name = do p <- canonicalizePath $ path </> name
                            getStats Directory p


getStats :: EntryType -> FilePath -> IO FunionFS
getStats entrytype uri = do
  status <- getFileStatus uri
  children <- case entrytype of
    Directory -> do contents <- dirContents uri
                    -- list of files
                    files    <- filterM (fileExists uri) contents
                    fileList <- mapM (getFileStats uri) files
                    -- list of directories
                    dirs     <- filterM (dirExists uri) contents
                    dirList  <- mapM (getDirStats uri) dirs
                    return $ dirList ++ fileList
    RegularFile -> return []
  return FunionFS {
      funionEntryName   = takeFileName uri
    , funionActualPath  = uri
    , funionVirtualPath = ""
    , funionFileStat    = FileStat
        { statEntryType = entrytype
        , statFileMode  = fileMode status
        , statLinkCount = linkCount status
        , statFileOwner = fileOwner status
        , statFileGroup = fileGroup status
        , statSpecialDeviceID = specialDeviceID status
        , statFileSize  = fileSize status
        , statBlocks    = fromIntegral $ fileSize status `div` 1024
        , statAccessTime= accessTime status
        , statModificationTime = modificationTime status
        , statStatusChangeTime = statusChangeTime status
        }
     , funionContents = children
  }


statIfExists :: FilePath -> FilePath -> IO (Maybe FunionFS)
statIfExists dir file = do
                          existsAsDir <- dir `dirExists` file
                          if existsAsDir then
                                      do debug $ file ++ " is a dir in "++dir
                                         stats <- dir `getDirStats` file
                                         return $ Just stats -- this already contains the children
                             else
                                      do existsAsFile <- dir `fileExists` file
                                         if existsAsFile then do
                                                       debug $ file ++ " is a file in " ++ dir
                                                       stats <- dir `getFileStats` file
                                                       return $ Just stats
                                            else return Nothing

funionFSOps :: Conf -> FuseOperations Fd
funionFSOps dir =
  defaultFuseOps {
                   fuseGetFileStat        = funionGetFileStat dir
                 , fuseGetFileSystemStats = funionGetFileSystemStats dir
                 , fuseOpenDirectory      = funionOpenDirectory dir
                 , fuseReadDirectory      = funionReadDirectory dir
                 , fuseRead               = funionRead dir
                 , fuseOpen               = funionOpen dir
                 , fuseRelease            = funionRelease dir
                 --, fuseSynchronizeFile    = funionSynchronizeFile dir -- this should reload/reparse the file
                 --, fuseReadSymbolicLink   = funionReadSymbolicLink dir -- symlinks already work out the box, but don't present nicely in `ls -la`
                 }


{- possible FUSE operations:
 -- | Implements Unix98 @pread(2)@. It differs from
 --   'System.Posix.Files.fdRead' by the explicit 'FileOffset' argument.
 --   The @fuse.h@ documentation stipulates that this \"should return
 --   exactly the number of bytes requested except on EOF or error,
 --   otherwise the rest of the data will be substituted with zeroes.\"
 fuseRead :: FilePath -> fh -> ByteCount -> FileOffset
          -> IO (Either Errno B.ByteString),
 --
 -- | Check file access permissions; this will be called for the
 --   access() system call.  If the @default_permissions@ mount option
 --   is given, this method is not called.  This method is also not
 --   called under Linux kernel versions 2.4.x
 fuseAccess :: FilePath -> Int -> IO Errno, -- FIXME present a nicer type to Haskell
 -}

{-
 - this is a rather important function for the system.
 - it finds a file by name. the idea is, if a file or directory
 - exists in the conf tree, return that.
 -}
funionLookUp :: Conf -> FilePath -> IO (Maybe FunionFS)
funionLookUp (C confdir) path = do
    confVersion <- statIfExists confdir path
    case confVersion of
        Just stats ->  do let oldFileStat = funionFileStat stats
                              -- TODO set owner to current user
                              newFileStat = oldFileStat {statFileMode = 0o400} -- read only for the owner
                              stats'      = stats {funionFileStat = newFileStat}
                          return $ Just stats'
        Nothing -> return Nothing



funionGetFileStat :: Conf -> FilePath -> IO (Either Errno FileStat)
funionGetFileStat dp (_:dir) = do
  lookup <- funionLookUp dp dir
  case lookup of
    Just file -> return $ Right $ funionFileStat file
    Nothing   -> return $ Left eNOENT



funionOpen :: Conf -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno Fd)
funionOpen dirs (_:path) ReadOnly flags = do
  file <- funionLookUp dirs path
  case file of
    Just f -> do
      fd <- openFd (funionActualPath f) ReadOnly Nothing defaultFileFlags
      return (Right fd)
    Nothing -> return (Left eNOENT)
funionOpen dirs (_:path) mode flags = return (Left eACCES)

funionRelease :: Conf -> FilePath -> Fd -> IO ()
funionRelease _ _ fd = closeFd fd

-- TODO: check permissions with `getPermissions`
funionOpenDirectory :: Conf -> FilePath -> IO Errno
funionOpenDirectory (C confdir) (_:path) = do
  extantDirs <- confdir `dirExists` path
  return $ if extantDirs then eOK else eNOENT


funionReadDirectory :: Conf -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
funionReadDirectory dirs@(C confdir) (_:dir) = do
  entry <- funionLookUp dirs dir
  case entry of
    Nothing -> return $ Left eNOENT
    Just e -> do
        let contents = funionContents e
        let dirContents = map (\x -> (funionEntryName x :: String , funionFileStat x)) contents
        dotstats <- confdir `getDirStats` dir
        return $ Right $ [ (".", funionFileStat dotstats), ("..", dirStat)] ++ dirContents

funionRead  :: Conf -> FilePath -> Fd -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
funionRead dirsToUnion (_:path) fd byteCount offset = do
  --this is unused, so it's probably for error checking. should die gracefully
  --if something goes wrong, not throw unmatched pattern...
  --(Just file) <- funionLookUp dirsToUnion path
  fdSeek fd AbsoluteSeek offset
  (bytes, num) <- fdRead fd  byteCount
  return $ Right $ pack bytes

