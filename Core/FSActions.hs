{-# LANGUAGE Haskell98 #-}
module Core.FSActions where

import Core.Datatypes
import Util.Debug
import Core.Constants
import Core.Parsers

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
import Control.Monad
import Data.Maybe
import Data.List (nub)
import Data.ByteString.Char8 (pack,unpack)

dirContents :: FilePath -> IO [FilePath]
dirContents = fmap (filter (`notElem` [".",".."])) . getDirectoryContents

fileExists, dirExists :: FilePath -> FilePath -> IO Bool
fileExists path name = doesFileExist $ path </> name
dirExists  path name = doesDirectoryExist $ path </> name


getFileStats, getDirStats :: FilePath-> FilePath -> IO DotFS
getFileStats path name = do p <- canonicalizePath $ path </> name
                            getStats RegularFile p
getDirStats  path name = do p <- canonicalizePath $ path </> name
                            getStats Directory p


getStats :: EntryType -> FilePath -> IO DotFS
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
  sz <- case entrytype of
    Directory -> return $ fileSize status
    RegularFile -> do
      fd <- readFile uri
      let parsed = process uri fd
      return $ fromIntegral (length parsed)
  return DotFS {
      dotfsEntryName   = takeFileName uri
    , dotfsActualPath  = uri
    , dotfsVirtualPath = ""
    , dotfsFileStat    = FileStat
        { statEntryType = entrytype
        , statFileMode  = fileMode status
        , statLinkCount = linkCount status
        , statFileOwner = fileOwner status
        , statFileGroup = fileGroup status
        , statSpecialDeviceID = specialDeviceID status
        , statFileSize  = sz
        , statBlocks    = fromIntegral $ sz `div` 1024
        , statAccessTime= accessTime status
        , statModificationTime = modificationTime status
        , statStatusChangeTime = statusChangeTime status
        }
     , dotfsContents = children
  }


statIfExists :: FilePath -> FilePath -> IO (Maybe DotFS)
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

dotFSOps :: Conf -> FuseOperations String
dotFSOps dir =
  defaultFuseOps {
                   fuseGetFileStat        = dotfsGetFileStat dir
                 , fuseGetFileSystemStats = dotfsGetFileSystemStats dir
                 , fuseOpenDirectory      = dotfsOpenDirectory dir
                 , fuseReadDirectory      = dotfsReadDirectory dir
                 , fuseRead               = dotfsRead dir
                 , fuseOpen               = dotfsOpen dir
                 --, fuseReadSymbolicLink   = dotfsReadSymbolicLink dir -- symlinks already work out the box, but don't present nicely in `ls -la`
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
dotfsLookUp :: Conf -> FilePath -> IO (Maybe DotFS)
dotfsLookUp (C confdir) path = do
    confVersion <- statIfExists confdir path
    case confVersion of
        Just stats ->  do let oldFileStat = dotfsFileStat stats
                              -- TODO set owner to current user
                              newFileStat = oldFileStat {statFileMode = 0o400} -- read only for the owner
                              stats'      = stats {dotfsFileStat = newFileStat}
                          return $ Just stats'
        Nothing -> return Nothing



dotfsGetFileStat :: Conf -> FilePath -> IO (Either Errno FileStat)
dotfsGetFileStat dp (_:dir) = do
  lookup <- dotfsLookUp dp dir
  case lookup of
    Just file -> return $ Right $ dotfsFileStat file
    Nothing   -> return $ Left eNOENT



dotfsOpen :: Conf -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno String)
dotfsOpen dirs (_:path) ReadOnly flags = do
  file <- dotfsLookUp dirs path
  case file of
    Just f -> do
      -- at this point load and parse the file.
      fd <- readFile (dotfsActualPath f)

      let parsed = process path fd
      return (Right parsed)
    Nothing -> return (Left eNOENT)
dotfsOpen dirs (_:path) mode flags = return (Left eACCES)

-- TODO: check permissions with `getPermissions`
dotfsOpenDirectory :: Conf -> FilePath -> IO Errno
dotfsOpenDirectory (C confdir) (_:path) = do
  extantDirs <- confdir `dirExists` path
  return $ if extantDirs then eOK else eNOENT


dotfsReadDirectory :: Conf -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
dotfsReadDirectory dirs@(C confdir) (_:dir) = do
  entry <- dotfsLookUp dirs dir
  case entry of
    Nothing -> return $ Left eNOENT
    Just e -> do
        let contents = dotfsContents e
        let dirContents = map (\x -> (dotfsEntryName x :: String , dotfsFileStat x)) contents
        dotstats <- confdir `getDirStats` dir
        return $ Right $ [ (".", dotfsFileStat dotstats), ("..", dirStat)] ++ dirContents

dotfsRead  :: Conf -> FilePath -> String -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
dotfsRead dirsToUnion (_:path) fd byteCount offset = do
  --this is unused, so it's probably for error checking. should die gracefully
  --if something goes wrong, not throw unmatched pattern...
  --(Just file) <- dotfsLookUp dirsToUnion path
  let a = drop (fromIntegral offset) fd
      b = take (fromIntegral byteCount) a
  return $ Right $ pack b

