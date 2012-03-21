module Core.FSActions where

import Core.Datatypes
import Core.FuseTypes
import Util.Debug
import Core.Constants
import Core.Parsers

import qualified Data.ByteString.Char8 as B
import System.Posix.Types
import System.Posix.Files
import System.FilePath.Posix
import System.Posix.IO
import System.Directory
import System.Fuse
import Control.Monad

import Data.List (intersperse, intercalate)

import Prelude hiding (readFile, length)
import Data.ByteString.Char8 hiding (notElem, map, filter, drop, take, intersperse, concat, intercalate)

dirContents :: FilePath -> IO [FilePath]
dirContents fp = do
                contents <- getDirectoryContents fp
                debug (intercalate "," contents)
                return $ filter (`notElem` [".",".."]) contents

fileExists, dirExists :: FilePath -> FilePath -> IO Bool
fileExists path name = doesFileExist $ path </> name
dirExists  path name = doesDirectoryExist $ path </> name


getGenStats :: FilePath -> FilePath -> IO DotFS
getGenStats path name = do p' <- canonicalizePath $ path </> name
                           let p = path </> name
                           st <- getSymbolicLinkStatus p
                           if isDirectory st then
                             getStats Directory p
                             else
                               if isSymbolicLink st then
                                   getStats SymbolicLink p
                               else --  isRegularFile st then
                                   getStats RegularFile p

getStats :: EntryType -> FilePath -> IO DotFS
getStats entrytype uri = do
  status <- getSymbolicLinkStatus uri
  children <- case entrytype of
    Directory -> do contents <- dirContents uri
                    -- list of files
                    files    <- filterM (fileExists uri) contents
                    fileList <- mapM (getGenStats uri) files
                    -- list of directories
                    dirs     <- filterM (dirExists uri) contents
                    dirList  <- mapM (getGenStats uri) dirs
                    return $ dirList ++ fileList
    RegularFile -> return []
    _           -> return [] -- symlinks etc etc
  sz <- case entrytype of
    Directory -> return $ fileSize status
    RegularFile -> do
      fd <- readFile uri
      let parsed = process uri fd
      return $ fromIntegral (length parsed)
    SymbolicLink -> return 42
    _ -> return 0
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
                                         stats <- dir `getGenStats` file
                                         return $ Just stats -- this already contains the children
                             else
                                      do existsAsFile <- dir `fileExists` file
                                         if existsAsFile then do
                                                       debug $ file ++ " is a file in " ++ dir
                                                       stats <- dir `getGenStats` file
                                                       return $ Just stats
                                            else return Nothing

dotFSOps :: Mountpoint -> Conf -> FuseOperations String
dotFSOps mp dir =
  defaultFuseOps {
                   fuseGetFileStat        = dotfsGetFileStat dir
                 , fuseGetFileSystemStats = dotfsGetFileSystemStats dir
                 , fuseOpenDirectory      = dotfsOpenDirectory dir
                 , fuseReadDirectory      = dotfsReadDirectory dir
                 , fuseRead               = dotfsRead dir
                 , fuseOpen               = dotfsOpen dir
                 , fuseReadSymbolicLink   = dotfsReadSymbolicLink dir mp
                 }

{- | here's quite a bit of logic: we want symlinks to be presented as
 - such, but we would also like links pointing outside the repo to work.
 -}
dotfsReadSymbolicLink :: Conf -> Mountpoint -> FilePath -> IO (Either Errno FilePath)
dotfsReadSymbolicLink (C confdir) mp path = do
                                              let absPathToLink = normalise $ confdir ++ "/" ++ path
                                              linkDestination <- readSymbolicLink absPathToLink
                                              let mountedDestination = mp </> linkDestination
                                              let finalDestination = normalise mountedDestination
                                              let answer = makeRelative mp finalDestination
                                              return $ Right answer

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
  lkup <- dotfsLookUp dp dir
  case lkup of
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
      return (Right $ unpack parsed)
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
        dotstats <- confdir `getGenStats` dir
        return $ Right $ [ (".", dotfsFileStat dotstats), ("..", dirStat)] ++ dirContents

dotfsRead  :: Conf -> FilePath -> String -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
dotfsRead dirsToUnion (_:path) fd byteCount offset = do
  --this is unused, so it's probably for error checking. should die gracefully
  --if something goes wrong, not throw unmatched pattern...
  --(Just file) <- dotfsLookUp dirsToUnion path
  let a = drop (fromIntegral offset) fd
      b = take (fromIntegral byteCount) a
  return $ Right $ pack b

