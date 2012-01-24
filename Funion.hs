module Main where

import Util.Options
import Util.Sanity
import Core.Datatypes
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

{-
 - TODO: we can fill in actualPath now. do we want to though?
 -}
{-
OPEN QUESTIONS:
For now, I suppose that I could just specify the directories that I want to union on the command line.
What about making this more aggressive and having some way of specifying unioning rules?  Perhaps
via a DSL?

How should I present SymLinks?
-}

{- TODO(nathan)
* There's currently no real error checking whatsoever.
* Add funionWrite
* Thread in logging
* Can I delete "funionVirtualPath"?
* need to add unit tests
-}


dirContents :: FilePath -> IO [FilePath]
dirContents = fmap (filter (`notElem` [".",".."])) . getDirectoryContents

-- TODO: nicer logging, to a variable filename (State / Reader monad?)
debug :: String -> IO ()
debug str = appendFile "/tmp/foo" (str ++ "\n")

fileExists, dirExists :: FilePath -> FilePath -> IO Bool
fileExists path name = doesFileExist $ path </> name
dirExists  path name = doesDirectoryExist $ path </> name


getFileStats, getDirStats :: FilePath-> FilePath -> IO FunionFS
getFileStats path name = getStats RegularFile (path </> name)
getDirStats  path name = getStats Directory (path </> name)


getStats :: EntryType -> FilePath -> IO FunionFS
getStats entrytype uri = do
  status <- getFileStatus uri
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
        , statBlocks    = 1            -- This is WRONG.  TODO
        , statAccessTime= accessTime status
        , statModificationTime = modificationTime status
        , statStatusChangeTime = statusChangeTime status
        }
     , funionContents = []
  }

{-
 - when my brain isn't spaghetti, what we should do is
 - rely on readDir more (lift some functionality from funionLookUp)
 - and do a proper dirStat.
 -}

readDir :: FilePath -> FilePath -> IO FunionFS
readDir dir file = do
  let uri = dir </> file
  debug $ "reading dir: " ++ uri
  contents <- dirContents uri
  files    <- filterM (fileExists uri) contents
  fileList <- mapM (getFileStats uri) files
  -- list of directories
  dirs     <- filterM (dirExists uri) contents
  dirList  <- mapM (getDirStats uri) dirs

  return FunionFS {
      funionEntryName   = takeFileName uri
    , funionActualPath  = uri
    , funionVirtualPath = uri -- TODO: this isn't even true
    , funionFileStat    = dirStat --TODO do properly.
    , funionContents    = fileList ++ dirList
  }

{-
 - this is a rather important function for the system.
 - it finds a file by name. the idea is, if a file or directory
 - exists in the conf tree, return that. if not, return one
 - that may be in home.
 -
 - the root is a special case: it makes more sense to return the
 - homedir as far as space / writing new files etc goes, but we also
 - want to return the contents of the root of conf as dirContents.
 -}
funionLookUp :: DirPair -> FilePath -> IO (Maybe (FunionFS, Version))
funionLookUp dirsToUnion ""   = do -- this corresponds to a stat (or something)
                                   -- on the root of the unionFS. Therefore,
                                   -- return the root of home. Makes more sense.
        let (H homedir) = home dirsToUnion
            (C confdir) = conf dirsToUnion
        homeVersion <- statIfExists homedir ""
        confVersion <- statIfExists confdir ""
        case homeVersion of
          Nothing -> return Nothing
          Just hV -> do
            let homeContents = funionContents hV
            case confVersion of
                Nothing -> return (Just (hV, Home))
                Just cV -> do
                    let confContents = funionContents cV
                        finalContents = nub $ homeContents ++ confContents
                    return $ Just (hV { funionContents = finalContents }, Home)
funionLookUp dirsToUnion path = do
    let (H homedir) = home dirsToUnion
        (C confdir) = conf dirsToUnion
    confVersion <- statIfExists confdir path
    case confVersion of
        Just stats ->  do let oldFileStat = funionFileStat stats
                              -- TODO set owner to current user
                              newFileStat = oldFileStat {statFileMode = 0o400} -- read only for the owner
                              stats'      = stats {funionFileStat = newFileStat}
                          return $ Just (stats', Conf)
        Nothing -> do homeVersion <- statIfExists homedir path
                      case homeVersion of
                        Just hV -> return $ Just (hV, Home)
                        Nothing -> return Nothing


statIfExists :: FilePath -> FilePath -> IO (Maybe FunionFS)
statIfExists dir file = do
                          existsAsDir <- dir `dirExists` file
                          if existsAsDir then
                                      do debug $ file ++ " is a dir in "++dir
                                         asdf <- readDir dir file
                                         let contents = funionContents asdf
                                         stats <- dir `getDirStats` file
                                         let stats' = stats {funionContents = contents}
                                         return $ Just stats'
                             else
                                      do existsAsFile <- dir `fileExists` file
                                         if existsAsFile then do
                                                       debug $ file ++ " is a file in " ++ dir
                                                       stats <- dir `getFileStats` file
                                                       return $ Just stats
                                            else return Nothing




funionFSOps :: DirPair -> FuseOperations Fd
funionFSOps dir =
  defaultFuseOps {
                   fuseGetFileStat        = funionGetFileStat dir
                 , fuseGetFileSystemStats = funionGetFileSystemStats dir
                 , fuseOpenDirectory      = funionOpenDirectory dir
                 , fuseReadDirectory      = funionReadDirectory dir
                 , fuseRead               = funionRead dir
                 , fuseFlush              = funionFlush dir
                 , fuseOpen               = funionOpen dir
                 }
{-
        -----   , fuseWrite              = funionWrite dir
                -}


funionGetFileStat :: DirPair -> FilePath -> IO (Either Errno FileStat)
funionGetFileStat dp (_:dir) = do
  lookup <- funionLookUp dp dir
  case lookup of
    Just (file,_) -> return $ Right $ funionFileStat file
    Nothing       -> return $ Left eNOENT



funionOpen :: DirPair -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno Fd)
funionOpen dirs (_:path) ReadOnly flags = do
  file <- funionLookUp dirs path
  case file of
    Just (f,_) -> do
      fd <- openFd (funionActualPath f) ReadOnly Nothing defaultFileFlags
      return (Right fd)
    Nothing -> return (Left eNOENT)
funionOpen dirs (_:path) mode flags = do
  file <- funionLookUp dirs path
  case file of
    Just (f,Home) -> do
      fd <- openFd (funionActualPath f) mode Nothing defaultFileFlags
      return (Right fd)
    Just (f,Conf)  -> return (Left eACCES)
    Nothing -> return (Left eNOENT)

-- What if 'fd' is no good?  What will happen?
funionFlush :: DirPair -> FilePath -> Fd -> IO Errno
funionFlush _ _ fd = do closeFd fd; return eOK


funionOpenDirectory :: DirPair -> FilePath -> IO Errno
funionOpenDirectory dirs (_:path) = do
  let (H homedir) = home dirs
      (C confdir) = conf dirs
      dirsToUnion = [homedir, confdir]
  extantDirs <- filterM (`dirExists` path) dirsToUnion
  return $ if length extantDirs > 0 then eOK else eNOENT



funionReadDirectory :: DirPair -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
funionReadDirectory dirs (_:dir) = do
  entry <- funionLookUp dirs dir
  case entry of
    Nothing -> return $ Left eNOENT
    Just (e,_)-> do
        let contents = funionContents e
        let dirContents = map (\x -> (funionEntryName x :: String , funionFileStat x)) contents
        return $ Right $ [ (".", dirStat), ("..", dirStat)] ++ dirContents


funionRead  :: DirPair -> FilePath -> Fd -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
funionRead dirsToUnion (_:path) fd byteCount offset = do
  --this is unused, so it's probably for error checking. should die gracefully
  --if something goes wrong, not throw unmatched pattern...
  --(Just file) <- funionLookUp dirsToUnion path
  fdSeek fd AbsoluteSeek offset
  (bytes, num) <- fdRead fd  byteCount
  return $ Right $ pack bytes


{-
funionWrite :: DirPair -> FilePath -> Fd -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
funionWrite dirsToUnion (_:path) fd content offset = do
  (Just file) <- funionLookUp dirsToUnion path
  fdSeek fd AbsoluteSeek offset
  bytes <- fdWrite fd (unpack content)
  return $ Right $ bytes
  -}

---------------------------------------------------------------------------------
--  Parse arguments and main
---------------------------------------------------------------------------------

main :: IO ()
main = do
  (args, fuseargs) <- liftM (break (== "--")) getArgs
  let (actions, dirList, errors) = getOpt Permute options args

  -- Currently ignoring.  Need to thread logging throughout
  opts <- foldl (>>=) (return defaultOptions) actions

  (mp, dirs) <- validateDirs dirList
  hPutStrLn stderr ("Mountpoint = \t "++mp)
  hPutStrLn stderr ("     Home  = \t "++show (home dirs))
  hPutStrLn stderr ("     Conf  = \t "++show (conf dirs))
  withArgs (mp:fuseargs) $ fuseMain (funionFSOps dirs) defaultExceptionHandler
