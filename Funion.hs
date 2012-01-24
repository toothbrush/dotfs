module Main where

import qualified Data.ByteString.Char8 as B
import System.Posix.Types
import System.Posix.Files
import System.FilePath.Posix
import System.Posix.IO
import System.Directory
import System.Environment
import System.Exit
import System.Fuse
import System.IO
import System(getArgs)
import System.Environment(withArgs)
import Control.Monad
import Data.Maybe
import Data.List (nubBy)
import Data.ByteString.Char8 (pack)
import System.Console.GetOpt

version = "0.0.2"

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

data FunionFS = FunionFS {
    funionEntryName     :: FilePath
  , funionActualPath    :: FilePath
  , funionVirtualPath   :: FilePath
  , funionFileStat      :: FileStat
  , funionContents      :: [FunionFS]
  }
 deriving Show


dirContents :: FilePath -> IO [FilePath]
dirContents = fmap (filter (`notElem` [".",".."])) . getDirectoryContents


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
    , funionFileStat    = FileStat { statEntryType = entrytype
        , statFileMode  = fileMode status
        , statLinkCount = linkCount status
        , statFileOwner = fileOwner status
        , statFileGroup = fileGroup status
        , statSpecialDeviceID = specialDeviceID status
        , statFileSize  = fileSize status
        , statBlocks    = 1            -- This is WRONG.  Change
        , statAccessTime= accessTime status
        , statModificationTime = modificationTime status
        , statStatusChangeTime = statusChangeTime status
      }
     , funionContents = []
  }


readDir :: FilePath -> IO (FunionFS)
readDir uri = do
  contents <- dirContents uri
  files    <- filterM (fileExists uri) contents
  fileList <- mapM (getFileStats uri) files
  -- list of directories
  dirs     <- filterM (dirExists uri) contents
  dirList  <- mapM (getDirStats uri) dirs

  return FunionFS {
      funionEntryName   = takeFileName uri
    , funionActualPath  = ""
    , funionVirtualPath = uri
    , funionFileStat    = dirStat
    , funionContents    = fileList ++ dirList
  }

funionLookUp :: [FilePath] -> FilePath -> IO (Maybe FunionFS)
funionLookUp dirsToUnion path = do
  dirs      <- filterM (`dirExists` path) dirsToUnion
  dirList   <- mapM (readDir.(</> path)) dirs
  files     <- filterM (`fileExists` path) dirsToUnion
  fileStats <- mapM (`getFileStats` path) files
  let contents = map funionContents dirList
  case dirs of
    []        -> return $ if length fileStats > 0 then Just $ head fileStats else Nothing
    otherwise -> return $ Just $ if length fileStats > 0 then head fileStats else FunionFS {
            funionEntryName   = takeFileName path
          , funionActualPath  = ""
          , funionVirtualPath = path
          , funionFileStat    = dirStat
          , funionContents    = nubBy (\x y -> funionEntryName x == funionEntryName y)  $ concat contents
        }


funionFSOps :: [FilePath] -> FuseOperations Fd
funionFSOps dir =
  defaultFuseOps{ fuseGetFileStat        = funionGetFileStat dir
                , fuseOpen               = funionOpen dir
                , fuseFlush              = funionFlush dir
                , fuseRead               = funionRead dir
                , fuseOpenDirectory      = funionOpenDirectory dir
                , fuseReadDirectory      = funionReadDirectory dir
                , fuseGetFileSystemStats = funionGetFileSystemStats dir
                }


funionGetFileStat :: [FilePath] -> FilePath -> IO (Either Errno FileStat)
funionGetFileStat dirsToUnion (_:dir) = do
  Just file <- funionLookUp dirsToUnion dir
  return $ Right $ funionFileStat file


funionOpen :: [FilePath] -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno Fd)
funionOpen dirsToUnion (_:path) mode flags = do
  file <- funionLookUp dirsToUnion path
  case file of
    Just f -> do
      fd <- openFd (funionActualPath f) ReadOnly Nothing defaultFileFlags
      return (Right fd)
    Nothing -> return (Left eNOENT)

-- What if 'fd' is no good?  What will happen?
funionFlush :: [FilePath] -> FilePath -> Fd -> IO Errno
funionFlush _ _ fd = do closeFd fd; return eOK


funionOpenDirectory :: [FilePath] -> FilePath -> IO Errno
funionOpenDirectory dirsToUnion (_:path) = do
  extantDirs <- filterM (`dirExists` path) dirsToUnion
  return $ if length extantDirs > 0 then eOK else eNOENT


funionGetFileSystemStats :: [FilePath]->String -> IO (Either Errno FileSystemStats)
funionGetFileSystemStats fileTree  str =
  return $ Right FileSystemStats
    { fsStatBlockSize  = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount  = 5      -- IS THIS CORRECT?
    , fsStatFilesFree  = 10     -- WHAT IS THIS?
    , fsStatMaxNameLength = 255 -- SEEMS SMALL?
    }


funionReadDirectory :: [FilePath] ->FilePath -> IO (Either Errno [(FilePath, FileStat)])
funionReadDirectory dirsToUnion (_:dir) = do
  entry <- funionLookUp dirsToUnion dir
  let contents = funionContents $ fromJust entry
  let dirContents = map (\x -> (funionEntryName x :: String , funionFileStat x)) contents
  return $ Right $ [ (".", dirStat), ("..", dirStat)] ++ dirContents


funionRead  :: [FilePath] -> FilePath -> Fd -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
funionRead dirsToUnion (_:path) fd byteCount offset = do
  (Just file) <- funionLookUp dirsToUnion path
  fdSeek fd AbsoluteSeek offset
  (bytes, num) <- fdRead fd  byteCount
  return $ Right $ pack bytes


dirStat = FileStat { statEntryType = Directory
  , statFileMode = foldr1 unionFileModes
                     [ ownerReadMode
                     , ownerExecuteMode
                     , groupReadMode
                     , groupExecuteMode
                     , otherReadMode
                     , otherExecuteMode
                     ]
  , statLinkCount = 5
  , statFileOwner = 1000
  , statFileGroup = 1000
  , statSpecialDeviceID = 0
  , statFileSize  = 4096
  , statBlocks    = 1
  , statAccessTime= 0
  , statModificationTime = 0
  , statStatusChangeTime = 0
  }


---------------------------------------------------------------------------------
--  Parse arguments and main
---------------------------------------------------------------------------------

data Options = Options {optLog :: String}


defaultOptions = Options { optLog = undefined }


options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "V?" ["version"] (NoArg printVersion) "show version number"
  , Option "l"  ["log"] (ReqArg (\ arg opt -> return opt {optLog = arg})
                          "FILE") "write log to FILE"
  , Option "h"  ["help"] (NoArg printHelp) "show help message"
  ]


printHelp :: Options -> IO (Options)
printHelp _ = do
  prg <- getProgName
  hPutStrLn stderr (usageInfo prg options)
  exitWith ExitSuccess


printVersion :: Options -> IO  (Options)
printVersion _ = do
  hPutStrLn stderr $ "Version " ++ version
  exitWith ExitSuccess


validateDirs :: [String] -> IO (String, [String])
validateDirs dirs
  | length dirs >= 3 = return (head dirs, tail dirs)
  | otherwise        = do hPutStrLn stderr "Wrong number of arguments"; exitWith $ ExitFailure 1


main :: IO ()
main = do
  (args, fuseargs) <- liftM (break (\x -> x == "--")) getArgs
  let (actions, dirList, errors) = getOpt Permute options args

  -- Currently ignoring.  Need to thread logging throughout
  opts <- foldl (>>=) (return defaultOptions) actions

  (mp, dirs) <- validateDirs dirList
  withArgs (mp:fuseargs) $ fuseMain (funionFSOps dirs) defaultExceptionHandler
