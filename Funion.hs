{-# LANGUAGE StandaloneDeriving #-}

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
import Data.List (nub)
import Data.ByteString.Char8 (pack,unpack)
import System.Console.GetOpt

version = "0.0.2"

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

data FunionFS = FunionFS {
    funionEntryName     :: FilePath
  , funionActualPath    :: FilePath
  , funionVirtualPath   :: FilePath
  , funionFileStat      :: FileStat
  , funionContents      :: [FunionFS]
  }
 deriving Show

instance Eq FunionFS where
  (==) x y = funionEntryName x == funionEntryName y

data Conf = C FilePath deriving Show
data Home = H FilePath deriving Show
data DirPair = DP { home :: Home
                  , conf :: Conf
                  }
                 deriving (Show)

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


readDir :: FilePath -> FilePath -> IO (FunionFS)
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

funionLookUp :: DirPair -> FilePath -> IO (Maybe FunionFS)
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
                Nothing -> return (Just hV)
                Just cV -> do
                    let confContents = funionContents cV
                        finalContents = nub $ homeContents ++ confContents
                    return $ Just $ hV { funionContents = finalContents }
funionLookUp dirsToUnion path = do
    let (H homedir) = home dirsToUnion
        (C confdir) = conf dirsToUnion
    confVersion <- statIfExists confdir path
    case confVersion of
        Just stats ->  do let oldFileStat = funionFileStat stats
                              -- TODO set owner to current user
                              newFileStat = oldFileStat {statFileMode = 0o400} -- read only for the owner
                              stats'      = stats {funionFileStat = newFileStat}
                          return $ Just stats'
        Nothing -> do homeVersion <- statIfExists homedir path
                      case homeVersion of
                        Just _  -> return homeVersion
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
                                         return $ Just $ stats'
                          else        do existsAsFile <- dir `fileExists` file
                                         if existsAsFile then do
                                                       debug $ file ++ " is a file in " ++ dir
                                                       stats <- dir `getFileStats` file
                                                       return $ Just $ stats
                                         else return Nothing




funionFSOps :: DirPair -> FuseOperations Fd
funionFSOps dir =
  defaultFuseOps {
                   fuseGetFileStat        = funionGetFileStat dir
                 , fuseGetFileSystemStats = funionGetFileSystemStats dir
                 , fuseOpenDirectory      = funionOpenDirectory dir
                 , fuseReadDirectory      = funionReadDirectory dir
                 }
{-
  defaultFuseOps{
                , fuseOpen               = funionOpen dir
                , fuseFlush              = funionFlush dir
                , fuseRead               = funionRead dir
                , fuseWrite              = funionWrite dir
                , fuseAccess             = funionAccess dir
                }
                -}


funionGetFileStat :: DirPair -> FilePath -> IO (Either Errno FileStat)
funionGetFileStat dp (_:dir) = do
  lookup <- funionLookUp dp dir
  case lookup of
    Just file -> return $ Right $ funionFileStat file
    Nothing   -> return $ Left eNOENT
{-
funionAccess :: [FilePath] -> FilePath -> Int -> IO Errno
funionAccess dirsToUnion (_:path)  code = do
  return eOK
  -}



{-
funionOpen :: [FilePath] -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno Fd)
funionOpen dirsToUnion (_:path) mode flags = do
  file <- funionLookUp dirsToUnion path
  case file of
    Just f -> do
      fd <- openFd (funionActualPath f) mode Nothing defaultFileFlags
      return (Right fd)
    Nothing -> return (Left eNOENT)
-}

{-
-- What if 'fd' is no good?  What will happen?
funionFlush :: [FilePath] -> FilePath -> Fd -> IO Errno
funionFlush _ _ fd = do closeFd fd; return eOK
-}


funionOpenDirectory :: DirPair -> FilePath -> IO Errno
funionOpenDirectory dirs (_:path) = do
  let (H homedir) = home dirs
      (C confdir) = conf dirs
      dirsToUnion = [homedir, confdir]
  extantDirs <- filterM (`dirExists` path) dirsToUnion
  return $ if length extantDirs > 0 then eOK else eNOENT


-- TODO: there must be a system call for this.
funionGetFileSystemStats :: DirPair -> String -> IO (Either Errno FileSystemStats)
funionGetFileSystemStats dp str = -- use stats from home dp
  return $ Right FileSystemStats
    { fsStatBlockSize  = 512
    , fsStatBlockCount = 1000
    , fsStatBlocksFree = 1000
    , fsStatBlocksAvailable = 1000
    , fsStatFileCount  = 5      -- IS THIS CORRECT?
    , fsStatFilesFree  = 10     -- WHAT IS THIS?
    , fsStatMaxNameLength = 255 -- SEEMS SMALL?
    }


funionReadDirectory :: DirPair -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
funionReadDirectory dirs (_:dir) = do
  entry <- funionLookUp dirs dir
  let contents = funionContents $ fromJust entry
  let dirContents = map (\x -> (funionEntryName x :: String , funionFileStat x)) contents
  return $ Right $ [ (".", dirStat), ("..", dirStat)] ++ dirContents


{-
funionRead  :: [FilePath] -> FilePath -> Fd -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
funionRead dirsToUnion (_:path) fd byteCount offset = do
  (Just file) <- funionLookUp dirsToUnion path
  fdSeek fd AbsoluteSeek offset
  (bytes, num) <- fdRead fd  byteCount
  return $ Right $ pack bytes
-}


{-
funionWrite :: DirPair -> FilePath -> Fd -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
funionWrite dirsToUnion (_:path) fd content offset = do
  (Just file) <- funionLookUp dirsToUnion path
  fdSeek fd AbsoluteSeek offset
  bytes <- fdWrite fd (unpack content)
  return $ Right $ bytes
  -}

-- TODO: real directory stat, not this fake info.
-- why was this even necessary?
dirStat = FileStat {
    statEntryType = Directory
  , statFileMode = foldr1 unionFileModes
                     [ ownerReadMode
                 --    , ownerWriteMode
                     , ownerExecuteMode
                 --    , groupReadMode
                 --    , groupExecuteMode
                 --    , otherReadMode
                 --    , otherExecuteMode
                 --    , groupWriteMode
                 --    , otherWriteMode
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
  hPutStrLn stderr "Usage:"
  hPutStrLn stderr $ "\t"++prg++" mountpoint confdir homedir"
  hPutStrLn stderr (usageInfo prg options)
  exitWith ExitSuccess


printVersion :: Options -> IO  (Options)
printVersion _ = do
  hPutStrLn stderr $ "Version " ++ version
  exitWith ExitSuccess


validateDirs :: [String] -> IO (String, DirPair)
validateDirs dirs =
                   do
                      existingDirs <- filterM doesDirectoryExist dirs
                      canonicalDirs <- mapM canonicalizePath existingDirs
                      hPutStrLn stderr $ (show existingDirs)
                      if length canonicalDirs == 3 then do
                          let (mountpoint : realdirs) = canonicalDirs
                              (c: h: []) = realdirs
                          return (mountpoint, (DP { conf = C c
                                            , home = H h
                                            }
                                            )
                                 )
                      else do
                        hPutStrLn stderr "Wrong number of arguments"
                        printHelp defaultOptions
                        exitWith $ ExitFailure 1


main :: IO ()
main = do
  (args, fuseargs) <- liftM (break (\x -> x == "--")) getArgs
  let (actions, dirList, errors) = getOpt Permute options args

  -- Currently ignoring.  Need to thread logging throughout
  opts <- foldl (>>=) (return defaultOptions) actions

  (mp, dirs) <- validateDirs dirList
  hPutStrLn stderr ("Mountpoint = \t "++mp)
  hPutStrLn stderr ("     Home  = \t "++(show (home dirs)))
  hPutStrLn stderr ("     Conf  = \t "++(show (conf dirs)))
  withArgs (mp:fuseargs) $ fuseMain (funionFSOps dirs) defaultExceptionHandler
