module Main where

import Util.Options
import Util.Debug
import Util.Sanity
import Core.Datatypes
import Core.Constants
import Core.FSActions

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
how to parse a file, then return as bytestring in which one can seek etc?

How should I present SymLinks?
-}

{- TODO(nathan)
* There's currently no real error checking whatsoever.
* Add dotfsWrite
* Thread in logging
* Can I delete "dotfsVirtualPath"?
* need to add unit tests
-}


---------------------------------------------------------------------------------
--  Parse arguments and main
---------------------------------------------------------------------------------

main :: IO ()
main = do
  (args, fuseargs) <- liftM (break (== "--")) getArgs -- send arguments after "--" to fusermount
  let (actions, dirList, errors) = getOpt Permute options args

  -- Currently ignoring.  Need to thread logging throughout
  opts <- foldl (>>=) (return defaultOptions) actions

  (mp, dirs) <- validateDirs dirList
  hPutStrLn stderr ("Mountpoint = \t "++mp)
  hPutStrLn stderr ("     Conf  = \t "++show dirs)
  withArgs (mp:fuseargs) $ fuseMain (dotFSOps dirs) defaultExceptionHandler
