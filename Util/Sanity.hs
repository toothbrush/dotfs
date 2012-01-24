module Util.Sanity where

import Core.Datatypes
import Util.Options

import System.IO
import System.Directory
import Control.Monad
import System.Exit

validateDirs :: [String] -> IO (String, DirPair)
validateDirs dirs =
                   do
                      existingDirs <- filterM doesDirectoryExist dirs
                      canonicalDirs <- mapM canonicalizePath existingDirs
                      if length canonicalDirs == 3 then do
                          let (mountpoint : realdirs) = canonicalDirs
                              (c: h: [])              = realdirs
                          return (mountpoint, DP { conf = C c
                                                 , home = H h
                                                 }
                                 )
                         else do
                           hPutStrLn stderr "Wrong number of arguments"
                           printHelp defaultOptions
                           exitWith $ ExitFailure 1


