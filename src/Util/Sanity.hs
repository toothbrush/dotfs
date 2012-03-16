{-# LANGUAGE Haskell98 #-}
module Util.Sanity where

import Core.Datatypes
import Util.Options

import System.IO
import System.Directory
import Control.Monad
import System.Exit

validateDirs :: [String] -> IO (String, Conf)
validateDirs dirs =
                   do
                      existingDirs <- filterM doesDirectoryExist dirs
                      canonicalDirs <- mapM canonicalizePath existingDirs
                      if length canonicalDirs == 2 then do
                          let (mountpoint : confdir : []) = canonicalDirs
                          return (mountpoint, C confdir)
                         else do
                           hPutStrLn stderr "Wrong number of arguments"
                           printHelp defaultOptions
                           exitWith $ ExitFailure 1


