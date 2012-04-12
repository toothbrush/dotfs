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
                          let (confdir : mountpoint : []) = canonicalDirs
                          return (mountpoint, C confdir)
                         else do
                           hPutStrLn stderr "Invalid director(y|ies)"
                           _ <- printHelp defaultOptions
                           exitWith $ ExitFailure 1


