{-# LANGUAGE Haskell98 #-}
module Main where

import Util.Options
import Util.Sanity
import Core.FSActions

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Fuse
import Control.Monad

{-
 - TODO: we can fill in actualPath now. do we want to though?
 -}

{- TODO
* There's currently no real error checking whatsoever.
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
  withArgs (mp:fuseargs) $ fuseMain (dotFSOps mp dirs) defaultExceptionHandler
