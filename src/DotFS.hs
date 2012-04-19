{-# LANGUAGE Haskell98 #-}
module Main where

import Util.Options
import Util.Sanity
import Core.FSActions
import Core.Datatypes
import Data.List

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
  case script opts of
    True  -> hPutStrLn stdout (printScript mp dirs)
    False -> return ()
  withArgs (mp:fuseargs) $ fuseMain (dotFSOps opts mp dirs) defaultExceptionHandler


printScript :: FilePath -> Conf -> String
printScript mountpoint (C confdir) = concat (intersperse "\n"
    [ "#!/bin/bash"
    , "#"
    , "# The idea here is to make symlinks to all the files in "
    , "# the mounted conf directory, inside the user's home (~)"
    , "#"
    , "# Should only need to be run once."
    , "# Will not overwrite files or directories, but will update"
    , "# symlinks, if found."
    , "#"
    , "# (c) Paul van der Walt, April 2012"
    , ""
    , "home=$HOME"
    , "confdir=\"" ++ confdir ++ "\""
    , ""
    , "mountpoint=\"" ++ mountpoint ++ "\""
    , ""
    , "for i in $(ls -a $confdir) ;"
    , "do"
    , "  # skip the breadcrumbs"
    , "  # skip .git too, since it makes shit way slow"
    , "  if [ \"$i\" = \".\" ] || [ \"$i\" = \"..\" ] || [ \"$i\" = \".git\" ] || [ \"${i##*.}\" = \"dontlink\" ]; "
    , "  then "
    , "    continue"
    , "  fi"
    , "  # here we should make symlinks, only if the source"
    , "  # doesn't yet exist"
    , "  newsource=\"$home/$(basename $i)\""
    , "  # note that preserving the tree structure here"
    , "  # happens automatically, since `ls` only goes 1 deep."
    , "  target=\"$mountpoint/$i\""
    , "  target_orig=\"$confdir/$i\""
    , "  if [ -L \"$newsource\" ] ; # -L <=> exists and is symlink"
    , "  then"
    , "    # rm old link"
    , "    echo rm -v \"$newsource\""
    , "  fi"
    , "  # you would think it's now enough to let ln try to make"
    , "  # a symlink, since it'll fail if the newsource exists, but"
    , "  # in the case of directories, it thinks the newsource is"
    , "  # WHERE you'd like the link, i.e. you get stuff like:"
    , "  # `~/.somefolder/.somefolder` -> `mountpoint/.somefolder`"
    , "  echo \"if [ ! -e \"$newsource\" ] ; then # -e <=> file exists\""
    , "  echo \"  if [ -e \"$target_orig.dontlink\" ] ; then\""
    , "  echo \"    ln -s -v \"$target_orig\" \"$newsource\"\""
    , "  echo \"  else\""
    , "  echo \"    ln -s -v \"$target\" \"$newsource\"\""
    , "  echo \"  fi\""
    , "  echo \"fi\""
    , "done"
    ])
