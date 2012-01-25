module Core.Datatypes where

import System.Fuse

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

