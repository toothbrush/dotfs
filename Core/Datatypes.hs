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

data Version = Conf
             | Home deriving Show

data Conf = C FilePath deriving Show
data Home = H FilePath deriving Show
data DirPair = DP { home :: Home
                  , conf :: Conf
                  }
                 deriving (Show)
