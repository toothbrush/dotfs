module Core.Datatypes where

import System.Fuse

data DotFS = DotFS {
    dotfsEntryName     :: FilePath
  , dotfsActualPath    :: FilePath
  , dotfsVirtualPath   :: FilePath
  , dotfsFileStat      :: FileStat
  , dotfsContents      :: [DotFS]
  }
 deriving Show

instance Eq DotFS where
  (==) x y = dotfsEntryName x == dotfsEntryName y

data Conf = C FilePath deriving Show


data ConfigFile = Vanilla String
                | Special Header Body
                deriving Show

type Body          = [Block]
type Header        = [KeyVal]

data KeyVal = Setting VariableName Value
              deriving Show



type Value         = String
type Condition     = String
type VariableName  = String

data Block    = FreeText String
              | Conditional Condition Block
              | Variable VariableName
              deriving Show
