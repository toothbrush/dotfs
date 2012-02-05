{-#LANGUAGE GADTs, EmptyDataDecls, KindSignatures, ExistentialQuantification #-}
module Core.Datatypes where

import System.Fuse
import Text.Parsec
import Text.ParserCombinators.Parsec.Prim
import Data.Map

data DotFS = DotFS {
    dotfsEntryName     :: FilePath
  , dotfsActualPath    :: FilePath
  , dotfsVirtualPath   :: FilePath
  , dotfsFileStat      :: FileStat
  , dotfsContents      :: [DotFS]
  }
 deriving Show

data Conf = C FilePath deriving Show

instance Eq DotFS where
  (==) x y = dotfsEntryName x == dotfsEntryName y


-- our threaded parser type
type VarParser a = GenParser Char DFSState a

-- and the results
type Varname = String
type DFSState = Map Varname Value
data Value = VInt Int
            | VBool Bool
            | VString String
            deriving Show



