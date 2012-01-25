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

type Body          = [Statement]
type Header        = [Assign]

data Assign = VariableName := Expr
              deriving Show

data Expr = Var String
          | Con Constant
          deriving Show

data Constant = S String
              | I Integer
              | B Bool
              deriving Show

type VariableName  = String

data Statement = FreeText String
               | If Expr Statement
               | VarRef VariableName
               deriving Show
