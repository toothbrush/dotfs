{-#LANGUAGE GADTs, EmptyDataDecls, KindSignatures, ExistentialQuantification #-}
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


-- Types for parsed stuff
type Variable a = String
type Header = [Assignment]

data Assignment = forall a. Show a => Assign (Variable a) (Expr a)
                | Execute (Variable String) String
                | TagStyle String String
                | CommentStyle String String

instance Show Assignment where
    show (Assign v e)         = (show v)++" <- "++(show e)++"\n"
    show (TagStyle s1 s2)     = "new tag style: "++s1++" tag "++s2++"\n"
    show (CommentStyle s1 s2) = "new comment style: "++s1++" comment "++s2++"\n"

data Expr a where
         -- constants:
         Int    :: Int -> Expr Int
         Bool   :: Bool -> Expr Bool
         String :: String -> Expr String
         Exec   :: String -> Expr String
         Var    :: Variable a -> Expr a
         -- int operators:
         Add    :: Num a => Expr a -> Expr a -> Expr a
         Sub    :: Expr Int -> Expr Int -> Expr Int
         Mul    :: Expr Int -> Expr Int -> Expr Int
         Div    :: Expr Int -> Expr Int -> Expr Int
         -- string operators:
         Con    :: Show a => Expr String -> Expr a -> Expr String
         -- comparators:
         Eq     :: Eq a => Expr a -> Expr a -> Expr Bool
         Neq    :: Expr a -> Expr a -> Expr Bool
         Gt     :: Ord a => Expr a -> Expr a -> Expr Bool
         -- boolean operators:
         And    :: Expr Bool -> Expr Bool -> Expr Bool
         Or     :: Expr Bool -> Expr Bool -> Expr Bool
         -- conditonal: temporarily: the condition is an integer!!!!!!!!!
         If     :: Expr Int -> Expr a -> Expr a -> Expr a

instance Show (Expr a) where
    show (Int i)     = show i
    show (Bool b)    = show b
    show (String s)  = show s
    show (Exec s)    = '`':s++"`"
    show (Var v)     = v
    show (Add i1 i2) = '(':(show i1)++"+"++(show i2)++")"
    show (Sub i1 i2) = '(':(show i1)++"-"++(show i2)++")"
    show (Mul i1 i2) = '(':(show i1)++"*"++(show i2)++")"
    show (Div i1 i2) = '(':(show i1)++"/"++(show i2)++")"
--    show (Eq e1 e2)  = (show e1)++"+"++(show e2)
--    show (Gt i1 i2)  = (show i1)++">"++(show i2)
--    show (And b1 b2) = (show b1)++"&&"++(show b2)
    show (If c i e)  = "if("++(show c)++"){"++(show i)++"}{"++(show e)++"}"

-- something about names and variables
-- (Varibale a) is a datastructure, that just stores a string
-- Var is the constructor function for an expr, that builds a "constant" expression, namely the value of that variable






