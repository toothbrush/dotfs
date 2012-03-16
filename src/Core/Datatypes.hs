{-# LANGUAGE GADTs, ExistentialQuantification #-}
{-# LANGUAGE Haskell98 #-}
module Core.Datatypes where

import Text.Parsec
import Text.ParserCombinators.Parsec.Prim
import Data.Map


data Conf = C FilePath deriving Show

-- our threaded parser type
type VarParser a = GenParser Char DFSState a

-- and the results
type VarName = String

type DFSState = Map VarName DFSExpr

-- | primitives
data Value = VInt Integer
           | VBool Bool
           | VString String
           deriving Eq

instance Show Value where
  show (VInt    i) = show i
  show (VBool   b) = show b
  show (VString s) = s


-- | a config file is either just a normal file, to be passed
-- through unchanged, or else it's an annotated file with a header
-- and a body.
-- a proper AST for our config files, in other words.
data Config = Vanilla String
            | Annotated Header Body

type Header = DFSState

type Body = [BodyElem]

-- | the body is either a conditional block (shown depending on some
-- boolean value) or a literal expression (usually a variable inserted somewhere)
-- or, of course, verbatim content. These components can be chained.
data BodyElem = Cond DFSExpr Body
              | Ref  DFSExpr
              | Verb String

-- | an expression
data DFSExpr   = Var  VarName
               | Prim Value
               | Sys  String -- maybe this can already be evaluated at parse-time. Ugly though.
               | If DFSExpr DFSExpr DFSExpr
               | UniOp Op DFSExpr
               | BiOp  Op DFSExpr DFSExpr
               deriving Eq

instance Show DFSExpr where
  show (Var  n) = n
  show (Prim v) = show v
  show (Sys  s) = s
  show (If c e1 e2) = "if("++show c++"){"++show e1++"}else{" ++ show e2 ++ "}"
  show (UniOp o e) = show o ++ show e
  show (BiOp  o e1 e2) = show e1++ show o ++ show e2


instance Show Op where
  show Add = "+"



data Op     = Add | Sub | Mul | Div
            | Eq  | LTOp| GTOp| LEQ | GEQ | NEQ
            | And | Or  | Not
            deriving Eq
