{-#LANGUAGE GADTs, ExistentialQuantification #-}
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
           deriving Show


-- a proper AST for our config files

-- | a config file is either just a normal file, to be passed
-- through unchanged, or else it's an annotated file with a header
-- and a body.
data Config = Vanilla String
            | Annotated Header Body
            deriving Show

-- | a header just consists of a number of assignments.
data Header = Assignment VarName DFSExpr
            deriving Show

-- | the body is either a conditional block (shown depending on some
-- boolean value) or a literal expression (usually a variable inserted somewhere)
-- or, of course, verbatim content. These components can be chained.
data Body   = Seq  Body Body
            | Cond DFSExpr Body Body
            | Lit  DFSExpr
            | Verb String
            deriving Show

-- | an expression
data DFSExpr   = Var  VarName
               | Prim Value
               | Sys  String -- maybe this can already be evaluated at parse-time. Ugly though.
               | If DFSExpr DFSExpr DFSExpr
               | UniOp Op DFSExpr
               | BiOp  Op DFSExpr DFSExpr
               deriving Show

data Op     = Add | Sub | Mul | Div
            | Eq  | LTOp| GTOp| LEQ | GEQ
            | And | Or  | Not
            deriving Show
