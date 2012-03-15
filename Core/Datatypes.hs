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
type Varname = String
type DFSState = Map Varname Value
data Value = VInt Int
            | VBool Bool
            | VString String
            deriving Show



