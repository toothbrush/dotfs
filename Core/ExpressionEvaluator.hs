{-# LANGUAGE GADTs #-}

{-# LANGUAGE Haskell98 #-}
module Core.ExpressionEvaluator where

import Core.Datatypes
import Data.Maybe
import Data.Map

eval :: DFSState -> DFSExpr -> Value
eval s (Prim p) = p
eval s (Var v)  = eval s $ s!v
eval s (Sys c)  = VString ("to be executed: " ++ c)
eval s (If c t e) = case eval s c of
                        VBool True -> eval s t
                        _          -> eval s e
