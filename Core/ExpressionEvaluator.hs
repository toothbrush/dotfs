{-# LANGUAGE GADTs #-}

{-# LANGUAGE Haskell98 #-}
module Core.ExpressionEvaluator where

import Core.Datatypes

eval :: DFSExpr -> Value
eval (Int i) = IntVal i
eval _ = undefined
