{-# LANGUAGE GADTs #-}

{-# LANGUAGE Haskell98 #-}
module Core.ExpressionEvaluator where

import Core.Datatypes

eval :: Expr a -> VarValue
eval (Int i) = IntVal i
eval _ = undefined
