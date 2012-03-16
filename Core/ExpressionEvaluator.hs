{-# LANGUAGE GADTs #-}

{-# LANGUAGE Haskell98 #-}
module Core.ExpressionEvaluator where

import System.IO
import Control.Applicative
import System.Process
import System.IO.Unsafe

import Core.Datatypes
import Data.Maybe
import Data.Map

eval :: DFSState -> DFSExpr -> Value
eval s (Prim p) = p
eval s (Var v)  = eval s $ s!v
eval s (Sys c)  = VString $ execSystem c
eval s (If c t e) = case eval s c of
                        VBool True -> eval s t
                        _          -> eval s e
eval s o@(UniOp _ e) = evalUni s o
eval s o@(BiOp  _ e1 e2) = evalBi s o


execSystem :: String -> String
execSystem c = unsafePerformIO $ do (inn,out,err,pid) <- runInteractiveCommand c
                                    mapM_ (flip hSetBinaryMode False) [inn, out]
                                    hSetBuffering out NoBuffering
                                    parsedIntro <- parseUntilPrompt out
                                    return (concat parsedIntro)

parseUntilPrompt :: Handle -> IO [String]
parseUntilPrompt out = do
  h <- hIsEOF out
  if h then
      return []
      else do
        latest <- hGetLine out
        (:) <$> return latest <*> parseUntilPrompt out

evalUni :: DFSState -> DFSExpr -> Value
evalUni s (UniOp Not b) = case eval s b of
                            VBool b -> VBool $ not b
                            _       -> VBool False   -- default value?? some way of error reporting here please
evalUni _ _ = VBool False -- no other uni-operators for now.

evalBi :: DFSState -> DFSExpr -> Value
evalBi s (BiOp Add e1 e2)   = doInt s (+) e1 e2
evalBi s (BiOp Sub e1 e2)   = doInt s (-) e1 e2
evalBi s (BiOp Mul e1 e2)   = doInt s (*) e1 e2
evalBi s (BiOp Div e1 e2)   = doInt s (div) e1 e2
evalBi s (BiOp Eq  e1 e2)   = let e1' = eval s e1
                                  e2' = eval s e2
                              in VBool $ e1' == e2'
evalBi s (BiOp LTOp e1 e2)  = doIntBool s (<) e1 e2
evalBi s (BiOp GTOp e1 e2)  = doIntBool s (>) e1 e2
evalBi s (BiOp LEQ  e1 e2)  = doIntBool s (<=) e1 e2
evalBi s (BiOp GEQ  e1 e2)  = doIntBool s (>=) e1 e2
evalBi s (BiOp NEQ  e1 e2)  = let e1' = eval s e1
                                  e2' = eval s e2
                              in VBool $ e1' /= e2'
evalBi s (BiOp And  e1 e2)  = doBool s (&&) e1 e2
evalBi s (BiOp Or   e1 e2)  = doBool s (||) e1 e2

doInt s f a b = let e1' = eval s a
                    e2' = eval s b
                in  e1' `handle` e2'
                where handle (VInt a) (VInt b) = VInt $ f a b
                      handle _ _ = VInt 0

doIntBool s f a b = let e1' = eval s a
                        e2' = eval s b
                    in  e1' `handle` e2'
                    where handle (VInt a) (VInt b) = VBool $ f a b
                          handle _ _ = VBool False

doBool s f a b = let e1' = eval s a
                     e2' = eval s b
                 in  e1' `handle` e2'
                 where handle (VBool a) (VBool b) = VBool $ f a b
                       handle _ _ = VBool False
