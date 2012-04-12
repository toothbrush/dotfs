module Test.Tests where

import Core.ExpressionParsers
import Text.Parsec hiding (parseTest)
import Control.Monad
import Core.Datatypes
import Test.QuickCheck.Gen
import Data.Char
import Data.Map

-- import the instances of Arbitrary ... from Test.Unit
-- this uses a trick to prevent circular dependencies
import {-# SOURCE #-} Test.Unit()

prop_parseExpr :: DFSExpr -> Bool
prop_parseExpr xs = xs == testExprP (show xs)

testExprP :: String -> DFSExpr
testExprP inp = case runParser exprP empty "expr" inp of
              Left err -> Prim . VString $ "error = \n" ++ show (errorPos err) ++ "\n"
              Right s  -> s

-- | unfortunately we need to override the "instance Arbitrary [a]" for strings
arbitraryStr :: Gen String
arbitraryStr = sized $ \n ->
    do k <- choose (0,n)
       sequence [ arbitraryChar | _ <- [1..k] ]

-- | we don't want newlines in our strings...
arbitraryChar :: Gen Char
arbitraryChar = chr `fmap` oneof [choose (65,90), choose (97,122)]
