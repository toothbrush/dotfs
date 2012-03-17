module Test.Tests where

import Core.ExpressionParsers
import Core.Datatypes
import Text.Parsec hiding (parseTest)
import Data.Map
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.Prim hiding (parseTest)

prop_parseExpr :: DFSExpr -> Bool
prop_parseExpr xs = xs == testExprP (show xs)

testExprP :: String -> DFSExpr
testExprP inp = case runParser exprP empty "expr" inp of
              Left err -> Prim . VString $ "error = \n" ++ show (errorPos err) ++ "\n"
              Right s  -> s
