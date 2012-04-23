{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module ParserSpec (main, spec) where

import           Test.Hspec.HUnit()
import           Test.Hspec.Monadic
import           Test.HUnit
import           Text.Parsec
import qualified Data.Map as Map

import System.DotFS.Core.ExpressionParsers
import System.DotFS.Core.Datatypes

main :: IO ()
main = hspecX spec

fromRight = either undefined id

emptyEnvironment :: DFSState
emptyEnvironment = Map.fromList []

spec :: Specs
spec = do
  describe "boolTerm" $ do
    it "parses constant 'true'" $ do
      assertEqual "" (fromRight $ parse boolTerm "" "true") True

    it "parses constant 'false'" $ do
      assertEqual "" (fromRight $ parse boolTerm "" "false") False

  describe "factor" $ do
    it "parses constants 'true' and 'false'" $ do
      assertEqual "" (fromRight $ runParser factor emptyEnvironment "" "true") (Prim $ VBool True)
      assertEqual "" (fromRight $ runParser factor emptyEnvironment "" "false") (Prim $ VBool False)

    it "parses prefixes of 'true' and 'false' to identifiers" $ do
      assertEqual "" (fromRight $ runParser factor emptyEnvironment "" "tr") (Var "tr")
      assertEqual "" (fromRight $ runParser factor emptyEnvironment "" "fa") (Var "fa")

    it "parses identifiers that start with 'true' and 'false' to identifiers" $ do
      assertEqual "" (fromRight $ runParser factor emptyEnvironment "" "truee") (Var "truee")
      assertEqual "" (fromRight $ runParser factor emptyEnvironment "" "falsee") (Var "falsee")
