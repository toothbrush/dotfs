module Main (main) where

import           Test.Hspec.Monadic

import qualified ParserSpec

main :: IO ()
main = hspecX $ do
  describe "Parser" ParserSpec.spec
