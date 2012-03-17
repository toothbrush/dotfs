{-# OPTIONS_GHC -fno-warn-unused-imports -fforce-recomp -fth #-}
module Main where

import Test.Utility -- our TH functions
import Test.Tests -- our test cases
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Test
import System.Exit

runTests :: IO ()
runTests = $(mkChecks tests)

main :: IO ()
main = runTests
