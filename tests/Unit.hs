{-# OPTIONS_GHC -fno-warn-unused-imports -fforce-recomp -fth #-}
module Unit where

import Utility -- our TH functions
import Tests -- our test cases

runTests :: IO ()
runTests = $(mkChecks tests)
