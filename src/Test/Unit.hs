{-# OPTIONS_GHC -fno-warn-unused-imports -fforce-recomp -fth #-}
module Test.Unit where

import Test.Utility -- our TH functions
import Test.Tests -- our test cases
import Control.Monad
import Test.QuickCheck
import Core.Datatypes
import Test.QuickCheck.Test
import System.Exit

runTests :: IO ()
runTests = $(mkChecks tests)

-- Arbitrary instances:

-- there's actually a reason these instances are here, even if
-- they seem like orphans. The TH above requires them, but ghc doesn't
-- realise this at compile time.
instance Arbitrary DFSExpr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM2 UniOp unioperator subform
                                       , liftM3 BiOp bioperator subform subform'
                                       ]
                 where
                   atom = oneof [ liftM Var (elements ["P", "Q", "R", "S"])
                                , liftM Prim arbitrary
                                ]
                   subform  = expr (n `div` 2)
                   subform' = expr (n `div` 4)
                   unioperator = elements [Not]
                   bioperator  = elements [Add, Sub, Mul, Div, And, Or, Eq, LTOp, GTOp, GEQ, LEQ, NEQ]

instance Arbitrary Value where
    arbitrary = oneof [ liftM VBool   arbitrary
                      , liftM VInt    arbitrary
                      , liftM VString arbitraryStr
                      ]
