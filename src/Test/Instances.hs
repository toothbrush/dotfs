{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances #-}
module Test.Instances where

import Test.QuickCheck.Arbitrary
import Control.Monad
import Core.Datatypes
import Test.QuickCheck.Gen
import Data.Char
import Data.List (nub)

instance Arbitrary DFSExpr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM2 UniOp unioperator subform
                                       , liftM3 BiOp bioperator subform subform
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


-- | unfortunately we need to override the "instance Arbitrary [a]" for strings
arbitraryStr = sized $ \n ->
    do k <- choose (0,n)
       sequence [ arbitraryChar | _ <- [1..k] ]

-- | we don't want newlines in our strings...
arbitraryChar = chr `fmap` oneof [choose (0,9), choose (11,255)]