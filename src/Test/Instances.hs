module Test.Instances where

import Test.QuickCheck.Arbitrary
import Control.Monad
import Core.Datatypes
import Test.QuickCheck.Gen

instance Arbitrary DFSExpr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM2 UniOp unioperator subform
                                       , liftM3 BiOp bioperator subform subform
                                       ]
                 where
                   atom = oneof [liftM Var (elements ["P", "Q", "R", "S"])
                           --     ,liftM Prim (elements [F,T])
                                ]
                   subform  = expr (n `div` 2)
                   subform' = expr (n `div` 4)
                   unioperator = elements [Not]
                   bioperator  = (elements [Add, Sub, Mul, Div, And, Or, Eq, LTOp, GTOp, GEQ, LEQ, NEQ])
