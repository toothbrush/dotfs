module Instances where

import Test.QuickCheck.Arbitrary
import Control.Monad
import Core.Datatypes
import Test.QuickCheck.Gen

instance Arbitrary DFSExpr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM (uniop) subform
                                       , liftM2 (BiOp Or) subform subform
                                       ]
                 where
                   atom = oneof [liftM Var (elements ["P", "Q", "R", "S"])
                               -- , elements [F,T]
                                ]
                   subform  =  expr (n `div` 2)
                   subform' =  expr (n `div` 4)
                   uniop a  = UniOp Not a

instance Arbitrary Op where
    arbitrary = (elements [Add, Sub, Mul])
