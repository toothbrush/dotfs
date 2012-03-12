{-# LANGUAGE Haskell98 #-}
module Core.PrettyPrinter where

import Core.Datatypes

instance Show ConfigFile where
    show (Vanilla text) = text
    show _ = "unimplemented"
