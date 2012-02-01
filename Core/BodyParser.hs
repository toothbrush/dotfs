{-# LANGUAGE NoImplicitPrelude #-}
module Core.BodyParser where

import Core.Datatypes
import Core.Lexers
import Core.ExpressionParsers

import Control.Applicative ((<*),(<$>),(<*>),(*>),(<$))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Token as P
import Text.Parsec.Prim
import Text.Parsec.Language
import Text.Parsec.Expr



