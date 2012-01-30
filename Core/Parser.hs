module Core.Parser where

import Core.Datatypes
import Core.PrettyPrinter

import Control.Applicative ((<*),(<$>),(<*>),(*>))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Token
import Text.Parsec.Prim
import Text.Parsec.Language



