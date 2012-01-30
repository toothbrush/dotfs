module Core.Parser where

import Core.Datatypes

import Control.Applicative ((<*),(<$>),(<*>),(*>),(<$))
import Control.Monad (join)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Token as P
import Text.Parsec.Prim
import Text.Parsec.Language
import Text.Parsec.Expr

-- stuff about the language and the default lexer
lang :: LanguageDef st
lang = javaStyle
     { reservedNames = ["commentstyle","tagstyle","if","else"]
     , opStart = opLetter headerLang
     , opLetter = oneOf "~!@#$%^&*()_+|`-=\\{}[]:;'<>?,./" -- only used to parse tagsdefs
     , reservedOpNames = [""]                -- things that cannot be the tags-delimiters
     , caseSensitive = True
     }

tp = P.makeTokenParser lang

-- first pass:

headerP :: Parser Header



