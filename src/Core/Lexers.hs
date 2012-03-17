{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Haskell98 #-}
module Core.Lexers where

import Prelude hiding (lex)

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token as P


-- stuff about the language and the default lexer
tagletter = oneOf "~!@#$%^&*_+|`-=\\:<>?[]',./"

lang :: LanguageDef st
lang = javaStyle
     { reservedNames = ["commentstyle","tagstyle","if","else","true","false"]
     , caseSensitive = True
     , opStart  = tagletter
     , opLetter = tagletter
     }

lex = P.makeTokenParser lang

-- alternative lexer for style definitions
styleLang :: LanguageDef st
styleLang = emptyDef
          { opStart  = tagletter
          , opLetter = tagletter }

styleLex = P.makeTokenParser styleLang
