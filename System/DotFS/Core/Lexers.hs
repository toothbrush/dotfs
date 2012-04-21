{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Haskell98 #-}
{-# LANGUAGE RankNTypes #-}
module System.DotFS.Core.Lexers where

import Prelude hiding (lex)

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token as P
import Data.Functor.Identity


-- stuff about the language and the default lexer
tagletter :: forall u. ParsecT [Char] u Data.Functor.Identity.Identity Char
tagletter = oneOf "~!@#$%^&*_+|`-=\\:<>?[]',./"

lang :: LanguageDef st
lang = javaStyle
     { reservedNames = ["commentstyle","tagstyle","if","else","true","false"]
     , caseSensitive = True
     , opStart  = tagletter
     , opLetter = tagletter
     }

lex :: forall u. GenTokenParser String u Identity
lex = P.makeTokenParser lang

-- alternative lexer for style definitions
styleLang :: LanguageDef st
styleLang = emptyDef
          { opStart  = tagletter
          , opLetter = tagletter }

styleLex :: forall u. GenTokenParser String u Identity
styleLex = P.makeTokenParser styleLang
