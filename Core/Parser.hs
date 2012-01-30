{-# LANGUAGE NoImplicitPrelude #-}
module Core.Parser where

import Prelude hiding (lex)
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
tagletter = oneOf "~!@#$%^&*()_+|`-=\\{}:\"<>?[];',./"

lang :: LanguageDef st
lang = javaStyle
     { reservedNames = ["commentstyle","tagstyle","if","else"]
     , caseSensitive = True
     , opStart = tagletter
     , opLetter = tagletter
     }

lex = P.makeTokenParser lang

-- alterantive lexer for style definitions
styleLang :: LanguageDef st
styleLang = emptyDef
          { opStart  = tagletter
          , opLetter = tagletter }

styleLex = P.makeTokenParser styleLang

-- first pass:

-- parse the header, no whitespace around it is eaten
headerP:: Parser Header
headerP = symbol lex "<<dotfs" *> many assignmentP <* string ">>"

-- parse an assignment
assignmentP :: Parser Assignment
assignmentP =  try tagstyleP
           <|> try commentstyleP

-- we must prevent comment tags from being ignored by the lexer,
-- so use the default lexer here intead that has no comments
tagstyleP,commentstyleP :: Parser Assignment
tagstyleP = TagStyle <$  symbol lex "tagstyle"
                     <*  symbol styleLex "="
                     <*> operator styleLex
                     <*  symbol styleLex "tag"
                     <*> operator lex
commentstyleP = CommentStyle <$  symbol lex "commentstyle"
                             <*  symbol styleLex "="       -- don't eat comment tags
                             <*> operator styleLex         -- don't
                             <*  symbol styleLex "comment" -- don't
                             <*> operator lex              -- after this you can ignore additional comments






