{-# LANGUAGE NoImplicitPrelude, GADTs, EmptyDataDecls, KindSignatures, ExistentialQuantification #-}
module Core.HeaderParser where

import Prelude hiding (lex)
import Core.Datatypes
import Core.Lexers
import Core.ExpressionParsers

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
import Data.HashMap



-- first pass:

-- parse the header, no whitespace around it is eaten
dotfsP:: VarParser Header
dotfsP = symbol lex "<<dotfs" *> many assignmentP <* string ">>"

-- parse an assignment
assignmentP :: VarParser Assignment
assignmentP = (try tagstyleP
           <|> try commentstyleP
           <|> try boolAssignP
           <|> try intAssignP ) <* optional ( symbol lex ";" )


-- we must prevent comment tags from being ignored by the lexer,
-- so use the default lexer here intead that has no comments
tagstyleP,commentstyleP :: VarParser Assignment
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


-- ok, this is the ugly code duplication part i was talking about
intAssignP :: VarParser Assignment
intAssignP = Assign <$> identifier lex
                    <*  symbol lex "="
                    <*> intExprP
boolAssignP :: VarParser Assignment
boolAssignP = Assign <$> identifier lex
                     <*  symbol lex "="
                     <*> boolExprP



