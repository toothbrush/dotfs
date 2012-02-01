{-# LANGUAGE NoImplicitPrelude, GADTs, EmptyDataDecls, KindSignatures, ExistentialQuantification #-}
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
assignmentP = ( try tagstyleP
           <|> try commentstyleP
           <|> try boolAssignP
           <|> try intAssignP ) <* optional ( symbol lex ";" )


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


-- ok, this is the ugly code duplication part i was talking about
intAssignP :: Parser Assignment
intAssignP = Assign <$> identifier lex
                    <*  symbol lex "="
                    <*> intExprP
boolAssignP :: Parser Assignment
boolAssignP = Assign <$> identifier lex
                     <*  symbol lex "="
                     <*> boolExprP


-- to factor out some common basics
nestExprP :: Parser (Expr a) -> Parser (Expr a)
nestExprP inner =  varP     -- <- ok this should not be here, but for now this is kinda convenient
               <|> parens lex inner
               <|> mkIfP inner
               <|> mkMediumIfP inner


-- the parser for integer expressions
intExprP :: Parser (Expr Int)
intExprP = buildExpressionParser table prim <?> "integer expression"
      where table = [[ inf "*" Mul AssocLeft , inf "/" Div AssocLeft ]
                    ,[ inf "+" Add AssocLeft , inf "-" Sub AssocLeft ]
                    ]                           -- an int expression can be:
--            prim =  parens lex (intExprP)                -- parens with int inside
--                <|> varP                                 -- variable
--                <|> mkIfP intExprP                       -- if-statement of other int-exprs
--                <|> mkMediumIfP intExprP                 -- alternate if syntax
            prim = nestExprP intExprP
                <|> Int . fromInteger <$> integer lex    -- a constant number


-- the parser for boolean expressions
boolExprP :: Parser (Expr Bool)
boolExprP = buildExpressionParser table prim <?> "boolean expression"
      where table = [[ pre "!" Not ]
                    ,[ inf "&&" And AssocNone ]
                    ,[ inf "||" Or AssocNone ]
                    ]                       -- a boolean expression can be:
            prim = try( nestExprP boolExprP )
--            prim =  parens lex boolExprP                  -- parens with bool inside
--                <|> varP                                  -- variable
                <|> Bool True  <$ symbol lex "true"       -- constant true
                <|> Bool False <$ symbol lex "false"      -- constant false
                <|> mkCompP intExprP                      -- comparator of integers
--                <|> mkIfP boolExprP                       -- if statements of bools
--                <|> mkMediumIfP boolExprP                 -- and the alternate syntax again





-- the tricky thing with short if statements is
--  that the boolean expression can start with a comparator
--  that starts with an integer, which can be another short if statemt...
--  I do not (yet) know how to handle this recursion.
--  



-- some helpers
varP :: Parser (Expr a)
varP = Var <$> identifier lex

-- the ifP parser generators create a parser of ifstatements of the given type
mkIfP,mkShortIfP,mkMediumIfP :: Parser (Expr a) -> Parser (Expr a)
mkIfP p = If <$  symbol lex "if"
             <*  symbol lex "("
             <*> boolExprP
             <*  symbol lex ")"
             <*  symbol lex "{"
             <*> p
             <*  symbol lex "}"
             <*  symbol lex "{"
             <*> p
             <*  symbol lex "}"

mkShortIfP p = If <$> boolExprP
                  <*  symbol lex "?"
                  <*> p
                  <*  symbol lex ":"
                  <*> p

mkMediumIfP p = If <$  symbol lex "?"
                   <*> boolExprP
                   <*> p
                   <*> p

mkCompP :: Ord a => Parser (Expr a) -> Parser (Expr Bool)
mkCompP p =  Eq  <$> p <* symbol lex "==" <*> p
         <|> Neq <$> p <* symbol lex "!=" <*> p
         <|> Gt  <$> p <* symbol lex ">"  <*> p
         <|> Lt  <$> p <* symbol lex "<"  <*> p
         <|> Gte <$> p <* symbol lex ">=" <*> p
         <|> Lte <$> p <* symbol lex "<=" <*> p


-- helpers for easy expression parser table generation
inf s f a = Infix   (do{ symbol lex s; return f }) a
pre s f   = Prefix  (do{ symbol lex s; return f })
post s f  = Postfix (do{ symbol lex s; return f })
