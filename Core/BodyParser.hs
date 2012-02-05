{-# LANGUAGE NoImplicitPrelude #-}
module Core.BodyParser where

import Prelude hiding (lex,lookup)

import Core.Datatypes
import Core.Lexers
import Core.ExpressionParsers
import Core.HelperParsers

import Data.Map

import Control.Applicative ((<*),(<$>),(<*>),(*>),(<$))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Token as P
import Text.Parsec.Prim
import Text.Parsec.Language
import Text.Parsec.Expr


bodyP :: VarParser String
bodyP = concat <$> many blockP 

blockP :: VarParser String
blockP =  try conditionalBlockP 
      <|> try exprBlockP
      <|> verbBlockP


conditionalBlockP :: VarParser String
conditionalBlockP = do{ map <- getState
                      ; symbol lex (extractTagStart map)
                      ; symbol lex "if"
                      ; cond <- boolExprP
                      ; symbol lex (extractTagStop map)
                      ; content <- bodyP
                      ; symbol lex (extractTagStart map)
                      ; symbol lex "endif" <|> symbol lex "/if" <|> symbol lex "fi"
                      ; string (extractTagStop map)
                      ; return $ case cond of
                                      True -> content
                                      False -> ""
                      }
            
exprBlockP :: VarParser String          
exprBlockP = do{ map <- getState
              ; symbol lex (extractTagStart map)
              ; symbol lex "var"
              ; var <- anyExprP
              ; string (extractTagStop map)
              ; return $ case var of
                              VInt i -> show i
                              VBool b -> show b
                              VString s -> s
              }


verbBlockP :: VarParser String
verbBlockP = do{ map <- getState
               ; text <- let opentag = (\x->()) <$> string (extractTagStart map)
                             enofVerb = lookAhead (eof <|> opentag)
                         in many1Till anyChar enofVerb
               ; return text
               }


-- a parser that parses any constant expression
anyExprP :: VarParser Value
anyExprP =  try (VInt <$> intExprP)
        <|> try (VBool <$> boolExprP)
        <|> VString <$> stringVarP




-- helpers to retrieve start and stop tags as string from the state map:
extractTagStart m = case (lookup "tagstart" m) of
                       Just (VString s) -> s
                       _                -> "<<"
                       
extractTagStop m = case (lookup "tagstop" m) of
                       Just (VString s) -> s
                       _                -> ">>"
