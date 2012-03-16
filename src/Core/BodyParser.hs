{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Haskell98 #-}
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


bodyP :: VarParser Body
bodyP = many blockP

blockP :: VarParser BodyElem
blockP =  try conditionalBlockP
      <|> try exprBlockP
      <|> verbBlockP


conditionalBlockP :: VarParser BodyElem
conditionalBlockP = do{ map <- getState
                      ; symbol lex (extractTagStart map)
                      ; symbol lex "if"
                      ; cond <- exprP
                      ; symbol lex (extractTagStop map)
                      ; content <- bodyP
                      ; symbol lex (extractTagStart map)
                      ; symbol lex "endif" <|> symbol lex "/if" <|> symbol lex "fi"
                      ; string (extractTagStop map)
                      ; return $ Cond cond content
                      }

exprBlockP :: VarParser BodyElem
exprBlockP = do{ map <- getState
              ; symbol lex (extractTagStart map)
              ; symbol lex "var"
              ; var <- exprP
              ; string (extractTagStop map)
              ; return $ Ref var
              }


verbBlockP :: VarParser BodyElem
verbBlockP = do{ map <- getState
               ; let opentag = const () <$> string (extractTagStart map)
                     endofVerb = lookAhead (eof <|> opentag)
                 in Verb <$> many1Till anyChar endofVerb
               }


-- helpers to retrieve start and stop tags as string from the state map:
extractTagStart m = case lookup "tagstart" m of
                       Just (Prim (VString s)) -> s
                       _                -> "<<"

extractTagStop m = case lookup "tagstop" m of
                       Just (Prim (VString s)) -> s
                       _                -> ">>"
