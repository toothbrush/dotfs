{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Haskell98 #-}
module Core.BodyParser where

import Prelude hiding (lex,lookup)

import Core.Datatypes
import Core.Lexers
import Core.ExpressionParsers
import Core.HelperParsers

import Data.Map

import Control.Applicative ((<$>))
import Text.Parsec
import Text.Parsec.Token as P


bodyP :: VarParser Body
bodyP = many blockP

blockP :: VarParser BodyElem
blockP =  try conditionalBlockP
      <|> try exprBlockP
      <|> verbBlockP


conditionalBlockP :: VarParser BodyElem
conditionalBlockP = do{ state <- getState
                      ; _ <- symbol lex (extractTagStart state)
                      ; _ <- symbol lex "if"
                      ; cond <- exprP
                      ; _ <- string (extractTagStop state)
                      ; content <- bodyP
                      ; _ <- symbol lex (extractTagStart state)
                      ; _ <- symbol lex "endif" <|> symbol lex "/if" <|> symbol lex "fi"
                      ; _ <- string (extractTagStop state)
                      ; return $ Cond cond content
                      }

exprBlockP :: VarParser BodyElem
exprBlockP = do{ state <- getState
               ; _ <- symbol lex (extractTagStart state)
               ; _ <- symbol lex "var"
               ; var <- exprP
               ; _ <- string (extractTagStop state)
               ; return $ Ref var
               }


verbBlockP :: VarParser BodyElem
verbBlockP = do{ map <- getState
               ; let opentag = const () <$> string (extractTagStart map)
                     endofVerb = lookAhead (eof <|> opentag)
                 in Verb <$> many1Till anyChar endofVerb
               }


-- helpers to retrieve start and stop tags as string from the state map:
extractTagStart,extractTagStop :: Map String DFSExpr -> String
extractTagStart m = case lookup "tagstart" m of
                       Just (Prim (VString s)) -> s
                       _                -> "<<"

extractTagStop m = case lookup "tagstop" m of
                       Just (Prim (VString s)) -> s
                       _                -> ">>"
