{-# LANGUAGE NoImplicitPrelude, GADTs, ExistentialQuantification #-}
{-# LANGUAGE Haskell98 #-}
module Core.HeaderParser where

import Prelude hiding (lex)

import Core.Datatypes
import Core.Lexers
import Core.ExpressionParsers
import Core.HelperParsers
import Core.ExpressionEvaluator

import Control.Applicative ((<*),(<$>),(<*>),(*>),(<$))
import Control.Monad (join)
import Text.Parsec hiding (parseTest)
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Token as P
import Text.Parsec.Prim hiding (parseTest)
import Text.Parsec.Language
import Text.Parsec.Expr
import Data.Map

-- parse the header, no whitespace around it is eaten
headerP:: VarParser ()
headerP = do { symbol lex "<<dotfs"
             ; whiteSpace lex
             ; as <- many assignmentP
             ; string ">>"
             ; return ()
             }

-- parse an assignment
assignmentP :: VarParser ()
assignmentP = (try tagstyleP
           <|> try commentstyleP
           <|> try shellCommandP
           <|> assignState
            ) <* ( semi lex <* whiteSpace lex)


-- we must prevent comment tags from being ignored by the lexer,
-- so use the alternative lexer with great care
tagstyleP,commentstyleP :: VarParser ()
tagstyleP = do{ symbol lex "tagstyle"
              ; symbol styleLex "="
              ; s1 <- operator styleLex
              ; symbol styleLex "tag"
              ; s2 <- operator lex
              ; updateState (insert "tagstart" (Prim(VString s1)))
              ; updateState (insert "tagstop"  (Prim(VString s2)))
              }

commentstyleP = do{ symbol lex "commentstyle"
                  ; symbol styleLex "="
                  ; s1 <- operator styleLex
                  ; updateState (insert "commentstart" (Prim(VString s1)))
                  ; symbol styleLex "comment"
                  ; (optional (do s2 <- operator lex
                                  updateState (insert "commentstop"  (Prim(VString s2)))
                        )
                    )
                  }

-- | this parses a shell command. These are denoted by using := instead
-- of = for assignment. This is because backticks are a pain to parse, and
-- we prefer the built-in stringLiteral parser.
shellCommandP :: VarParser ()
shellCommandP = do { name <- identifier lex
                   ; whiteSpace lex
                   ; symbol lex ":="
                   ; whiteSpace lex
                   ; command <- stringLiteral lex
                   ; s <- getState
                   ; let e = eval s (Sys command)
                   ; updateState (insert name (Prim e))
}

-- | assignState parses an assignment. That is, an identifier, an equals (=)
-- symbol, and then an expression.
assignState :: VarParser ()
assignState = do{ name <- identifier lex
                ; whiteSpace lex
                ; symbol lex "="
                ; whiteSpace lex
                ; val <- exprP
                ; s   <- getState
                ; let e = eval s val
                ; updateState (insert name (Prim e))
                }
