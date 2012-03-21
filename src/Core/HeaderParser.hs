{-# LANGUAGE NoImplicitPrelude, GADTs, ExistentialQuantification #-}
{-# LANGUAGE Haskell98 #-}
module Core.HeaderParser where

import Prelude hiding (lex)

import Core.Datatypes
import Core.Lexers
import Core.ExpressionParsers
import Core.ExpressionEvaluator
import Core.HelperParsers (eatEverything)

import Control.Applicative ((<*))
import Text.Parsec hiding (parseTest)
import Text.Parsec.Token as P
import Data.Map

headerRecogniseP = do { _ <- symbol lex "<<dotfs"
                      ; return ()
                      }

-- parse the header, no whitespace around it is eaten
headerP :: VarParser DFSState
headerP = do { _ <- symbol lex "<<dotfs"
             ; whiteSpace lex
             ; _ <- many assignmentP
             ; _ <- string ">>"
             ; getState -- returns the state
             }

-- parse an assignment
assignmentP :: VarParser ()
assignmentP = (try tagstyleP
           <|> try commentstyleP
           <|> try shellCommandP
           <|> assignState
            ) <* ( semi lex <* whiteSpace lex) <?> "assignment"


-- we must prevent comment tags from being ignored by the lexer,
-- so use the alternative lexer with great care
tagstyleP,commentstyleP :: VarParser ()
tagstyleP = do{ _ <- symbol lex "tagstyle"
              ; _ <- symbol styleLex "="
              ; s1 <- operator styleLex
              ; _ <- symbol styleLex "tag"
              ; s2 <- operator lex
              ; updateState (insert "tagstart" (Prim(VString s1)))
              ; updateState (insert "tagstop"  (Prim(VString s2)))
              }

commentstyleP = do{ _ <- symbol lex "commentstyle"
                  ; _ <- symbol styleLex "="
                  ; s1 <- operator styleLex
                  ; updateState (insert "commentstart" (Prim(VString s1)))
                  ; _ <- symbol styleLex "comment"
                  ; optional (do s2 <- operator lex
                                 updateState (insert "commentstop"  (Prim(VString s2))))
                  }

-- | this parses a shell command. These are denoted by using := instead
-- of = for assignment. This is because backticks are a pain to parse, and
-- we prefer the built-in stringLiteral parser.
shellCommandP :: VarParser ()
shellCommandP = do { name <- identifier lex
                   ; whiteSpace lex
                   ; _ <- symbol lex ":="
                   ; whiteSpace lex
                   ; command <- exprP
                   ; s <- getState
                   ; let finalCommand = eval s command
                   ; let e = eval s (Sys (show finalCommand))
                   ; updateState (insert name (Prim e))
}

-- | assignState parses an assignment. That is, an identifier, an equals (=)
-- symbol, and then an expression.
assignState :: VarParser ()
assignState = do{ name <- identifier lex
                ; whiteSpace lex
                ; _ <- symbol lex "="
                ; whiteSpace lex
                ; val <- exprP
                ; s   <- getState
                ; let e = eval s val
                ; updateState (insert name (Prim e))
                }
