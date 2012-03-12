{-# LANGUAGE NoImplicitPrelude, GADTs, ExistentialQuantification #-}
{-# LANGUAGE Haskell98 #-}
module Core.HeaderParser where

import Prelude hiding (lex)

import Core.Datatypes
import Core.Lexers
import Core.ExpressionParsers
import Core.HelperParsers

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
headerP = () <$ symbol lex "<<dotfs" <* many assignmentP <* string ">>"


-- parse an assignment
assignmentP :: VarParser ()
assignmentP = (try tagstyleP
           <|> try commentstyleP
           <|> try boolAssignState
           <|> intAssignState ) <* optional ( symbol lex ";" )


-- we must prevent comment tags from being ignored by the lexer,
-- so use the alternative lexer with great care
tagstyleP,commentstyleP :: VarParser ()
tagstyleP = do{ symbol lex "tagstyle" 
              ; symbol styleLex "="
              ; s1 <- operator styleLex
              ; symbol styleLex "tag"
              ; s2 <- operator lex
              ; updateState (insert "tagstart" (VString s1))
              ; updateState (insert "tagstop" (VString s2))
              ; return ()
              }

commentstyleP = do{ symbol lex "commentstyle" 
                  ; symbol styleLex "="
                  ; s1 <- operator styleLex
                  ; symbol styleLex "comment"
                  ; s2 <- operator lex
                  ; updateState (insert "commentstart" (VString s1))
                  ; updateState (insert "commentstop" (VString s2))
                  ; return ()
                  }


-- statefull interger assignment parser      
intAssignState :: VarParser ()
intAssignState = do{ name <- identifier lex 
                   ; symbol lex "="
                   ; val <- intExprP
                   ; updateState (insert name (VInt val))
                   ; return ()
                   }              

-- and the statefull boolean assignment parser
boolAssignState :: VarParser ()
boolAssignState = do{ name <- identifier lex
                    ; symbol lex "="
                    ; val <- boolExprP
                    ; updateState (insert name (VBool val))
                    ; return ()
                    }


