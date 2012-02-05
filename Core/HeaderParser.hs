{-# LANGUAGE NoImplicitPrelude, GADTs, EmptyDataDecls, KindSignatures, ExistentialQuantification #-}
module Core.HeaderParser where

import Prelude hiding (lex)
import Core.Datatypes
import Core.Lexers
import Core.ExpressionParsers

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

includeState :: GenParser s st a -> GenParser s st (a,st)
includeState p = do{ res <- p
                   ; state <- getState
                   ; return (res,state)
                   } 

parseTest p st inp = case (runParser (includeState p) st "" inp ) of
                                                     (Left err) -> do{ putStr "parse error at "
                                                                           ; print err
                                                                           }
                                                     (Right (x,state))  -> do{ putStr "result: "
                                                                           ; print x
                                                                           ; putStr "output state: "
                                                                           ; print state
                                                                           }

-- first pass:

-- parse the header, no whitespace around it is eaten
dotfsP:: VarParser [()]
dotfsP = symbol lex "<<dotfs" *> many assignmentP <* string ">>"

-- parse an assignment
assignmentP :: VarParser ()
assignmentP = (try tagstyleP
           <|> try commentstyleP
           <|> try boolAssignState
           <|>  intAssignState ) <* optional ( symbol lex ";" )


-- we must prevent comment tags from being ignored by the lexer,
-- so use the default lexer here intead that has no comments
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
--tagstyleP = TagStyle <$  symbol lex "tagstyle"
--                     <*  symbol styleLex "="
--                     <*> operator styleLex
--                     <*  symbol styleLex "tag"
--                     <*> operator lex
commentstyleP = do{ symbol lex "commentstyle" 
                  ; symbol styleLex "="
                  ; s1 <- operator styleLex
                  ; symbol styleLex "comment"
                  ; s2 <- operator lex
                  ; updateState (insert "commentstart" (VString s1))
                  ; updateState (insert "commentstop" (VString s2))
                  ; return ()
                  }
--commentstyleP = CommentStyle <$  symbol lex "commentstyle"
--                             <*  symbol styleLex "="       -- don't eat comment tags
--                             <*> operator styleLex         -- don't
--                             <*  symbol styleLex "comment" -- don't
--                             <*> operator lex              -- after this you can ignore additional comments


-- ok, this is the ugly code duplication part i was talking about
--intAssignP :: VarParser Assignment
--intAssignP = Assign <$> identifier lex
--                    --<*  symbol lex "="
--                    --<*> intExprP
      
intAssignState :: VarParser ()
intAssignState = do{ name <- identifier lex 
                   ; symbol lex "="
                   ; val <- intExprP
                   ; updateState (insert name (VInt val))
                   ; return ()
                   }              
                    
--boolAssignP :: VarParser Bool
--boolAssignP = Assign <$> identifier lex
--                     <*  symbol lex "="
--                     <*> boolExprP

boolAssignState :: VarParser ()
boolAssignState = do{ name <- identifier lex
                    ; symbol lex "="
                    ; val <- boolExprP
                    ; updateState (insert name (VBool val))
                    ; return ()
                    }


