{-# LANGUAGE NoImplicitPrelude, GADTs, ExistentialQuantification #-}
{-# LANGUAGE Haskell98 #-}
module Core.ExpressionParsers where

import Prelude hiding (lex,lookup)
import Core.Datatypes
import Core.Lexers
import Data.Map
import Data.Functor.Identity
import Core.HelperParsers
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


exprP :: VarParser DFSExpr
exprP = buildExpressionParser table factor <?> "expression"

table :: [[ Operator String st Identity DFSExpr ]]
table = [
    [pre "!" (UniOp Not)],
    [ op "&&" (BiOp And) AssocNone ],
    [ op "||" (BiOp Or) AssocNone ],
    [ op "*" (BiOp Mul) AssocLeft, op "/" (BiOp Div) AssocLeft ],
    [ op "+" (BiOp Add) AssocLeft, op "-" (BiOp Sub) AssocLeft ],
    [ op "==" (BiOp Eq) AssocNone
        , op ">" (BiOp GTOp) AssocNone
        , op "<" (BiOp LTOp) AssocNone
        , op "<=" (BiOp LEQ) AssocNone
        , op ">=" (BiOp GEQ) AssocNone
    ]
    ]
  where
    op s f assoc = Infix   (do { reservedOp lex s; return f } <?> "operator") assoc
    pre s f      = Prefix  (do { reservedOp lex s; return f })
    post s f     = Postfix (do { reservedOp lex s; return f })

factor =  parens lex exprP
      <|> ((Prim . VInt) <$> natural lex)
      <|> ((Prim . VBool) <$> boolTerm)
      <|> ((Prim . VString) <$> stringLiteral lex)
      <|> Var <$> identifier lex
      <?> "simple expression or variable"

boolTerm =  do { reservedOp lex "true"
               ; return True
               }
        <|> do { reservedOp lex "false"
               ; return False
               }

-- to factor out some common basics
-- nestExprP :: VarParser a -> VarParser a
-- nestExprP inner =  parens lex inner
--                <|> mkIfP inner
--            --    <|> mkMediumIfP inner
-- 

-- -- the parser for boolean expressions
-- boolExprP :: VarParser Bool
-- boolExprP = buildExpressionParser table prim <?> "boolean expression"
--       where table = [[ pre "!" not ]
--                     ,[ inf "&&" (&&) AssocNone ]
--                     ,[ inf "||" (||) AssocNone ]
--                     ]                       -- a boolean expression can be:
--             prim =  try (nestExprP boolExprP)
--                 <|> try (True  <$ symbol lex "true")       -- constant true
--                 <|> try (False <$ symbol lex "false")      -- constant false
-- 			    <|> try (mkCompP intExprP)                 -- comparator of integers
-- 				<|> boolVarP                               -- variable
-- 
-- 
-- 
-- -- some helpers
-- 
-- intVarP :: VarParser Int
-- intVarP = do{ name <- identifier lex
--             ; map <- getState
--             ; case lookup name map of
--                             Just (Prim (VInt i))  -> return i
--                             Just _         -> fail $ "variable "++name++" is not an integer"
--                             Nothing        -> fail $ "variable "++name++" is undefined"
--             }
-- 
-- boolVarP :: VarParser Bool
-- boolVarP = do{ name <- identifier lex
--              ; map <- getState
--              ; case lookup name map of
--                             Just (Prim(VBool b))     -> return b
--                             Just _             -> fail $ "variable "++name++" is not a booleans"
--                             Nothing            -> fail $ "variable "++name++" is undefined"
--              }
-- 
-- stringVarP :: VarParser String
-- stringVarP = do{ name <- identifier lex
--                ; map <- getState
--                ; case lookup name map of
--                       Just (Prim(VString s))  -> return s
--                       Just _             -> fail $ "variable "++name++" is not a string"
--                       Nothing            -> fail $ "variable "++name++" is undefined"
--                }
-- 
-- 
-- -- the ifP parser generators create a parser of ifstatements of the given type
-- mkIfP{-,mkShortIfP,mkMediumIfP-} :: VarParser a -> VarParser a
-- mkIfP p = do{ symbol lex "if"
--             ; symbol lex "("
--             ; cond <- boolExprP
--             ; symbol lex ")"
--             ; symbol lex "{"
--             ; r1 <- p
--             ; symbol lex "}"
--             ; symbol lex "{"
--             ; r2 <- p
--             ; symbol lex "}"
--             ; return $ if cond then r1 else r2
--             }
-- 
-- 
-- -- a combinator that generates a boolean parser
-- mkCompP :: Ord a => VarParser a -> VarParser Bool
-- mkCompP p =  try ((==) <$> p <* symbol lex "==" <*> p)
--          <|> try ((/=) <$> p <* symbol lex "!=" <*> p)
--          <|> try ((>)  <$> p <* symbol lex ">"  <*> p)
--          <|> try ((<)  <$> p <* symbol lex "<"  <*> p)
--          <|> try ((>=) <$> p <* symbol lex ">=" <*> p)
--          <|> try ((<=) <$> p <* symbol lex "<=" <*> p)
-- 
-- 
-- 
-- 
-- 
-- -- helpers for easy expression parser table generation
