{-# LANGUAGE NoImplicitPrelude, GADTs, EmptyDataDecls, KindSignatures, ExistentialQuantification #-}
module Core.ExpressionParsers where

import Prelude hiding (lex,lookup)
import Core.Datatypes
import Core.Lexers

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
import Data.Map



-- to factor out some common basics
nestExprP :: VarParser a -> VarParser a
nestExprP inner =  parens lex inner
               <|> mkIfP inner
           --    <|> mkMediumIfP inner


-- the parser for integer expressions
intExprP :: VarParser Int
intExprP = buildExpressionParser table prim <?> "integer expression"
      where table = [[ inf "*" (*) AssocLeft {-, inf "/" (/) AssocLeft-} ]
                    ,[ inf "+" (+) AssocLeft , inf "-" (-) AssocLeft ]
                    ]                           -- an int expression can be:
--            prim =  parens lex (intExprP)                -- parens with int inside
--                <|> varP                                 -- variable
--                <|> mkIfP intExprP                       -- if-statement of other int-exprs
--                <|> mkMediumIfP intExprP                 -- alternate if syntax
            prim = nestExprP intExprP
                <|> intVarP
                <|> fromInteger <$> integer lex    -- a constant number


-- the parser for boolean expressions
boolExprP :: VarParser Bool
boolExprP = buildExpressionParser table prim <?> "boolean expression"
      where table = [[ pre "!" not ]
                    ,[ inf "&&" (&&) AssocNone ]
                    ,[ inf "||" (||) AssocNone ]
                    ]                       -- a boolean expression can be:
            prim =  try (nestExprP boolExprP)
                <|> try (True  <$ symbol lex "true")       -- constant true
                <|> try (False <$ symbol lex "false")      -- constant false
			    <|> try (mkCompP intExprP)                 -- comparator of integers
				<|> boolVarP                               -- variable
                
--                <|> mkIfP boolExprP                       -- if statements of bools
--                <|> mkMediumIfP boolExprP                 -- and the alternate syntax again





-- the tricky thing with short if statements is
--  that the boolean expression can start with a comparator
--  that starts with an integer, which can be another short if statemt...
--  I do not (yet) know how to handle this recursion.
--  



-- some helpers

--varP :: VarParser (Expr a)
--varP = Var <$> identifier lex

intVarP :: VarParser Int
intVarP = do{ name <- identifier lex
            ; map <- getState
            ; case lookup name map of
                            Just (VInt i)  -> return i
                            Just _         -> fail $ "variable "++name++" is not an integer"
                            Nothing        -> fail $ "variable "++name++" is undefined"
            }
 
boolVarP :: VarParser Bool
boolVarP = do{ name <- identifier lex
             ; map <- getState
             ; case lookup name map of
                            Just (VBool b)     -> return b
                            Just _             -> fail $ "variable "++name++" is not a booleans"
                            Nothing            -> fail $ "variable "++name++" is undefined"
             }

-- the ifP parser generators create a parser of ifstatements of the given type
mkIfP{-,mkShortIfP,mkMediumIfP-} :: VarParser a -> VarParser a
{-
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
             -}
mkIfP p = do{ symbol lex "if"
            ; symbol lex "("
            ; cond <- boolExprP
            ; symbol lex ")"
            ; symbol lex "{"
            ; r1 <- p
            ; symbol lex "}"
            ; symbol lex "{"
            ; r2 <- p
            ; symbol lex "}"
            ; return $ case cond of
                            True -> r1
                            False -> r2 
            }

{-
mkShortIfP p = If <$> boolExprP
                  <*  symbol lex "?"
                  <*> p
                  <*  symbol lex ":"
                  <*> p

mkMediumIfP p = If <$  symbol lex "?"
                   <*> boolExprP
                   <*> p
                   <*> p
-}

-- a combinator that generates a boolean parser
mkCompP :: Ord a => VarParser a -> VarParser Bool
mkCompP p =  try ((==) <$> p <* symbol lex "==" <*> p)
         <|> try ((/=) <$> p <* symbol lex "!=" <*> p)
         <|> try ((>)  <$> p <* symbol lex ">"  <*> p)
         <|> try ((<)  <$> p <* symbol lex "<"  <*> p)
         <|> try ((>=) <$> p <* symbol lex ">=" <*> p)
         <|> try ((<=) <$> p <* symbol lex "<=" <*> p)


-- helpers for easy expression parser table generation
inf s f a = Infix   (do{ symbol lex s; return f }) a
pre s f   = Prefix  (do{ symbol lex s; return f })
post s f  = Postfix (do{ symbol lex s; return f }) 
