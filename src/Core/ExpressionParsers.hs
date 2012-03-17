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
        , op "!=" (BiOp NEQ) AssocNone
    ]
    ]
  where
    op s f       = Infix   (do { reservedOp lex s; return f } <?> "operator")
    pre s f      = Prefix  (do { reservedOp lex s; return f })
    post s f     = Postfix (do { reservedOp lex s; return f })

factor =  parens lex exprP
      <|> ((Prim . VInt) <$> integer lex)
      <|> ((Prim . VBool) <$> boolTerm)
      <|> ((Prim . VString) <$> stringLiteral lex)
      <|> Var <$> identifier lex
      <|> ifTerm
      <?> "simple expression or variable"

boolTerm =  do { reservedOp lex "true"
               ; return True
               }
        <|> do { reservedOp lex "false"
               ; return False
               }

ifTerm = do { reservedOp lex "if"
            ; condition <- parens lex exprP
            ; symbol lex "{"
            ; thenbody <- exprP
            ; symbol lex "}"
            ; reservedOp lex "else"
            ; symbol lex "{"
            ; elsebody <- exprP
            ; symbol lex "}"
            ; return (If condition thenbody elsebody)
            }
