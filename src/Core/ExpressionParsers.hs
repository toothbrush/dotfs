{-# LANGUAGE NoImplicitPrelude, GADTs, ExistentialQuantification #-}
{-# LANGUAGE Haskell98 #-}
module Core.ExpressionParsers where

import Prelude hiding (lex,lookup)
import Control.Applicative ((<$>))
import Core.Datatypes
import Core.Lexers
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Token as P
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
            ; _ <- symbol lex "{"
            ; thenbody <- exprP
            ; _ <- symbol lex "}"
            ; reservedOp lex "else"
            ; _ <- symbol lex "{"
            ; elsebody <- exprP
            ; _ <- symbol lex "}"
            ; return (If condition thenbody elsebody)
            }
