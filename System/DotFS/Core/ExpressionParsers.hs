{-# LANGUAGE NoImplicitPrelude, GADTs, ExistentialQuantification #-}
module System.DotFS.Core.ExpressionParsers where

import Prelude hiding (lex,lookup)
import Control.Applicative ((<$>))
import System.DotFS.Core.Datatypes
import System.DotFS.Core.Lexers
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

factor :: ParsecT String DFSState Identity DFSExpr
factor =  try (parens lex exprP)
      <|> (try $ (Prim . VInt) <$> integer lex)
      <|> (try $ (Prim . VBool) <$> boolTerm)
      <|> (try $ (Prim . VString) <$> stringLiteral lex)
      <|> (try $ Var <$> identifier lex)
      <|> (try ifTerm)
      <?> "simple expression or variable"

boolTerm :: forall u. ParsecT String u Identity Bool
boolTerm =  do { _ <- reserved lex "true"
               ; return True
               }
        <|> do { _ <- reserved lex "false"
               ; return False
               }

ifTerm :: ParsecT String DFSState Identity DFSExpr
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
