module Core.HeaderParser where

import Core.Datatypes

import Control.Applicative((<*),(*>))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Char
import Text.Parsec.Language

def = emptyDef{
           --     commentStart = "{-"
           --   , commentEnd = "-}"
               identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "~&=:"
              , opLetter = oneOf "~&=:"
              , reservedOpNames = ["~", "&", "=", ":="]
              , reservedNames = ["true", "false",
                                 "if", "then", "else", "fi"
                                ]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , natural = m_natural
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [
        ]
term = m_whiteSpace *>
         (   m_parens exprparser
         <|> fmap (Con . I) m_natural
         <|> fmap (Con . S) (char '"' *> many1 (noneOf "\"") <* char '"')
         <|> fmap Var m_identifier
         <|> (m_reserved "true" >> return (Con (B True)))
         <|> (m_reserved "false" >> return (Con (B False)))
         )
       <* m_whiteSpace
