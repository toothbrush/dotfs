module Core.HeaderParser where

import Core.Datatypes

import Control.Applicative((<*),(*>),(<$>),(<*>))
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
              , opStart = oneOf "~&=:><"
              , opLetter = oneOf "~&=:><"
              , reservedOpNames = [{- "~", "&", -} "==", ":="]
              , reservedNames = ["true", "false",
                                 "<<if", ">>", "<</if>>"
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
         [Infix (m_reservedOp "==" >> return (Duo Iff)) AssocLeft]
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

headerParser :: Parser Header
headerParser = m_whiteSpace >> assignparser <* eof
    where
      assignparser :: Parser Header
      assignparser = m_semiSep1 assign1
      assign1 = m_whiteSpace >> (
                do { v <- m_identifier
                   ; m_reservedOp ":="
                   ; e <- exprparser
                   ; return (v := e)
                }
                )

exampleheader = parse headerParser "-" "  hullp :=    3  ;\n  two := \"sdfsdf\" == 4 "

-- this still sucks. possibly we should split the input into a
-- list of strings (lines), lex that, then try parsing again.
-- maybe even try a combination of alex&happy?
bodyParser :: Parser Body
bodyParser = m_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Body
      stmtparser = many1 stmt1 -- sepBy1 stmt1 (many1 (string "\n"))
      stmt1 = (
              (
                If <$> ifhdr <*> stmtparser <* m_reserved "\n<</if>>\n"
              )
              <|> do { x <- many1 (noneOf "\n")
                     ; return (FreeText x)
                     }
                     )
      ifhdr = do { condition <- between (optional (string "\n") *> m_reserved "<<if") (m_reserved ">>\n") exprparser
                 ; return condition
      }
