module Core.Parsers where

import Core.Datatypes
import Core.HeaderParser (dotfsP)
import Core.HelperParsers
import Core.Lexers
import Core.ExpressionParsers

import Control.Applicative ((<*),(<$>),(<*>),(*>),(<$))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
--import Text.Parsec.Error
--import Text.Parsec.Token as P
import Text.Parsec.Prim
--import Text.Parsec.Language
--import Text.Parsec.Expr

-- parse the header and return the body untouched
splitter :: Parser (Header,String)
splitter = try ( (\a b c->(b,a++c)) <$> manyTill anyChar (lookAhead (string "<<dotfs"))
                                   <*> dotfsP
                                   <*> many anyChar    )
          <|> (\a->([],a)) <$> many1 anyChar


-- test the parsing on a given file
testfile name = do fc <- readFile name
                   return $ process fc

-- run the header parser and evauator, and then the body parser on the result
process :: String -> String
process inp = case parse splitter "split" inp of
              Left err -> "error = \n" ++show (errorPos err) ++ "\n"
              Right (head,body) -> show (head,body)



evaluate :: Header -> VarList
evaluate (TagStyle _ _:rest) = evaluate rest
evaluate (CommentStyle _ _:rest) = evaluate rest
evaluate (Execute name cmd:rest) =  undefined
evaluate (Assign name val:rest) = undefined






