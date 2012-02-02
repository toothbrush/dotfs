module Core.Parsers where

import Core.Datatypes
import Core.HeaderParser (dotfsP)
import Core.HelperParsers
import Core.Lexers
import Core.ExpressionParsers
import Core.ExpressionEvaluator

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
testfile :: FilePath -> IO ()
testfile name = do fc <- readFile name
                   putStrLn $ process fc

-- run the header parser and evauator, and then the body parser on the result
process :: String -> String
process inp = case parse splitter "split" inp of
              Left err -> "error = \n" ++show (errorPos err) ++ "\n"
              -- TODO: use the result of evaluate head (which can tell us commentStyle etc
              -- to parse the body.
              Right (head,body) -> show (evaluate head,body)



evaluate :: Header -> VarList
evaluate []                       = []
evaluate (TagStyle _ _     :rest) = evaluate rest
evaluate (CommentStyle _ _ :rest) = evaluate rest
evaluate (Execute name cmd :rest) = (name, StringVal cmd):evaluate rest -- todo: system call
evaluate (Assign name val  :rest) = (name, eval val) : evaluate rest






