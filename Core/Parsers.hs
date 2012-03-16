{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Haskell98 #-}
module Core.Parsers where

import Prelude hiding (lex)

import Core.Datatypes
import Core.HeaderParser (headerP)
import Core.HelperParsers
import Core.Lexers
import Core.ExpressionParsers
import Core.BodyParser

import Control.Applicative ((<*),(<$>),(<*>),(*>),(<$))
import Text.Parsec hiding (parseTest)
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.Prim hiding (parseTest)

import Data.Map

import Util.Helpers

-- test the parsing on a given file
testfile :: FilePath -> IO ()
testfile name = do { fc <- readFile name
                   ; parseTest fileP empty fc
                   ; return ()
                   }

fileP :: VarParser Config
fileP = (try (do { whiteSpace lex
                 ; headerP
                 ; h <- getState
                 ; b <- bodyP
                 ; eof
                 ; return (Annotated h b)
                 }
                 )) -- end with bodyP. parameterised.
     <|> (Vanilla <$> eatEverything)

-- run the header parser and evauator, and then the body parser on the result
process :: FilePath -> String -> String
process file inp = case runParser fileP empty "main" inp of
              Left err -> "error = \n" ++ show (errorPos err) ++ "\n"
              Right s  -> case s of
                    Vanilla v -> v
                    Annotated h b -> show h ++ show b








