{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Haskell98 #-}
module Core.Parsers where

import Prelude hiding (lex)

import Core.Datatypes
import Core.HeaderParser (headerP)
import Core.HelperParsers
import Core.Lexers
import Core.ExpressionParsers
import Core.ExpressionEvaluator
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
                   ; let output = process name fc
                   -- ; parseTest fileP empty fc
                   ; putStrLn output
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
                 ))
     <|> (Vanilla <$> eatEverything)

-- run the header parser and evaluator, and then the body parser on the result
process :: FilePath -> String -> String
process file inp = case runParser fileP empty "main" inp of
              Left err -> "error = \n" ++ show (errorPos err) ++ "\n"
              Right s  -> case s of
                    Vanilla v     -> v
                    Annotated h b -> present h b

present :: Header -> Body -> String
present h []     = ""
present h ((Cond c b):bs) = case eval h c of
                                VBool True -> present h b
                                _          -> ""
                            ++ present h bs
present h ((Ref r):bs)    = show (eval h r)    ++ present h bs
present h ((Verb v):bs)   = v ++ present h bs






