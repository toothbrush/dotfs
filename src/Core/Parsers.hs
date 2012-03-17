{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Haskell98 #-}
module Core.Parsers where

import Prelude hiding (lex, lookup)

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
import Data.List (intersperse)

import Util.Helpers

-- test the parsing on a given file
testfile :: FilePath -> IO ()
testfile name = do { fc <- readFile name
                   ; let output = process name fc
                   ; putStrLn output
                   ; return ()
                   }

fileP :: Bool -- ^ whether to force parsing of an annotated file.
      -> VarParser Config
fileP f = try (do { whiteSpace lex
                   ; headerP
                   ; h <- getState
                   ; b <- bodyP
                   ; eof
                   ; return (Annotated h b)
                   }
                   )
       <|> if f then fail "no header." else Vanilla <$> eatEverything

-- run the header parser and evaluator, and then the body parser on the result
process :: FilePath -> String -> String
process file inp = case runParser (fileP False) empty file inp of
              Left err -> "error = \n" ++ show (errorPos err) ++ "\n"
              Right s  -> case s of
                    Vanilla v     -> v
                    Annotated h b -> present h b

present :: Header -> Body -> String
present h []     = ""
present h (Cond c b:bs) = case eval h c of
                                VBool True -> outputInfoIf h c ++ present h b ++ outputEndIf h c
                                _          -> ""
                            ++ present h bs
present h (Ref r:bs)    = outputInfoRef h r ++ show (eval h r) ++ present h bs
present h (Verb v:bs)   = v ++ present h bs

outputInfoRef :: Header -> DFSExpr -> String
outputInfoRef h e        = case lookup "commentstart" h of
                             Nothing -> ""
                             Just (Prim (VString start)) ->
                               case lookup "commentstop" h of
                               Nothing -> ""
                               Just (Prim (VString stop)) -> unwords [start,"ref:",show e,stop]
outputInfoIf :: Header -> DFSExpr -> String
outputInfoIf h e        = case lookup "commentstart" h of
                            Nothing -> ""
                            Just (Prim (VString start)) ->
                              case lookup "commentstop" h of
                              Nothing -> concat ["\n",start," if: ",show e,"\n"]
                              Just (Prim (VString stop)) -> unwords [start,"if:",show e,stop]
outputEndIf :: Header -> DFSExpr -> String
outputEndIf h e        = case lookup "commentstart" h of
                            Nothing -> ""
                            Just (Prim (VString start)) ->
                              case lookup "commentstop" h of
                              Nothing -> concat ["\n",start," endif: ",show e,"\n"]
                              Just (Prim (VString stop)) -> unwords [start,"endif:",show e,stop]
