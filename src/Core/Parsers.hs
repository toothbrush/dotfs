{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Haskell98 #-}
module Core.Parsers where

import Prelude hiding (lex, lookup, readFile, putStrLn)

import Core.Datatypes
import Core.HeaderParser (headerP, headerRecogniseP)
import Core.HelperParsers
import Core.Lexers
import Core.ExpressionEvaluator
import Core.BodyParser

import Control.Applicative ((<$>))
import Text.Parsec hiding (parseTest)
import Text.Parsec.Token

import Data.Map

import Data.ByteString.Char8 (unpack, pack, ByteString)
import Data.ByteString (readFile, putStrLn)

-- | Test-process a given file, and show the result.
-- Especially useful for testing in combination with GHCi.
testfile :: FilePath -> IO ()
testfile name = do { fc <- readFile name
                   ; let output = process name fc
                   ; putStrLn output
                   ; return ()
                   }


-- run the header parser and evaluator, and then the body parser on the result
process :: FilePath -> ByteString -> ByteString
process file contents =
                       let inp = unpack contents in
                         case runParser headerRecogniseP empty file inp of
                             Left err  -> contents
                             Right _   -> case runParser bodyP empty file inp of
                                Left err     -> pack $ "\n" ++ "error = \n" ++ show err ++ "\n"
                                Right (h,bs) -> pack $ present h bs

present :: Header -> Body -> String
present _ []     = ""
present h (Cond c b:bs) = case eval h c of
                                VBool True -> outputComment h c "if:" ++ present h b ++ outputComment h c "endif:"
                                _          -> outputComment h c "if-hiding; false == "
                            ++ present h bs
present h (Ref r:bs)    = outputComment h r "ref:" ++ show (eval h r) ++ present h bs
present h (Verb v:bs)   = v ++ present h bs

outputComment :: Header -> DFSExpr -> String -> String
outputComment h e note   = case lookup "commentstart" h of
                           Nothing -> ""
                           Just (Prim (VString start)) ->
                             case lookup "commentstop" h of
                             Nothing -> concat               ("\n":[start,note,show e]++["\n"])
                             Just (Prim (VString stop)) -> unwords ([start,note,show e]++[stop])
                             _ -> ""
                           _ -> ""
