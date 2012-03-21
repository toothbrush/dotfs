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

-- test the parsing on a given file
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

showMap :: Map VarName DFSExpr -> String
showMap = foldrWithKey (\k v -> (++) $ show k ++ " = " ++ show v ++ "\n" ) ""

present :: Header -> Body -> String
present _ []     = ""
present h (Cond c b:bs) = case eval h c of
                                VBool True -> outputInfoIf h c ++ present h b ++ outputEndIf h c
                                _          -> outputFalseIf h c
                            ++ present h bs
present h (Ref r:bs)    = outputInfoRef h r ++ show (eval h r) ++ present h bs
present h (Verb v:bs)   = v ++ present h bs

-- TODO: this clearly needs cleaning up, merging, and putting in a correct place.
outputInfoRef :: Header -> DFSExpr -> String
outputInfoRef h e        = case lookup "commentstart" h of
                             Nothing -> ""
                             Just (Prim (VString start)) ->
                               case lookup "commentstop" h of
                               Nothing -> ""
                               Just (Prim (VString stop)) -> unwords [start,"ref:",show e,stop]
                               _ -> ""
                             _ -> ""
outputFalseIf :: Header -> DFSExpr -> String
outputFalseIf h e       = case lookup "commentstart" h of
                            Nothing -> ""
                            Just (Prim (VString start)) ->
                              case lookup "commentstop" h of
                              Nothing -> concat ["\n",start," hiding block because: \n",start,"false == ",show e,"\n"]
                              Just (Prim (VString stop)) -> unwords [start,"evals to false: ",show e,stop]
                              _ -> ""
                            _ -> ""
outputInfoIf :: Header -> DFSExpr -> String
outputInfoIf h e        = case lookup "commentstart" h of
                            Nothing -> ""
                            Just (Prim (VString start)) ->
                              case lookup "commentstop" h of
                              Nothing -> concat ["\n",start," if: ",show e,"\n"]
                              Just (Prim (VString stop)) -> unwords [start,"if:",show e,stop]
                              _ -> ""
                            _ -> ""
outputEndIf :: Header -> DFSExpr -> String
outputEndIf h e        = case lookup "commentstart" h of
                            Nothing -> ""
                            Just (Prim (VString start)) ->
                              case lookup "commentstop" h of
                              Nothing -> concat ["\n",start," endif: ",show e,"\n"]
                              Just (Prim (VString stop)) -> unwords [start,"endif:",show e,stop]
                              _ -> ""
                            _ -> ""
