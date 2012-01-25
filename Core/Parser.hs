module Core.Parser where

import Core.Datatypes
import Core.PrettyPrinter

import Control.Applicative ((<*),(<$>),(<*>),(*>))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Token
import Text.Parsec.Prim
import Text.Parsec.Language




-- a header is between these tags.
headerP :: Parser String
headerP = beginHeaderP *> manyTill anyChar endHeaderP

beginHeaderP, endHeaderP :: Parser String
beginHeaderP = spaces *> string "<<dotfile>>"
endHeaderP = string "<</dotfile>>"


-- a body is anything
bodyP :: Parser String
bodyP = many1 anyChar

-- a document is an optional header, and a body
docP :: Parser (String, String)
docP = (,) <$> option "" headerP <*> bodyP


test2 = case (parse docP "" "e>>   ..blaat..   <</dotfile>>\n meer config \n meer!!!") of
         Left err  -> print err
         Right xs  -> print xs
test = case (parse docP "" "   \n \t <<dotfile>>   ..blaat..   <</dotfile>>\n meer config \n meer!!!") of
         Left err  -> print err
         Right xs  -> print xs


processConfig :: FilePath -> String -> String
processConfig path fd = case parse configFile path fd of
                            Left err -> "error = \n" ++ show (errorPos err) ++ "\n"
                            Right cf -> show $ present cf

present :: (String, String) -> ConfigFile
present ("", body)     = Vanilla body
present (header, body) = Special [Setting "" header] [FreeText body]


configFile :: Parser (String, String)
configFile = docP

