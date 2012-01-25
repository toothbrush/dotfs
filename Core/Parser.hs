module Core.Parser where
import Control.Applicative ((<*),(<$>),(<*>),(*>))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Token
import Text.Parsec.Prim
import Text.Parsec.Language


import Core.Datatypes


beginHeaderP, endHeaderP :: Parser String
beginHeaderP = spaces *> string "<<dotfile>>"
endHeaderP = string "<</dotfile>>"

headerP :: Parser String
headerP = beginHeaderP *> manyTill anyChar endHeaderP

bodyP :: Parser String
bodyP = many1 anyChar

docP :: Parser (String, String)
docP = ((,) <$> headerP <*> bodyP) <|> ((\x -> ("",x)) <$> bodyP)

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
present (header, body) = Special header [FreeText body]


configFile :: Parser (String, String)
configFile = docP

