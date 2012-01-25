module Core.Parser where
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error
import Text.Parsec.Token
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Combinator ()


import Core.Datatypes

processConfig :: FilePath -> String -> String
processConfig path fd = case parse mainParseConfig path fd of
                            Left err -> "error = \n" ++ show (errorPos err) ++ "\n"
                            Right cf -> "parse = \n" ++ show cf



headersep :: Parser String
headersep = do
            char '\n'
            manyTill (char '-') (char '\n')


hdr :: Parser String
hdr = (manyTill anyChar (try headersep))

configFile :: Parser ConfigFile
configFile = do  (do
                    h <- hdr
                    rest <- many1 anyChar
                    return $ Special h [FreeText rest]
                       )
                 <|>
                 (do
                     rest <- many1 anyChar
                     return $ Vanilla rest)




mainParseConfig :: Parser ConfigFile
mainParseConfig = configFile
