module Core.Parser where
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error
import Text.Parsec.Token
import Text.Parsec.Language


import Core.Datatypes

processConfig :: FilePath -> String -> String
processConfig path fd = case parse mainParseConfig path fd of
                            Left err -> concatMap messageString (errorMessages err)
                            Right cf -> show cf



-- headersep = do
--             string "\n"
--             manyTill (string "-") (string "\n")
--
-- header = do
--             (manyTill anyChar (try headersep))

configFile = do
                    --h <- header
                    rest <- many1 anyChar
                    return $ Special "blank header" [FreeText rest]
                --   <|>
                --   (
                --   rest <- anyChar)




mainParseConfig :: Parser ConfigFile
mainParseConfig = configFile
