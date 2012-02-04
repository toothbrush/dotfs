module Core.HelperParsers where

import Core.Datatypes

import Control.Applicative ((<*),(<$>),(<*>),(*>),(<$))
import Control.Monad (join)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Token as P
import Text.Parsec.Prim
import Text.Parsec.Language
import Text.Parsec.Expr





-- new combinator: (source: http://www.haskell.org/pipermail/beginners/2010-January/003123.html)
many1Till :: Show end => VarParser a -> VarParser end -> VarParser [a]
many1Till p end = do notFollowedBy' end
                     p1 <- p
                     ps <- manyTill p end
                     return (p1:ps) where
                       notFollowedBy' :: Show a => GenParser tok st a -> GenParser tok st ()
                       notFollowedBy' p = try $ join $ do a <- try p
                                                          return (unexpected (show a))
                                                       <|> return (return ())








