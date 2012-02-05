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




-- combinator that outputs the state tupled with the parse result
includeState :: GenParser s st a -> GenParser s st (a,st)
includeState p = do{ res <- p
                   ; state <- getState
                   ; return (res,state)
                   } 

-- parseTest adepted to accept an initial state
parseTest p st inp = case runParser (includeState p) st "" inp of
                          (Left err) -> do{ putStr "parse error at "
                                          ; print err
                                          }
                          (Right (x,state))  -> do{ putStr "result: "
                                                  ; print x
                                                  ; putStr "output state: "
                                                  ; print state
                                                  }
