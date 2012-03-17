{-# LANGUAGE TemplateHaskell #-}

module Test.Utility where

import Language.Haskell.Syntax
import Language.Haskell.TH
import Data.List
import Test.QuickCheck
import Language.Haskell.Parser
import System.IO.Unsafe
import System.IO

{- | looks in Tests.hs for functions like prop_foo and returns
  the list.  Requires that Tests.hs be valid Haskell98. -}
tests :: [String]
tests = unsafePerformIO $
  do h <- openFile "src/Test/Tests.hs" ReadMode
     s <- hGetContents h
     case parseModule s of
       (ParseOk (HsModule _ _ _ _ ds)) -> return (map declName (filter isProp ds))
       (ParseFailed loc s')            -> error (s' ++ " " ++ show loc)

{- | checks if function binding name starts with @prop_@ indicating
 that it is a quickcheck property -}
isProp :: HsDecl -> Bool
isProp d@(HsFunBind _) = "prop_" `isPrefixOf` (declName d)
isProp _ = False

{- | takes an HsDecl and returns the name of the declaration -}
declName :: HsDecl -> String
declName (HsFunBind (HsMatch _ (HsIdent name) _ _ _:_)) = name
declName _                                              = undefined

mkCheck name = [| putStr (name ++ ": ")
               >> verboseCheck $(varE (mkName name)) |]

mkChecks []        = undefined -- if we don't have any tests, then the test suite is undefined right?
mkChecks [name]    = mkCheck name
mkChecks (name:ns) = [| $(mkCheck name) >> $(mkChecks ns) |]
