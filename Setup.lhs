#!/usr/bin/env runhaskell
> import Distribution.Simple
> import System.Cmd
> import System.Exit
> 
> main = defaultMainWithHooks (defaultUserHooks { runTests = quickCheck } )
>   where
>   quickCheck _ _ _ _ = do ec <- system $ "ghc --make -odir dist/build -hidir dist/build -idist/build:src src/Unit.hs -main-is Unit.runTests -o unit"
>                           case ec of
>                             ExitSuccess -> System "unit"
>                             _           -> return ec
