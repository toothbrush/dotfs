module Util.Debug where

-- TODO: nicer logging, to a variable filename (State / Reader monad?)
debug :: String -> IO ()
debug str = appendFile "/tmp/foo" (str ++ "\n")


