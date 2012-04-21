module System.DotFS.Util.Options where

import System.DotFS.Util.Version
import System.Exit
import System.Environment
import System.Console.GetOpt
import System.IO

data Options = Options {
    optLog :: String,
    script :: Bool
    }


defaultOptions :: Options
defaultOptions = Options { optLog = undefined, script = False }


options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "V?" ["version"] (NoArg printVersion) "show version number"
  , Option "l"  ["log"] (ReqArg (\ arg opt -> return opt {optLog = arg})
                          "FILE") "write log to FILE"
  , Option "h"  ["help"] (NoArg printHelp) "show this help message"
  , Option "g"  ["gen","gen-symlinks"] (NoArg printSyms) "generate a script to set symlinks"
  ]

printSyms :: Options -> IO Options
printSyms os = do
    hPutStrLn stderr "Printing script to stdout."
    return $ os {script = True}

printHelp :: Options -> IO Options
printHelp _ = do
  prg <- getProgName
  hPutStrLn stderr "Usage:"
  hPutStrLn stderr $ "\t"++prg++" [options] confdir mountpoint"
  hPutStrLn stderr (usageInfo prg options)
  exitSuccess


printVersion :: Options -> IO Options
printVersion _ = do
  hPutStrLn stderr $ "DotFS v" ++ version ++ "\n\nCopyright 2012 Sjoerd Timmer, Paul van der Walt"
  exitSuccess


