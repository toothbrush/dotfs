module Util.Options where

import Util.Version
import System.Exit
import System.Environment
import System.Console.GetOpt
import System.IO

data Options = Options {optLog :: String}


defaultOptions = Options { optLog = undefined }


options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "V?" ["version"] (NoArg printVersion) "show version number"
  , Option "l"  ["log"] (ReqArg (\ arg opt -> return opt {optLog = arg})
                          "FILE") "write log to FILE"
  , Option "h"  ["help"] (NoArg printHelp) "show help message"
  ]


printHelp :: Options -> IO Options
printHelp _ = do
  prg <- getProgName
  hPutStrLn stderr "Usage:"
  hPutStrLn stderr $ "\t"++prg++" mountpoint confdir homedir"
  hPutStrLn stderr (usageInfo prg options)
  exitSuccess


printVersion :: Options -> IO Options
printVersion _ = do
  hPutStrLn stderr $ "Version " ++ version
  exitSuccess


