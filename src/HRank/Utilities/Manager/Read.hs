module HRank.Utilities.Manager.Read where

import Control.Monad
import System.Process

import HRank.Utilities.Manager.Haskell

copyStringToPasteboard :: String -> IO ()
copyStringToPasteboard = void . readCreateProcessWithExitCode (shell "pbcopy") 

copySource :: FilePath -> IO ()
copySource path = readFile path >>= copyStringToPasteboard . pushInMain
