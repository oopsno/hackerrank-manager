module HackerRank.Utilities.Manager.Read where

import Control.Applicative
import Control.Monad
import System.Process

import HackerRank.Utilities.Manager.Haskell ( pushInMain )

copyStringToPasteboard :: String -> IO ()
copyStringToPasteboard = void . readCreateProcessWithExitCode (shell "pbcopy") 

copySource :: FilePath -> IO ()
copySource = rewriteSource >=> copyStringToPasteboard

printSource :: FilePath -> IO ()
printSource = rewriteSource >=> putStrLn

rewriteSource :: FilePath -> IO String
rewriteSource = pushInMain
