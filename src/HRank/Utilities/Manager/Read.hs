module HRank.Utilities.Manager.Read where

import Control.Applicative
import Control.Monad
import System.Process

import HRank.Utilities.Manager.Haskell

copyStringToPasteboard :: String -> IO ()
copyStringToPasteboard = void . readCreateProcessWithExitCode (shell "pbcopy") 

copySource :: FilePath -> IO ()
copySource = rewriteSource >=> copyStringToPasteboard

printSource :: FilePath -> IO ()
printSource = rewriteSource >=> putStrLn

rewriteSource :: FilePath -> IO String
rewriteSource = return . pushInMain <=< readFile
