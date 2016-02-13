module Main where

import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Maybe
import System.Environment
import System.FilePath.Posix

import HRank.Utilities.Manager.Create  ( createQuiz )
import HRank.Utilities.Manager.Read    ( copySource )
import HRank.Utilities.Manager.Test    ( runUTest   )
import HRank.Utilities.Manager.Execute ( executeMain )
import HRank.Utilities.Manager.DB      ( nameOrPath )

usage :: [a] -> IO ()
usage _ = putStrLn "Usage: hrmng targets [c|r|t|x]"

nameToSol :: FilePath -> IO FilePath
nameToSol = return . (</> "Solution.hs") <=< nameOrPath

route :: [(String, [String] -> IO ())]
route = [ ("c", mapM_ $ createQuiz <=< nameToSol)
        , ("t", mapM_ $ runUTest   <=< nameToSol)
        , ("r", copySource  <=< nameToSol . head)
        , ("x", executeMain <=< nameToSol . head) ]

main :: IO ()
main = do
  (op:args) <- getArgs
  (fromMaybe usage $ lookup op route) args
