module HRank.Utilities.Manager.Create where

import System.Process

createQuiz :: String -> IO ()
createQuiz name = callCommand $ unwords ["env python3 scripts/create.py", name]
