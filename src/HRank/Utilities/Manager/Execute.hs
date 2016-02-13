module HRank.Utilities.Manager.Execute where

import HRank.Utilities.Manager.Haskell ( runhaskell, pushInMain )

executeMain :: FilePath -> IO ()
executeMain path = readFile path >>= runhaskell . pushInMain 
