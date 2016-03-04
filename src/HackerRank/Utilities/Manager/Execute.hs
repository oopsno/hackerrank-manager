module HackerRank.Utilities.Manager.Execute where

import Control.Monad

import HackerRank.Utilities.Manager.Haskell ( runhaskell, pushInMain )

executeMain :: FilePath -> IO ()
executeMain = pushInMain >=> runhaskell
