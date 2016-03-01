module HRank.Utilities.Manager.Execute where

import Control.Monad

import HRank.Utilities.Manager.Haskell ( runhaskell, pushInMain )

executeMain :: FilePath -> IO ()
executeMain = pushInMain >=> runhaskell
