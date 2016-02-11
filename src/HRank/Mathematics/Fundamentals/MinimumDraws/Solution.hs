module HRank.Mathematics.Fundamentals.MinimumDraws.Solution where

import Control.Applicative
import Control.Monad

main = mapM_ print =<< (map (succ . read) . tail . lines <$> getContents :: IO [Int])
