module HRank.Mathematics.Fundamentals.FindPoint.Solution where

type Point = (Int, Int)

parse :: String -> [Int]
parse = map read . words

symmetric :: [Int] -> Point
symmetric [px, py, qx, qy] = (2 * qx - px, 2 * qy - py)

printPoint :: Point -> IO ()
printPoint (x, y) = (putStr . show) x >> putChar ' ' >> print y

main :: IO ()
main = fmap (map (symmetric . parse) . tail . lines) getContents >>= mapM_ printPoint
