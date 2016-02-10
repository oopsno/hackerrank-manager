module HRank.Mathematics.Fundamentals.FindPoint.UnitTest where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Applicative
import System.Process

import HRank.Mathematics.Fundamentals.FindPoint.Solution

sampleInput :: String
sampleInput = "2\n0 0 1 1\n1 1 2 2"

sampleOutput :: String
sampleOutput = "2 2\n3 3"

main :: IO ()
main = hspec $
  describe "parse" $ do
    it "returns empty list" $
      parse "" `shouldBe` ([] :: [Int])
   
    it "returns list" $
      property $ \xs -> parse (unwords $ map show xs) == (xs :: [Int])
