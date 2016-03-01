module HRank.Utilities.Manager.Test where

import System.Directory
import System.FilePath.Posix

import HRank.Utilities.Manager.Haskell

runUTest :: FilePath -> IO ()
runUTest path = do
  solutionSrc <- pushInMain path
  moduleT <- detectModuleName $ replaceBaseName path "UnitTest"
  executeOnTheFly solutionSrc $ \p -> do
    let mainSrc = [ "module Main where"
                  , "import qualified " ++ moduleT ++ " as U"
                  , "import qualified Data.Text as T"
                  , "import Test.Hspec"
                  , "import Test.QuickCheck"
                  , "import System.Process"
                  , "import Control.Monad"  
                  , "sample :: IO ()"
                  , "sample ="
                  , "  unless (null U.sampleInput && null U.sampleOutput)"
                  , "         hspec $ describe \"Sample\" $ it \"should fit sample\" $"
                  , "           shouldReturn (fmap (T.strip . T.pack) (readProcess (\"./\" ++ \"" ++ p ++ "\") [] U.sampleInput))"
                  , "                        (T.pack U.sampleOutput)"
                  , "main :: IO ()"
                  , "main = U.main >> Main.sample" ]
    runhaskell $ unlines mainSrc
