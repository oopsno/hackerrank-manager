module HRank.Utilities.Manager.Test where

import System.Directory
import System.FilePath.Posix

import HRank.Utilities.Manager.Haskell

runUTest :: FilePath -> IO ()
runUTest path = do
  solutionSrc <- pushInMain path
  moduleT <- detectModuleName $ replaceBaseName path "UnitTest"
  executeOnTheFly solutionSrc $ \p -> runhaskell $ unlines [ ""
    , "module Main where"
    , ""
    , "import qualified " ++ moduleT ++ " as U"
    , "import qualified Data.Text as T"
    , "import Test.Hspec"
    , "import Test.QuickCheck"
    , "import System.Process"
    , "import Control.Monad"  
    , "sample :: IO ()"
    , "sample = do"
    , "  unless (null U.sampleInput || null U.sampleOutput) $ do"
    , "         hspec $ do"
    , "           describe \"Sample\" $ do"
    , "             it \"should fit sample\" $ do"
    , "               shouldReturn (fmap (T.strip . T.pack)"
    , "                                  (readProcess (\"./\" ++ \"" ++ p ++ "\") [] U.sampleInput))"
    , "                            (T.pack U.sampleOutput)"
    , "  when (null U.sampleInput || null U.sampleOutput) $ do"
    , "    putStrLn \"No available Sample Input/Output\""
    , ""
    , "main :: IO ()"
    , "main = U.main >> Main.sample" ]
