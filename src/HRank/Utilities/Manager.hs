module Main where

import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Maybe
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import System.IO
import System.Exit
import System.Directory
import System.FilePath.Posix
import System.Environment
import System.Process
import System.Posix.Temp


type FuncT = [String] -> IO ()

-- C, but why bother?

createQuiz :: String -> IO ()
createQuiz name = callCommand $ unwords ["env python3 scripts/create.py", name]

-- R

adjustCode :: String -> String
adjustCode = unlines . ("module Main where":) . tail . lines

copyStringToPasteboard :: String -> IO ()
copyStringToPasteboard = void . readCreateProcessWithExitCode (shell "pbcopy") 

copySource :: FilePath -> IO ()
copySource path = readFile path >>= copyStringToPasteboard . adjustCode

-- X

compile :: FilePath -> IO ()
compile src = do
  tmpdir <- mkdtemp "GHCTemp"
  let cmd = unwords ["ghc", "--make", "-isrc", "-hidir", tmpdir, "-odir", tmpdir, src]
  putStrLn $ "[hrmng][Compile]: " ++ cmd
  catch (callCommand cmd)
        (\e -> hPutStrLn stderr $ "[hrmng][Errro]: Compile Failed: " ++ show (e :: IOException))

  removeDirectoryRecursive tmpdir

executeOnTheFly :: String -> (FilePath -> IO()) -> IO ()
executeOnTheFly src action = do
  (p, h) <- mkstemps "Main" ".hs"
  let bin = takeBaseName p
  hPutStrLn h src
  hFlush h
  compile p
  action bin
  hClose h
  putStrLn $ unwords ["[hrmng][Cleanup] removing: ", p, bin]
  mapM_ removeFile [p, bin]

runhaskell :: String -> IO ()
runhaskell src = executeOnTheFly src $ \p -> do
  putStrLn $ "[hrmng][Excute]: " ++ p
  catch (callProcess ("./" ++ p) [])
        (\e -> hPutStrLn stderr $ "[hrmng][Errro]: Terminated with non-zero exit code:" ++ show (e :: IOException))

executeMain :: FilePath -> IO ()
executeMain path = readFile path >>= runhaskell . adjustCode

detectModuleName :: FilePath -> IO String
detectModuleName path = do
  src <- readFile path
  case parseModule src of
    ParseOk m -> return $ moduleName m
    ParseFailed loc err -> error $ "Parse error at " ++ formatLoc loc ++ ": " ++ err
  where moduleName :: Module -> String 
        moduleName (Module _ (ModuleName n) _ _ _ _ _) = n
        formatLoc :: SrcLoc -> String
        formatLoc s = show (srcLine s) ++ ":" ++ show (srcColumn s)

-- T

runUTest :: FilePath -> IO ()
runUTest path = do
  solutionSrc <- readFile path
  moduleT <- detectModuleName $ replaceBaseName path "UnitTest"
  executeOnTheFly (adjustCode solutionSrc) $ \p -> do
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
    
usage :: FuncT
usage _ = putStrLn "Usage: hrmng targets [c|r|t|x]"

route :: [(String, FuncT)]
route = [ ("c", mapM_ createQuiz)
        , ("r", copySource . head)
        , ("t", mapM_ runUTest)
        , ("x", executeMain . head) ]

functionOrUsage :: String -> FuncT
functionOrUsage = fromMaybe usage . flip lookup route

main :: IO ()
main = do
  op   <- last <$> getArgs
  args <- init <$> getArgs
  functionOrUsage op args
