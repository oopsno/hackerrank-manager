module Main where

import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Maybe
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import System.IO
import System.Directory
import System.FilePath.Posix
import System.Environment
import System.Process
import System.Posix.Temp


type FuncT = [String] -> IO ()

-- C

createQuiz :: String -> IO ()
createQuiz name = do
  let url = "https://www.hackerrank.com/challenges/" ++ name
  putStrLn url

-- R

adjustCode :: String -> String
adjustCode = unlines . ("module Main where":) . tail . lines

copyStringToPasteboard :: String -> IO ()
copyStringToPasteboard = void . readCreateProcessWithExitCode (shell "pbcopy") 

copySource :: FilePath -> IO ()
copySource path = readFile path >>= copyStringToPasteboard . adjustCode

-- X

executeOnTheFly :: String -> IO ()
executeOnTheFly src = do
  cwd <- getCurrentDirectory
  setCurrentDirectory "src"
  (p, h) <- mkstemps "Main" ".hs"
  let cmd = "ghc -o Main " ++ p
  hPutStrLn h src
  hClose h
  putStrLn $ "[hrmng][Compile]: " ++ cmd
  callCommand cmd
  putStrLn "[hrmng][Excute]: Main"
  catch (callCommand "./Main")
        (\e -> do let err = show (e :: IOException)
                  hPutStrLn stderr ("[hrmng][Errro]: Terminated with non-zero exit code: " ++ err)
                  return ())
  putStrLn "[hrmng][Cleanup]: removing temporary files"
  removeFile p
  removeFile (replaceExtension p "hi")
  removeFile (replaceExtension p "o")
  removeFile "Main"
  setCurrentDirectory cwd

executeMain :: FilePath -> IO ()
executeMain path = do
  src <- readFile path
  executeOnTheFly $ adjustCode src

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
  module_s <- detectModuleName path
  module_t <- detectModuleName $ replaceBaseName path "UnitTest"
  let main_src = [ "module Main where"
                 , "import qualified " ++ module_s ++ " as S"
                 , "import qualified " ++ module_t ++ " as T"
                 , "import qualified HRank.Utilities.UnitTest as U"
                 , "main :: IO ()"
                 , "main = U.runTest S.main T.main" ]
  executeOnTheFly $ unlines main_src

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
