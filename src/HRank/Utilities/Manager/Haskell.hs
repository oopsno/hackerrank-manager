module HRank.Utilities.Manager.Haskell where

import Control.Exception
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import System.Directory
import System.FilePath.Posix
import System.Exit
import System.IO
import System.Posix.Temp
import System.Process

pushInMain :: String -> String
pushInMain = unlines . ("module Main where":) . tail . lines

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

