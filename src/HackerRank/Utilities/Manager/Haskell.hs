module HackerRank.Utilities.Manager.Haskell where

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath.Posix
import System.Exit
import System.IO
import qualified System.IO.Strict as SIO
import System.Posix.Temp
import System.Process

import DynFlags
import GHC
import GHC.Paths ( libdir )
import Outputable

import HackerRank.Utilities.Manager.IOError

parseSource :: FilePath -> IO ParsedSource
parseSource path = do
  source <- readFile path
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let Right (_, p) = parser source dflags path
      return p

transformModule :: (ParsedSource -> ParsedSource) -> FilePath -> IO ParsedSource
transformModule fn = parseSource >=> return . fn

renameModule :: String -> ParsedSource -> ParsedSource
renameModule name = fmap (\hsm -> hsm { hsmodName = fmap (fmap (const $ mkModuleName name)) (hsmodName hsm) })

pushInMain :: FilePath -> IO String
pushInMain = transformModule (renameModule "Main") >=> \res ->
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    return $ showSDocDump dflags $ ppr res

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
  name <- hsmodName . unLoc <$> parseSource path
  return $ maybe "Main" (moduleNameString . unLoc) name

makeImport :: String -> String
makeImport = ("import " ++)

makeImportQ :: String -> String -> String
makeImportQ m alias = unwords [makeImport m, "as", alias]

renderCategory :: String -> [String] -> String
renderCategory name imports = unlines $
  [ "{-|"
  , "Module      : " ++ name
  , "Description : Wrapper for submodules in " ++ name 
  , "License     : CC BY-NC-SA 3.0"
  , "Stability   : experimental"
  , "Portability : POSIX"
  , ""
  , "Just imports all submodules."
  , "-}"
  , ""
  , unwords [ "module", name, "where" ]
  , ""
  ] ++ map makeImport imports

getImports :: FilePath -> IO [String]
getImports path = withDefault "[Manager][DB][getImports]" [] $ do
  imps <- hsmodImports . unLoc <$> parseSource path
  return $ map (moduleNameString . unLoc . ideclName . unLoc) imps
  
