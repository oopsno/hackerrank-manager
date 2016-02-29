{-# LANGUAGE TemplateHaskell #-}

module HRank.Utilities.Manager.Haskell where

import Control.Exception
import Control.Monad
import Control.Lens hiding (element)
import Data.List
import Language.Haskell.Exts
import System.Directory
import System.FilePath.Posix
import System.Exit
import System.IO
import qualified System.IO.Strict as SIO
import System.Posix.Temp
import System.Process

import HRank.Utilities.Manager.IOError

data Module' = Module' { _mSrcLoc  :: SrcLoc
                       , _mName    :: ModuleName
                       , _mPragmas :: [ModulePragma]
                       , _mWarning :: Maybe WarningText
                       , _mExport  :: Maybe [ExportSpec]
                       , _mImport  :: [ImportDecl]
                       , _mDecl    :: [Decl] } deriving (Eq, Show)

-- Lenses
makeLenses ''Module'

fromModule :: Module -> Module'
fromModule (Module s n p w e i d) = Module' s n p w e i d

toModule :: Module' -> Module
toModule (Module' s n p w e i d) = Module s n p w e i d

pushInMain :: String -> String
pushInMain code = case parseModule code of
  (ParseOk m) -> prettyPrint . toModule . set mName mMain . fromModule $ m
  (ParseFailed loc err) -> throw . userError $ unwords
      [show err, " at:", show (srcLine loc), ":", show (srcColumn loc)]
  where mMain = ModuleName "Main"
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
  , ""
  ] ++ map makeImport imports

getImports :: FilePath -> IO [String]
getImports path = withDefault "[Manager][DB][getImports]" [] (do
  parseResult <- parseModule <$!> SIO.readFile path
  case parseResult of 
    (ParseOk (Module _ _ _ _ _ xs _)) -> return $ map ((\(ModuleName n) -> n) . importModule) xs
    (ParseFailed loc err) -> ioError . userError $ unwords
      [show err, " at: ", path, ":", show (srcLine loc), ":", show (srcColumn loc)])

