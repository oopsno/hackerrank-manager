module HRank.Utilities.Manager.IOError where

import Control.Exception
import System.Directory
import System.FilePath.Posix
import System.IO
import System.IO.Error
import Text.Show.Pretty ( ppShow )

withExsit :: (FilePath -> IO Bool) -> (FilePath -> IO a) -> (FilePath -> IO a) -> FilePath -> IO a
withExsit p m e path = p path >>= \exists -> if exists then m path else e path

withFileExsit :: (FilePath -> IO a) -> (FilePath -> IO a) -> FilePath ->  IO a
withFileExsit = withExsit doesFileExist 

withDirExsit :: (FilePath -> IO a) -> (FilePath -> IO a) -> FilePath -> IO a
withDirExsit = withExsit doesDirectoryExist

wrapIOError :: String -> IO a -> IO a
wrapIOError prefix = modifyIOError (\e -> userError $ prefix ++ ": IOError occurred: " ++ show e)

withDefault :: Show a => String -> a -> IO a -> IO a
withDefault prefix value action = catchIOError action (\e -> do
  putStrLn (prefix ++ ": Warning: IOError occured: " ++ show e)
  putStrLn ("\t using default value: " ++ ppShow value)
  return value)

