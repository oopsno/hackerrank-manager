module HRank.Utilities.Manager.IOError where

import Control.Exception
import System.Directory
import System.FilePath.Posix
import System.IO

withExsit :: (FilePath -> IO Bool) -> (FilePath -> IO a) -> (FilePath -> IO a) -> FilePath -> IO a
withExsit p m e path = p path >>= \exists -> if exists then m path else e path

withFileExsit :: (FilePath -> IO a) -> (FilePath -> IO a) -> FilePath ->  IO a
withFileExsit = withExsit doesFileExist 

withDirExsit :: (FilePath -> IO a) -> (FilePath -> IO a) -> FilePath -> IO a
withDirExsit = withExsit doesDirectoryExist

catchIOError :: String -> IO a -> IO a -> IO a
catchIOError prefix action hock = catch action (\e ->
  hPutStrLn stderr (prefix ++ ": IOError occurred: " ++ show (e :: IOError)) >> hock)

catchIOError_ :: String -> IO () -> IO ()
catchIOError_ p a = catchIOError p a (return ())

