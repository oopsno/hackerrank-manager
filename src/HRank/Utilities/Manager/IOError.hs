module HRank.Utilities.Manager.IOError where

import Control.Exception
import System.Directory
import System.FilePath.Posix
import System.IO
import System.IO.Error

withExsit :: (FilePath -> IO Bool) -> (FilePath -> IO a) -> (FilePath -> IO a) -> FilePath -> IO a
withExsit p m e path = p path >>= \exists -> if exists then m path else e path

withFileExsit :: (FilePath -> IO a) -> (FilePath -> IO a) -> FilePath ->  IO a
withFileExsit = withExsit doesFileExist 

withDirExsit :: (FilePath -> IO a) -> (FilePath -> IO a) -> FilePath -> IO a
withDirExsit = withExsit doesDirectoryExist

wrapIOError :: String -> IO a -> IO a
wrapIOError prefix = modifyIOError (userError . flip (++) (prefix ++ ": IOError occurred: ") . show)

withDefault :: a -> IO a -> IO a
withDefault value action = catchIOError action (\e -> return value)

