module HRank.Utilities.Manager.DB where

import Control.Monad
import Data.Either
import Data.Maybe
import System.Directory
import System.FilePath.Posix

absDBPath :: IO FilePath
absDBPath = (</> ".hrmng/db.hs") <$> getHomeDirectory 

withExsit :: (FilePath -> IO Bool) -> (FilePath -> IO a) -> (FilePath -> IO a) -> FilePath -> IO a
withExsit p m e path = p path >>= \exists -> if exists then m path else e path

withFileExsit :: (FilePath -> IO a) -> (FilePath -> IO a) -> FilePath ->  IO a
withFileExsit = withExsit doesFileExist 

withDirExsit :: (FilePath -> IO a) -> (FilePath -> IO a) -> FilePath -> IO a
withDirExsit = withExsit doesDirectoryExist

readDB :: IO [(String, FilePath)]
readDB = absDBPath >>= withFileExsit (\p -> read <$> readFile p) (\_ -> return [])

writeDB :: [(String, FilePath)] -> IO ()
writeDB xs = absDBPath >>= withFileExsit write (mkdir >> write)
  where mkdir = createDirectory . takeDirectory
        write = flip writeFile (show xs)

eitherJust :: a -> Maybe b -> Either a b
eitherJust _ (Just v) = return v
eitherJust v Nothing  = Left v

lookupDB :: String -> IO (Either String FilePath)
lookupDB slug = eitherJust slug . lookup slug <$> readDB

updateDB :: (String, FilePath) -> IO ()
updateDB pair = (pair:) <$> readDB >>= writeDB

nameOrPath :: String -> IO FilePath
nameOrPath = withDirExsit return (lookupDB >=> (\m ->
  case m of
    Right path -> return path
    Left slug   -> return $ error "Cannot resolve \"" ++ slug ++ "\""))
