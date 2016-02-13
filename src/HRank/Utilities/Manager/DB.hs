module HRank.Utilities.Manager.DB where

import Control.Monad
import System.Directory
import System.FilePath.Posix

absDBPath :: IO FilePath
absDBPath = (</> ".hrmng/db.hs") <$> getHomeDirectory 

readDB :: IO [(String, FilePath)]
readDB = do
  path <- absDBPath
  exists <- doesFileExist path
  if exists then read <$> readFile path
            else return []

writeDB :: [(String, FilePath)] -> IO ()
writeDB xs = do
  path <- absDBPath
  exists <- doesFileExist path
  unless exists $ createDirectory (takeDirectory path)
  writeFile path $ show xs

lookupDB :: String -> IO (Maybe FilePath)
lookupDB name = lookup name <$> readDB

updateDB :: (String, FilePath) -> IO ()
updateDB pair = (pair:) <$> readDB >>= writeDB

nameOrPath :: String -> IO FilePath
nameOrPath xs = do
  exists <- doesDirectoryExist xs
  if exists then return xs
            else do
              x <- lookupDB xs
              case x of
                Just path -> return path
                Nothing   -> return $ error "Cannot resolve \"" ++ xs ++ "\""
