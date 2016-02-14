module HRank.Utilities.Manager.DB where

import Control.Monad
import Data.Either
import Data.Maybe
import System.Directory
import System.FilePath.Posix
import System.IO
import System.IO.Error
import qualified System.IO.Strict as SIO
import Text.Show.Pretty ( ppShow )

import HRank.Utilities.Manager.IOError

absDBPath :: IO FilePath
absDBPath = (</> ".hrmng/db.hs") <$> getHomeDirectory 

readDB :: IO [(String, FilePath)]
readDB = withDefault [] absDBPath >>= (SIO.readFile >=> return . read)

writeDB :: [(String, FilePath)] -> IO ()
writeDB xs = wrapIOError "[Manager][DB][writeDB]" (absDBPath >>= withFileExsit write (mkdir >> write))
  where mkdir = createDirectory . takeDirectory
        write = flip writeFile (ppShow xs)

eitherJust :: a -> Maybe b -> Either a b
eitherJust _ (Just v) = return v
eitherJust v Nothing  = Left v

lookupDB :: String -> IO (Either String FilePath)
lookupDB slug = eitherJust slug . lookup slug <$> readDB

updateDB :: (String, FilePath) -> IO ()
updateDB pair = (pair:) <$> readDB >>= writeDB >> putStrLn ("[Manager][DB][updateDB]: added " ++ fst pair)

nameOrPath :: String -> IO FilePath
nameOrPath = withDirExsit return (lookupDB >=> either (ioError . mkErr) return)
  where mkErr slug = userError $ "[Manager][DB][nameOrPath]: Cannot resolve \"" ++ slug ++ "\""

