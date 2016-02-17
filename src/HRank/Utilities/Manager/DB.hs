module HRank.Utilities.Manager.DB where

import Control.Monad
import Data.Either
import Data.Maybe
import System.Directory
import System.FilePath.Posix
import System.IO
import System.IO.Error
import qualified System.IO.Strict as SIO
import qualified Text.Fuzzy as F
import Text.Show.Pretty ( ppShow )

import HRank.Utilities.Manager.IOError

absDBPath :: IO FilePath
absDBPath = (</> ".hrmng/db.hs") <$> getHomeDirectory 

readDB :: IO [(String, FilePath)]
readDB = withDefault "[Manager][DB][readDB]" [] (absDBPath >>= (SIO.readFile >=> return . read))

writeDB :: [(String, FilePath)] -> IO ()
writeDB xs = wrapIOError "[Manager][DB][writeDB]" (absDBPath >>= withFileExsit write (mkdir >> write))
  where mkdir = createDirectory . takeDirectory
        write = flip writeFile (ppShow xs)

lookupDB :: String -> IO [(String, FilePath)]
lookupDB slug = fuzzyLookup slug <$!> readDB
  where fuzzyLookup p xs = map F.original $ F.filter p xs "<" ">" fst False

exsitsInDB :: String -> IO Bool
exsitsInDB slug = wrapIOError "[Manager][DB][exsitsInDB]" (isJust . lookup slug <$!> readDB)

updateDB :: (String, FilePath) -> IO ()
updateDB pair = wrapIOError "[Manager][DB][updateDB]" ((pair:) <$!> readDB >>= writeDB) 

nameToPath :: String -> IO FilePath
nameToPath slug = withDirExsit return (lookupDB >=> (\xs ->
  case length xs of
    0 -> ioError nrErr
    1 -> return . snd . head $ xs
    _ -> mulErr xs >>= ioError)) slug
  where nrErr = userError $ "[Manager][DB][nameOrPath]: Cannot resolve \"" ++ slug ++ "\""
        mulErr xs = do
          matches <- mapM fmt xs
          return . userError $ unlines (("[Manager][DB][nameOrPath]: Multiple matches of \"" ++ slug ++ "\" found:"):matches)
        fmt (s, p) = do
          rel <- makeRelativeToCurrentDirectory p
          return $ unlines ["\tName: " ++ s, "\tPath: " ++ rel]


