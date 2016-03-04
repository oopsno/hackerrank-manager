module HackerRank.Utilities.Manager.DB where

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

import HackerRank.Utilities.Manager.IOError
import HackerRank.Utilities.Manager.Challenge

absDBPath :: IO FilePath
absDBPath = (</> ".hrmng/db.hs") <$> getHomeDirectory 

readDB :: IO [(String, (FilePath, Challenge))]
readDB = withDefault "[Manager][DB][readDB]"
                     []
                     (absDBPath >>= (SIO.readFile >=> return . read))

writeDB :: [(String, (FilePath, Challenge))] -> IO ()
writeDB xs = wrapIOError "[Manager][DB][writeDB]"
                         (absDBPath >>= withFileExsit write (mkdir >> write))
  where mkdir = createDirectory . takeDirectory
        write = flip writeFile (ppShow xs)

lookupDB :: String -> IO [(String, (FilePath, Challenge))]
lookupDB slug = fuzzyLookup slug <$!> readDB
  where fuzzyLookup p xs = map F.original $ F.filter p xs "<" ">" fst False

exsitsInDB :: String -> IO Bool
exsitsInDB slug = wrapIOError "[Manager][DB][exsitsInDB]"
                              (isJust . lookup slug <$!> readDB)

updateDB :: (String, (FilePath, Challenge)) -> IO ()
updateDB pair = wrapIOError "[Manager][DB][updateDB]"
                            ((pair:) <$!> readDB >>= writeDB) 

nameToPath :: String -> IO FilePath
nameToPath slug = withDirExsit return (lookupDB >=> (\xs ->
  case length xs of
    0 -> ioError nrErr
    1 -> return . fst . snd . head $ xs
    _ -> mulErr xs >>= ioError)) slug
  where nrErr = userError $
          "[Manager][DB][nameOrPath]: Cannot resolve \"" ++ slug ++ "\""
        mulErr xs = do
          matches <- mapM fmt xs
          return . userError $
            unlines (("[Manager][DB][nameOrPath]: Multiple matches of \"" ++
                        slug ++ "\" found:"):matches)
        fmt (s, (p, c)) = do
          rel <- makeRelativeToCurrentDirectory p
          return $ unlines ["\tName: " ++ s, "\tPath: " ++ rel]

listChallenges :: IO [(String, (FilePath, Challenge))] 
listChallenges = readDB >>= mapM (\(n, (p, c)) -> do
  rel <- makeRelativeToCurrentDirectory p
  return (n, (rel, c)))
