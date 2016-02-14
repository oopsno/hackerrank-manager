module HRank.Utilities.Manager.Challenge where

import Control.Applicative
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import System.FilePath.Posix

data Challenge = Challenge { name         :: String
                           , slug         :: String
                           , track        :: [String]
                           , description  :: Maybe String
                           , sampleInput  :: Maybe String
                           , sampleOutput :: Maybe String } deriving (Eq, Show)

data ModuleType = Solution | UnitTest deriving (Enum, Eq, Show)

titlizeSlug :: String -> String
titlizeSlug = concatMap (\(x:xs) -> toUpper x:xs) . splitOn "-" 

titlizeChallenge :: Challenge -> String
titlizeChallenge = titlizeSlug . slug

titlizeTrack :: Challenge -> [String]
titlizeTrack = map titlizeSlug . track

breadcrumb :: Challenge -> String
breadcrumb c = intercalate " / " $ titlizeTrack c ++ [name c]

rootDir :: Challenge -> FilePath
rootDir c = foldl1 (</>) ("src/HRank" : titlizeTrack c ++ [titlizeChallenge c])

filePath :: ModuleType -> Challenge -> FilePath
filePath t c = foldl1 (</>) ("src/HRank" : titlizeTrack c ++ [titlizeChallenge c, show t]) <.> "hs"

moduleName :: ModuleType -> Challenge -> String
moduleName t c = intercalate "." $ "HRank" : titlizeTrack c ++ [titlizeChallenge c, show t]

moduleDecl :: ModuleType -> Challenge -> String
moduleDecl t c = unwords [ "module", moduleName t c, "where" ]

moduleImpl :: ModuleType -> Challenge -> String
moduleImpl t c = unwords [ "import", "qualified", moduleName t c ]

moduleImplQ :: ModuleType -> Challenge -> String -> String
moduleImplQ t c alias = unwords [ moduleImpl t c, "as", alias ]

solutionRender :: Challenge -> String
solutionRender c = unlines 
  [ moduleDecl Solution c
  , "import Control.Monad"
  , "import Control.Applicative"
  , ""
  , "-- Solution for Challenge: " ++ breadcrumb c
  , ""
  , "main :: IO ()"
  , "main = putStrLn \"No available implementation\"" ]

unittestRender :: Challenge -> String
unittestRender c = unlines
  [ moduleDecl UnitTest c
  , ""
  , "import Test.Hspec"
  , "import Test.QuickCheck"
  , ""
  , "-- UnitTest for Challenge: " ++ breadcrumb c
  , ""
  , moduleImplQ Solution c "S"
  , ""
  , "sampleInput :: String"
  , "sampleInput = " ++ show (fromMaybe "" (sampleInput c))
  , ""
  , "sampleOutput :: String"
  , "sampleOutput = " ++ show (fromMaybe "" (sampleOutput c))
  , ""
  , "main :: IO ()"
  , "main = putStrLn \"No available test cases\"" ]

codeRender :: ModuleType -> Challenge -> String
codeRender UnitTest = unittestRender 
codeRender Solution = solutionRender

render :: ModuleType -> Challenge -> (FilePath, String)
render = curry $ (,) <$> uncurry filePath <*> uncurry codeRender

renderChallenge :: Challenge -> ((String, FilePath), [(FilePath, String)])
renderChallenge c = ((slug c, rootDir c), map (`render` c) [Solution ..])
