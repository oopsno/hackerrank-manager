module HRank.Utilities.Manager.Challenge where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import System.FilePath.Posix
import Text.Pandoc

import HRank.Utilities.Manager.Haskell

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

modulePath :: ModuleType -> Challenge -> FilePath
modulePath t c = rootDir c </> show t <.> "hs"

wrapperPath :: Challenge -> FilePath
wrapperPath c = rootDir c <.> "hs"

wrapperName :: Challenge -> String
wrapperName c = intercalate "." $ "HRank" : titlizeTrack c

moduleName :: ModuleType -> Challenge -> String
moduleName t c = wrapperName c <.> titlizeChallenge c

wrapperDecl :: Challenge -> String
wrapperDecl c = unwords [ "module", wrapperName c, "where" ]

moduleDecl :: ModuleType -> Challenge -> String
moduleDecl t c = unwords [ "module", moduleName t c, "where" ]

moduleImpl :: ModuleType -> Challenge -> String
moduleImpl t c = unwords [ "import", "qualified", moduleName t c ]

moduleImplQ :: ModuleType -> Challenge -> String -> String
moduleImplQ t c alias = unwords [ moduleImpl t c, "as", alias ]

renderDescription :: Challenge -> String
renderDescription c = case readHtml def . fromMaybe "" . description $ c of
  Right doc -> writeHaddock def doc
  Left  err -> "-- No printable description"

solutionRender :: Challenge -> String
solutionRender c = unlines 
  [ "{-| "
  , "Module: " ++ moduleName Solution c
  , "Description : Solution for Challenge [" ++ breadcrumb c ++ "]"
  , "License     : CC BY-NC-SA 3.0"
  , "Stability   : experimental"
  , "Portability : POSIX"
  , ""
  , renderDescription c
  , "-}"
  , ""
  , moduleDecl Solution c
  , ""
  , "import Control.Monad"
  , "import Control.Applicative"
  , ""
  , "main :: IO ()"
  , "main = putStrLn \"No available implementation\"" ]

unittestRender :: Challenge -> String
unittestRender c = unlines
  [ "{-| "
  , "Module: " ++ moduleName UnitTest c
  , "Description : UnitTest for Challenge [" ++ breadcrumb c ++ "]"
  , "License     : CC BY-NC-SA 3.0"
  , "Stability   : experimental"
  , "Portability : POSIX"
  , ""
  , "Provides offical sample in(out)put, and basic unit test framework of challenge" ++ name c
  , "-}"
  , ""
  , moduleDecl UnitTest c
  , ""
  , "import Test.Hspec"
  , "import Test.QuickCheck"
  , ""
  , "sampleInput :: String"
  , "sampleInput = " ++ show (fromMaybe "" (sampleInput c))
  , ""
  , "sampleOutput :: String"
  , "sampleOutput = " ++ show (fromMaybe "" (sampleOutput c))
  , ""
  , "main :: IO ()"
  , "main = putStrLn \"No available test cases\"" ]

wrapperRender :: Challenge -> String
wrapperRender c = unlines 
  [ "{-|"
  , "Module: " ++ wrapperName c 
  , "Description : Wrapper for Challenge [" ++ breadcrumb c ++ "]"
  , "License     : CC BY-NC-SA 3.0"
  , "Stability   : experimental"
  , "Portability : POSIX"
  , ""
  , "Just inports Solution and its UnitTest"
  , "-}"
  , ""
  , wrapperDecl c
  , ""
  , moduleImplQ Solution c "S"
  , moduleImplQ UnitTest c "U"
  , ""
  , "main :: IO ()"
  , "main = S.main" ]

codeRender :: ModuleType -> Challenge -> String
codeRender UnitTest = unittestRender 
codeRender Solution = solutionRender

render :: ModuleType -> Challenge -> (FilePath, String)
render = curry $ (,) <$> uncurry modulePath <*> uncurry codeRender

preoperation :: Challenge -> IO ()
preoperation c = do
  let xs = "HRank" : titlizeTrack c ++ [titlizeChallenge c]
  forM_ [1..length xs -1] $ \n -> do
    let categoryCrumb = take n xs
        moduleCrumb   = take (n + 1) xs
        categoryName  = foldl1 (<.>) categoryCrumb
        categoryPath  = "src" </> foldl1 (</>) categoryCrumb <.> "hs"
        importModule  = foldl1 (<.>) moduleCrumb
    makeCategory categoryName [importModule] categoryPath

postoperation :: Challenge -> IO ()
postoperation c = writeFile (wrapperPath c) (wrapperRender c)
       
renderChallenge :: Challenge -> ((String, FilePath), [(FilePath, String)])
renderChallenge c = ((slug c, rootDir c), map (`render` c) [Solution ..])

