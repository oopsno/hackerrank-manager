{-|
Module      : HRank.Utilities.Manager.Challenge
Description : Fetch, parse and render Challenges
License     : Unlicense
Stability   : experimental
Portability : POSIX
|-}

module HRank.Utilities.Manager.Challenge where

import Control.Applicative
import Control.Exception
import Control.Lens ((^.), (^?))
import Control.Monad
import Data.Aeson.Lens
import qualified Data.Aeson.Types as AT
import Data.ByteString.Lazy ( ByteString )
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Text ( pack, unpack ) 
import qualified Data.Text as TX
import System.FilePath.Posix
import System.Directory
import Text.HandsomeSoup
import Text.Pandoc
import Text.Regex.PCRE
import Text.XML.HXT.Core

import HRank.Utilities.Manager.Haskell
import HRank.Utilities.Manager.IOError

data Challenge = Challenge { name         :: String
                           , slug         :: String
                           , track        :: [String]
                           , description  :: Maybe String
                           , sampleInput  :: Maybe String
                           , sampleOutput :: Maybe String
                           , bodyHtml     :: String } deriving (Eq, Show, Read)

data ModuleType = Solution | UnitTest deriving (Enum, Eq, Show)

parseChallenge :: ByteString -> IO Challenge
parseChallenge content = do
    let html = fetchField content "body_html"
        slug = fetchField content "slug"
        name = fetchField content "name"
        track = [ fetchField2 content ["track", "track_slug"], fetchField2 content ["track", "slug"] ]
    let doc = parseHtml html
    quizzes <- runX (doc >>> css "p" //> hasText (not . null) >>> getText)
    let sim = html =~ siRE :: [[String]]
        som = html =~ soRE :: [[String]]
        quiz = maybeField quizzes head
        si   = maybeField sim     (last . head) 
        so   = maybeField som     (last . head)
    return $ Challenge name slug track quiz si so html
  where
    buildRE t = concat ["<strong>Sample ", t, "</strong></p>\\s+<pre><code>([^<]*)</code>"]
    siRE = buildRE "Input"
    soRE = buildRE "Output"
    maybeField s op = if null s then Nothing else Just $ op s

fetchField :: ByteString -> String -> String
fetchField content x =
  case content ^? key (pack "model") . key (pack x) of
    Just (AT.String field) -> unpack field
    _ -> throw $ PatternMatchFail $ "Cannot fetch field \"model." ++ x ++  "\""

fetchField2 :: ByteString -> [String] -> String
fetchField2 content xs@[x, y] =
  case content ^? key (pack "model") . key (pack x) . key (pack y) of
    Just (AT.String field) -> unpack field
    _ -> throw $ PatternMatchFail $ "Cannot fetch field \"model." ++ intercalate "." xs ++  "\""

titlize :: String -> String
titlize "fp"   = "FP"
titlize "sql"  = "SQL"
titlize (x:xs) = toUpper x : xs

titlizeSlug :: String -> String
titlizeSlug = concatMap titlize . splitOn "-" 

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
wrapperName c = "HRank" <.> intercalate "." (titlizeTrack c) <.>titlizeChallenge c

moduleName :: ModuleType -> Challenge -> String
moduleName t c = wrapperName c <.> show t

wrapperDecl :: Challenge -> String
wrapperDecl c = unwords [ "module", wrapperName c, "where" ]

moduleDecl :: ModuleType -> Challenge -> String
moduleDecl t c = unwords [ "module", moduleName t c, "where" ]

moduleImpl :: ModuleType -> Challenge -> String
moduleImpl t c = unwords [ "import", "qualified", moduleName t c ]

moduleImplQ :: ModuleType -> Challenge -> String -> String
moduleImplQ t c alias = unwords [ moduleImpl t c, "as", alias ]

reformateDescription :: (WriterOptions -> Pandoc -> String) -> Challenge -> String
reformateDescription writter c = case readHtml def . bodyHtml $ c of
  Right doc -> writter def doc
  Left  err -> "-- No printable description"

renderDescription :: Challenge -> String
renderDescription = reformateDescription writeHaddock

asciiDescription :: Challenge -> String
asciiDescription = reformateDescription writeAsciiDoc

solutionRender :: Challenge -> String
solutionRender c = unlines 
  [ "{-| "
  , "Module      : " ++ moduleName Solution c
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
  , "Module      : " ++ moduleName UnitTest c
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
  , "Module      : " ++ wrapperName c 
  , "Description : Wrapper for Challenge [" ++ breadcrumb c ++ "]"
  , "License     : CC BY-NC-SA 3.0"
  , "Stability   : experimental"
  , "Portability : POSIX"
  , ""
  , "Just imports Solution and its UnitTest"
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

renderChallenge :: Challenge -> ((String, FilePath), [(FilePath, String)])
renderChallenge c = ((slug c, rootDir c), map (`render` c) [Solution ..])

makeCategory :: String -> [String] -> FilePath -> IO ()
makeCategory name imports path = do
  putStrLn $ "makeCategory: " ++ path
  createDirectoryIfMissing True (takeDirectory path)
  withFileExsit
    (\path -> (renderCategory name . nub . (++) imports) <$!> getImports path
                >>= writeFile path)
    (\path -> writeFile path $ renderCategory name imports)
    path

preoperation :: Challenge -> IO ()
preoperation c = do
  let xs = "HRank" : titlizeTrack c ++ [titlizeChallenge c]
  forM_ [1..length xs - 1] $ \n -> do
    let categoryCrumb = take n xs
        moduleCrumb   = take (n + 1) xs
        categoryName  = foldl1 (<.>) categoryCrumb
        categoryPath  = "src" </> foldl1 (</>) categoryCrumb <.> "hs"
        importModule  = foldl1 (<.>) moduleCrumb
    print [ categoryName, categoryPath ]
    makeCategory categoryName [importModule] categoryPath

postoperation :: Challenge -> IO ()
postoperation c = writeFile (wrapperPath c) (wrapperRender c)
       
