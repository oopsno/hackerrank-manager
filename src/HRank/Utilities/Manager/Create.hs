module HRank.Utilities.Manager.Create where

import Control.Lens
import Control.Exception
import Control.Monad
import qualified Control.Monad as M
import Data.Aeson.Lens
import qualified Data.Aeson.Types as AT
import Data.Text ( pack, unpack ) 
import qualified Data.Text as TX
import Data.ByteString.Lazy ( ByteString )
import Data.Either
import Data.List
import Data.Maybe
import System.Process
import Network.Wreq
import Text.HandsomeSoup
import Text.Regex.PCRE
import Text.XML.HXT.Core
import System.FilePath.Posix
import System.IO.Error

import HRank.Utilities.Manager.Challenge
import HRank.Utilities.Manager.DB
import HRank.Utilities.Manager.IOError

type ResponseT = Response ByteString

buildURL :: String -> String
buildURL = ("https://www.hackerrank.com/rest/contests/master/challenges" </>)

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
    return $ Challenge name slug track quiz si so
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

getContent :: String -> IO ByteString
getContent url = do
  response <- get url
  let sc = response ^. responseStatus . statusCode
      sm = response ^. responseStatus . statusMessage
  if 200 == sc 
     then return $ response ^. responseBody
     else ioError $ userError $ unwords ["Cannot access URL:", url, show sc, show sm]

getChallenge :: String -> IO Challenge
getChallenge = parseChallenge <=< getContent . buildURL

writeChallenge :: ((String, FilePath), [(FilePath, String)]) -> IO ()
writeChallenge (db@(_, root), xs) = do
  wrapper "[updateDB]"  $ updateDB db
  wrapper "[mkdir]"     $ createProcess (shell ("mkdir -p " ++ root))
  wrapper "[writeFile]" $ mapM_ (uncurry writeFile) xs
  where wrapper = wrapIOError . (++) "[Manager][Create][writeChallenge]"

createQuiz :: String -> IO ()
createQuiz slug = do
  exsits <- exsitsInDB slug
  if exsits
    then putStrLn $ slug ++ "already exists in LoaclDB"
    else do
      putStrLn $ "[Manager][DB][createQuiz]Creating challenge: " ++ slug
      c <- getChallenge slug
      preoperation c
      writeChallenge $ renderChallenge c
      postoperation c
      putStrLn $ "[Manager][DB][createQuiz]Created challenge: " ++ slug

