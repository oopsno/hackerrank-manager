module HRank.Utilities.Manager.Create where

import Control.Exception
import Control.Lens ((^.))
import Control.Monad
import qualified Control.Monad as M
import Data.ByteString.Lazy ( ByteString )
import Data.Either
import Data.List
import Data.Maybe
import System.Directory
import System.Process
import Network.Wreq
import System.FilePath.Posix
import System.IO.Error

import HRank.Utilities.Manager.Challenge
import HRank.Utilities.Manager.DB
import HRank.Utilities.Manager.IOError

type ResponseT = Response ByteString

buildURL :: String -> String
buildURL = ("https://www.hackerrank.com/rest/contests/master/challenges" </>)

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

writeChallenge :: Challenge -> IO ()
writeChallenge c = do
  let (db@(_, root), xs) = renderChallenge c
  wrapper "[BuildCategory]" $ preoperation c
  wrapper "[MakeDirectory]" $ createDirectoryIfMissing True root
  wrapper "[WriteFile]"     $ mapM_ (uncurry writeFile) xs
  wrapper "[UpdateDB]"      $ updateDB db
  wrapper "[MakeWrapper]"   $ postoperation c
  where wrapper = wrapIOError . (++) "[Manager][Create][writeChallenge]"

createQuiz :: String -> IO ()
createQuiz slug = do
  exsits <- exsitsInDB slug
  if exsits
    then putStrLn $ slug ++ "already exists in LoaclDB"
    else do
      putStrLn $ "[Manager][DB][createQuiz] Creating challenge: " ++ slug
      getChallenge slug >>= writeChallenge
      putStrLn $ "[Manager][DB][createQuiz] Created challenge: " ++ slug

