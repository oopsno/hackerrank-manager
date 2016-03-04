{-|
Module      : HackerRank.Utilities.Manager
Description : Exposes Manager's main function.
License     : Unlicense
Stability   : experimental
Portability : POSIX

Implementation of __Create__ functionality.

-}

module HackerRank.Utilities.Manager.Create (
  -- * Dependence
  module HackerRank.Utilities.Manager.Challenge,
  module HackerRank.Utilities.Manager.DB,
  module HackerRank.Utilities.Manager.IOError,
  -- * Network
  buildURL,
  getContent,
  getChallenge,
  -- * Maintaining local records
  writeChallenge,
  -- * Action
  createChallenge
) where

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

import HackerRank.Utilities.Manager.Challenge
import HackerRank.Utilities.Manager.DB
import HackerRank.Utilities.Manager.IOError

-- | Shortcut of @Wreq@'s Response type
type ResponseT = Response ByteString

-- | build URL of given Challenge from slug-name
buildURL :: String -- ^ slug-name
         -> String -- ^ RESTful API's URL
buildURL = ("https://www.hackerrank.com/rest/contests/master/challenges" </>)

-- | fetch raw response from HackerRank's RESTful API
getContent :: String         -- ^ URL built by 'buildURL'
           -> IO ByteString  -- ^ Raw response
getContent url = do
  response <- get url
  let sc = response ^. responseStatus . statusCode
      sm = response ^. responseStatus . statusMessage
  if 200 == sc 
     then return $ response ^. responseBody
     else ioError $ userError $ unwords ["Cannot access URL:", url, show sc, show sm]

-- | Get 'Challenge' by slug-name via HackerRank's RESTful API
--   Throws 'IOError' when operation fails.
getChallenge :: String        -- ^ slug-name
             -> IO Challenge  -- ^ parsed 'Challenge'
getChallenge = parseChallenge <=< getContent . buildURL

-- | Write a 'Challenge' to local record
writeChallenge :: Challenge -> IO ()
writeChallenge c = do
  let ((name, root), xs) = renderChallenge c
  wrapper "[BuildCategory]" $ preoperation c
  wrapper "[MakeDirectory]" $ createDirectoryIfMissing True root
  wrapper "[WriteFile]"     $ mapM_ (uncurry writeFile) xs
  wrapper "[UpdateDB]"      $ updateDB (name, (root, c))
  wrapper "[MakeWrapper]"   $ postoperation c
  where wrapper = wrapIOError . (++) "[Manager][Create][writeChallenge]"

-- | create a Challenge from slug-name, get information via RESTful API and write to
--   local records
createChallenge :: String -- ^ slug-name
           -> IO ()
createChallenge slug = do
  exsits <- exsitsInDB slug
  if exsits
    then putStrLn $ slug ++ "already exists in LoaclDB"
    else do
      putStrLn $ "[Manager][DB][createChallenge] Creating challenge: " ++ slug
      getChallenge slug >>= writeChallenge
      putStrLn $ "[Manager][DB][createChallenge] Created challenge: " ++ slug

