{-|
Module      : HackerRank.Utilities.Manager
Description : Exposes Manager's main function.
License     : Unlicense
Stability   : experimental
Portability : POSIX

Entrance, helper functions, functionalities and router for the Manager.

-}

module HackerRank.Utilities.Manager.Manager (
  -- * Modules
  -- | The modules used here
  module HackerRank.Utilities.Manager.Challenge,
  module HackerRank.Utilities.Manager.Create,
  module HackerRank.Utilities.Manager.Read,
  module HackerRank.Utilities.Manager.Test,
  module HackerRank.Utilities.Manager.Execute,
  module HackerRank.Utilities.Manager.Edit,
  module HackerRank.Utilities.Manager.DB,
  -- * Types and Datatypes
  Action,
  Functionality,
  -- * Formatting Functions
  genUsage,
  -- * Utilities
  withPath,
  withSolution,
  -- * Functionalities
  -- | All the functionalities' definitions.
  create,
  edit,
  editUnitTest,
  read,
  copyToPaste,
  execute,
  test,
  usage,
  -- * Router
  functionalities,
  route,
  -- * Main Function
  parseArgs,
  manager
) where

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Lens
import Data.Maybe
import Data.Char
import Prelude hiding ( read )
import System.Environment
import System.FilePath.Posix
import System.IO
import System.IO.Error

import qualified System.IO.Error.Lens as IOELens

-- import for exposing
import qualified HackerRank.Utilities.Manager.Challenge
import qualified HackerRank.Utilities.Manager.Create
import qualified HackerRank.Utilities.Manager.Read
import qualified HackerRank.Utilities.Manager.Test
import qualified HackerRank.Utilities.Manager.Execute
import qualified HackerRank.Utilities.Manager.Edit
import qualified HackerRank.Utilities.Manager.DB

-- aliases modules
import qualified HackerRank.Utilities.Manager.Challenge  as D
import qualified HackerRank.Utilities.Manager.Create     as C
import qualified HackerRank.Utilities.Manager.Read       as R
import qualified HackerRank.Utilities.Manager.Test       as T
import qualified HackerRank.Utilities.Manager.Execute    as X
import qualified HackerRank.Utilities.Manager.Edit       as E
import qualified HackerRank.Utilities.Manager.DB         as DB

-- | The representation of a functionality's implementation of the Manager
--   Take a slug-name of a Challenge (like @minimum-draws@), and do something
--   affects the real world.
type Action = String  -- ^ Slug-name of a Challange
            -> IO ()  -- ^ The Action should take.


-- | The representation of a functionality of the Manager
data Functionality = Functionality {
    name        :: String   -- ^ Fullname of the functionality
  , alias       :: String   -- ^ A shortcut for CLI use
  , description :: [String] -- ^ Detailed description
  , action      :: Action   -- ^ 'Action' implements the functionality
  }

-- | Deriving 'Show' for debugging
instance Show Functionality where
  show x = unwords [ "Functionality", name x, "as", alias x]

-- | formats a 'Functionality' to generate help messages used in 'usage'
genUsage :: Functionality -> String
genUsage f = unlines $
  unwords [" ", alias f, "-", name f]:map ("    " ++) (description f)

-- | Wraps an 'Action' with slug-name got replaced by physical dictionary's path
withPath :: Action -- ^ 'Action' takes slug-name as argument
         -> Action -- ^ 'Action' takes path as argument
withPath = (DB.nameToPath >=>)

-- | Wraps an 'Action' with slug-name got replaced by path to it's @Solution.hs@
withSolution :: Action -- ^ 'Action' takes slug-name as argument
             -> Action -- ^ 'Action' takes path as argument
withSolution = withPath . (. (</> "Solution.hs"))

-- | List all registered challenges
listChallenges :: Functionality
listChallenges = Functionality
  { name        = "list"
  , alias       = "l"
  , description = [ "List all registered challenges" ]
  , action      = \pattern -> 
    (if null pattern then DB.listChallenges else DB.lookupDB pattern)
      >>= \xs -> do
        let xss = length xs
        putStrLn $ unwords
          [ "Listing", show xss, "Challenge"++(if xss == 1 then "" else "s") ]
        when (xss > 0) (foldl1 psp . map p $ xs) }
  where p = uncurry D.printChallenge  . snd
        psp a b = a >> putStrLn "" >> b

-- | Create a challenge
create :: Functionality
create = Functionality
  { name        = "create"
  , alias       = "c"
  , description = [ "Create a challenge" ]
  , action      = C.createChallenge }

-- | Edit a challenge's source code
edit :: Functionality
edit = Functionality
  { name        = "edit"
  , alias       = "e"
  , description = [ "Edit a challenge's source code." ]
  , action      = withSolution E.editSource }

-- | Edit a challenge's unittest code
editUnitTest :: Functionality
editUnitTest = Functionality
  { name        = "editut"
  , alias       = "u"
  , description = [ "Edit a challenge's unittest code." ]
  , action      = withPath E.editUnitTest }

-- | Print a challenge's source code
read :: Functionality
read = Functionality
  { name        = "read"
  , alias       = "r"
  , description = [ "Print a challenge's source code." ]
  , action      = withSolution R.printSource }

-- | Copy a challenge's source code to pasteboard
copyToPaste :: Functionality
copyToPaste = Functionality
  { name        = "copy"
  , alias       = "R"
  , description = [ "Copy a challenge's source code to pasteboard." ]
  , action      = withSolution R.copySource }

-- | Execute a challenge's main :: IO () action
execute :: Functionality
execute = Functionality
  { name        = "execute"
  , alias       = "x"
  , description = [ "Execute a challenge's main :: IO () action." ]
  , action      = withSolution X.executeMain }

-- | Execute a challenge's unittest
test :: Functionality
test = Functionality
  { name        = "test"
  , alias       = "t"
  , description = [ "Execute a challenge's unittest." ]
  , action      = withSolution T.runUTest }

-- | All functionalities __but 'usage'__
--   This list is used to generate 'route'
functionalities :: [Functionality]
functionalities = [ listChallenges
            , create
            , edit
            , editUnitTest
            , read
            , copyToPaste
            , execute
            , test
            , usage ]

-- | Print usage of the Manager
--   This one is not included in 'functionalities' to avoid infinite recursion
usage :: Functionality
usage = Functionality
  { name        = "usage"
  , alias       = "h" 
  , description = [ "Print this usage description" ]
  , action      = const $ mapM_ putStrLn $
                    [ "Usage: hrmng operations target"
                    , ""
                    , "Operations:" ] ++ map genUsage functionalities }


-- | Used to print error messages when argument-parsing failed
unknownOperation :: String -> Functionality
unknownOperation args = Functionality
  { name        = "UnknownOperation"
  , alias       = "-"
  , description = ["Used to print error messages"]
  , action      = const $ putStrLn $ unwords
      [ "Error: Cannot understand \"", args, "\": unknown operation:"
      , "->" ++ dropWhile (isJust . (`lookup` route) . return) args ] }

-- | The functionality router, as an alias-Functionality lookup
route :: [(String, Functionality)]
route = map ((,) <$> alias <*> id) functionalities

-- | Parse given command-line arguments
parseArgs :: [String]                  -- ^ Command-line arguments
          -> ([Functionality], String) -- ^ parsed functionalities sequences and the
                                       --   slug-name
parseArgs [ops, target] = 
  let searchResult = map ((`lookup` route) . return) ops
  in if all isJust searchResult
       then (catMaybes searchResult, target)
       else ([unknownOperation ops, usage], target)
parseArgs ["l"] = ([listChallenges], "")
parseArgs [targets] = ([usage], targets)
parseArgs _ = ([usage], "")

-- | The real main function
manager :: IO ()
manager = do
  (argFuncs, argTarget) <- parseArgs <$> getArgs
  let totalSteps = length argFuncs
  forM_ (zip [1..] argFuncs) $ \(i, act) -> do
    when (totalSteps > 1) $
      putStrLn $ unwords
        [ "Step", show i, "of", show totalSteps , "-", name act, argTarget ]
    catchIOError (action act argTarget)
                 (hPutStrLn stderr . (^. IOELens.description))
