module Main where

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Lens
import Data.Maybe
import Data.Char
import System.Environment
import System.FilePath.Posix
import System.IO
import System.IO.Error

import qualified System.IO.Error.Lens as IOELens

import qualified HRank.Utilities.Manager.Challenge  as D
import qualified HRank.Utilities.Manager.Create     as C
import qualified HRank.Utilities.Manager.Read       as R
import qualified HRank.Utilities.Manager.Test       as T
import qualified HRank.Utilities.Manager.Execute    as X
import qualified HRank.Utilities.Manager.Edit       as E
import qualified HRank.Utilities.Manager.DB         as DB

type Action = String -> IO ()

data Function = Function { name        :: String
                         , alias       :: String
                         , description :: [String]
                         , action      :: Action }

instance Show Function where
  show x = unwords [ "Function", name x, "as", alias x]

formatDoc :: Function -> String
formatDoc f = unlines $
  unwords [" ", alias f, "-", name f]:map ("    " ++) (description f)

withPath :: Action -> Action
withPath = (DB.nameToPath >=>)

withSolution :: Action -> Action
withSolution = withPath . (. (</> "Solution.hs"))

printChallenge :: FilePath -> D.Challenge -> IO ()
printChallenge p c = do
    putStrLn $ "  Challenge:   " ++ D.name c
    putStrLn $ "  Slug:        " ++ D.slug c
    putStrLn $ "  Location:    " ++ p
    putStrLn $ "  Origin:      " ++ originURL (D.slug c)
    putStrLn $ description c
  where
    pre = "  Description: "
    indent = (++) $ replicate (length pre) ' '
    description c = case lines $ D.asciiDescription c of
      (x:xs) -> unlines $ (pre ++ x) : map indent xs
      []     -> pre ++ "Unavailable"
    originURL = ("https://www.hackerrank.com/challenges/"++)

listChallenges :: Function
listChallenges = Function
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
  where p = uncurry printChallenge  . snd
        psp a b = a >> putStrLn "" >> b

create :: Function
create = Function
  { name        = "create"
  , alias       = "c"
  , description = [ "Create a challenge" ]
  , action      = C.createQuiz }

edit :: Function
edit = Function
  { name        = "edit"
  , alias       = "e"
  , description = [ "Edit a challenge's source code." ]
  , action      = withSolution E.editSource }

editUnitTest :: Function
editUnitTest = Function
  { name        = "editut"
  , alias       = "u"
  , description = [ "Edit a challenge's unittest code." ]
  , action      = withPath E.editUnitTest }

read :: Function
read = Function
  { name        = "read"
  , alias       = "r"
  , description = [ "Print a challenge's source code." ]
  , action      = withSolution (readFile >=> putStrLn) }

copyToPaste :: Function
copyToPaste = Function
  { name        = "copy"
  , alias       = "R"
  , description = [ "Copy a challenge's source code to pasteboard." ]
  , action      = withSolution R.copySource }

execute :: Function
execute = Function
  { name        = "execute"
  , alias       = "x"
  , description = [ "Execute a challenge's main :: IO () action." ]
  , action      = withPath X.executeMain }

test :: Function
test = Function
  { name        = "test"
  , alias       = "t"
  , description = [ "Execute a challenge's unittest." ]
  , action      = withSolution T.runUTest }

functions :: [Function]
functions = [ listChallenges
            , create
            , edit
            , editUnitTest
            , Main.read
            , copyToPaste
            , execute
            , test ]

usage :: Function
usage = Function
  { name        = "usage"
  , alias       = "u" 
  , description = [ "Print usage" ]
  , action      = const $ mapM_ putStrLn $
                    [ "Usage: hrmng operations target"
                    , ""
                    , "Operations:" ] ++ map formatDoc functions }

unknownOperation :: String -> Function
unknownOperation args = Function
  { name        = "UnknownOperation"
  , alias       = "-"
  , description = ["Used to print error messages"]
  , action      = const $ putStrLn $ unwords
      [ "Error: Cannot understand \"", args, "\": unknown operation:"
      , "->" ++ dropWhile (isJust . (`lookup` route) . return) args ] }

route :: [(String, Function)]
route = map ((,) <$> alias <*> id) functions

parseArgs :: [String] -> ([Function], String)
parseArgs [ops, target] = 
  let searchResult = map ((`lookup` route) . return) ops
  in if all isJust searchResult
       then (catMaybes searchResult, target)
       else ([unknownOperation ops, usage], target)
parseArgs ["l"] = ([listChallenges], "")
parseArgs [targets] = ([usage], targets)
parseArgs _ = ([usage], "")

main :: IO ()
main = do
  (argFuncs, argTarget) <- parseArgs <$> getArgs
  let totalSteps = length argFuncs
  forM_ (zip [1..] argFuncs) $ \(i, act) -> do
    when (totalSteps > 1) $
      putStrLn $ unwords
        [ "Step", show i, "of", show totalSteps , "-", name act, argTarget ]
    catchIOError (action act argTarget)
                 (hPutStrLn stderr . (^. IOELens.description))
