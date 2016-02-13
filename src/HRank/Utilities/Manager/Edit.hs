module HRank.Utilities.Manager.Edit where

import Control.Exception
import System.Environment
import System.Process
import System.FilePath.Posix

import HRank.Utilities.Manager.IOError

getEditor :: IO String
getEditor = catch (getEnv "EDITOR") ((\e -> return "vim") :: IOException -> IO String)

editSource :: FilePath -> IO ()
editSource path = do
  editor <- getEditor
  let message = "[Manager][Edit][editSource]"
  catchIOError_ message (callProcess editor [path])

editSolution :: FilePath -> IO ()
editSolution = editSource . (</> "Solution.hs")

editUnitTest :: FilePath -> IO ()
editUnitTest = editSource . (</> "UnitTest.hs")
