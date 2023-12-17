module Main (main) where

import qualified Data.ByteString.Char8 as B
import HaskGit
import System.Environment (getArgs)
import Util (getGitDirectory)

main :: IO ()
main = do
  let git = ".haskgit"
  argsRaw <- getArgs
  gitDir <- getGitDirectory git
  processArgs argsRaw gitDir

processArgs :: [String] -> FilePath -> IO ()
processArgs [] _ = mapM_ (putStrLn . helpMsg) ["show", "updateRef", "add", "commit", "revList", "log"]
processArgs args gitDir =
  case head args of
    "show" ->
      case tail args of
        [] -> putStrLn "Error: haskgit show requires an argument <object>"
        [object] -> gitShow (B.pack object) gitDir
        _ -> putStrLn "Error: haskgit show only has one argument <object>"
    "add" ->
      case tail args of
        [] -> putStrLn "Error: haskgit add requires an argument <files>"
        files -> gitAdd files gitDir
    "commit" ->
      case tail args of
        [flag, message] ->
          if flag == "-m"
            then gitCommit message gitDir
            else putStrLn "Error: haskgit commit must have -m flag enabled to commit"
        _ -> putStrLn "Error: haskgit commit only has one argument <message>"
    "updateRef" ->
      case tail args of
        [refdest, refsrc] -> gitUpdateRef refdest refsrc gitDir
        _ -> putStrLn "Usage: updateRef refdest refsrc"
    "revList" ->
      case tail args of
        [object] -> gitRevList (B.pack object) gitDir
        _ -> putStrLn "Usage: revList <commit-object>"
    "log" ->
      case tail args of
        [object] -> gitLog (B.pack object) gitDir
        _ -> putStrLn "Usage: log <commit-object>"
    "read-tree" ->
      case tail args of
        [object] -> gitReadTree (B.pack object) gitDir
        _ -> putStrLn "Usage: read-tree <tree-hash>"
    "help" ->
      case tail args of
        [cmd] -> putStrLn $ helpMsg cmd
        _ -> putStrLn "Error: haskgit help only has one argument <command-name>"
    _ -> putStrLn $ "haskgit: " ++ head args ++ " is not a haskgit command"

helpMsg :: String -> String
helpMsg cmd =
  case cmd of
    "show" -> "haskgit show - Show various types of objects"
    "updateRef" -> "haskgit updateRef - Update the object name stored in a ref safely"
    "add" -> "haskgit add - Add file contents to the index"
    "commit" -> "haskgit commit - Record changes to the repository"
    "revList" -> "haskgit revList - list commit objects in reverse chronological order"
    "log" -> "haskgit log - show commit logs"
    _ -> "Error: the command `" ++ cmd ++ "` doesn't exist"
