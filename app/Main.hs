module Main (main) where

import Control.Monad (void)
import qualified Data.ByteString.Char8 as B
import GitHash
import HaskGit
import System.Environment (getArgs)
import Util (getGitDirectory)

main :: IO ()
main = do
  let gitDir = ".haskgit"
  argsRaw <- getArgs
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
        [] -> gitLog Nothing gitDir
        [object] -> gitLog (Just $ B.pack object) gitDir
        _ -> putStrLn "Usage: log <commit-object>"
    "read-tree" ->
      case tail args of
        [object] -> gitReadTree (B.pack object) gitDir
        _ -> putStrLn "Usage: read-tree <tree-hash>"
    "hash-object" ->
      case tail args of
        [object] -> gitHashObject object
        _ -> putStrLn "Usage: hash-object <file>"
    "status" ->
      case tail args of
        [] -> gitStatus gitDir
        _ -> putStrLn "Usage: status"
    "branch" ->
      case tail args of
        [] -> gitListBranch gitDir
        [branchName] -> gitCreateBranch branchName gitDir
        _ -> putStrLn "Error: haskgit branch only has one argument <branch-name>"
    "checkout" ->
      case tail args of
        [object] -> gitCheckout object gitDir
        [flag, object] ->
          -- Create a new branch and checkout
          if flag == "-b"
            then do
              gitCreateBranch object gitDir
              gitCheckout object gitDir
            else putStrLn "Error: haskgit checkout needs one argument. haskgit checkout [-b] <branch-name> or <commit-hash>"
        _ -> putStrLn "Error: haskgit checkout needs one argument. haskgit checkout [-b] <branch-name> or <commit-hash>"
    "reset" ->
      case tail args of
        [] -> gitReset gitDir
        [object] -> gitResetMixed object gitDir
        [flag, object] ->
          case flag of
            "--soft" -> void $ gitResetSoft object gitDir
            "--mixed" -> gitResetMixed object gitDir
            "--hard" -> gitResetHard object gitDir
            _ -> putStrLn "Error: invalid format. haskgit reset [--soft | --mixed | --hard] [commit_hash] "
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
    "status" -> "haskgit status - show the working tree status"
    "branch" -> "haskgit branch - list, create, or delete branches"
    _ -> "Error: the command `" ++ cmd ++ "` doesn't exist"
