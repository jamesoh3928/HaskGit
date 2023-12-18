module Main (main) where

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
        [] -> gitStatusModifiedHash gitDir
    "branch" ->
      case tail args of
        [] -> gitListBranch gitDir
        [branchName] -> gitCreateBranch branchName gitDir
        _ -> putStrLn "Error: haskgit branch only has one argument <branch-name>"
    "checkout" ->
      case tail args of
        [object] -> gitCheckout object gitDir
        _ -> putStrLn "Error: haskgit checkout needs one argument <branch-name> or <commit-hash>"
        [] -> gitStatusModifiedHash gitDir
    "index" -> -- ^ used for test
      case tail args of
        [path] -> getIndexEntry gitDir path
    "hash2commit" ->
      case tail args of
        [hash] -> do 
            case bsToHash (B.pack hash) of
              Nothing -> error "invalid hash"
              Just bs -> do
                  obj <- hash2CommitObj bs gitDir
                  case obj of
                    Nothing -> error "fail on hash2CommitObj"
                    Just obj -> print obj
    "hash2tree" ->
      case tail args of
        [hs] -> hash2Tree hs gitDir
    "test" ->
      case tail args of
        [] -> gitFlatTree gitDir
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
    "branch" -> "haskgit branch - list, create, or delete branches"
    _ -> "Error: the command `" ++ cmd ++ "` doesn't exist"
