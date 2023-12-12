module Main (main) where

import qualified Data.ByteString.Char8 as B
import HaskGit
-- import System.Console.CmdArgs
import System.Environment (getArgs)
import Util (getGitDirectory)

main :: IO ()
main = do
  argsRaw <- getArgs
  gitDir <- getGitDirectory
  processArgs argsRaw gitDir

processArgs :: [String] -> FilePath -> IO ()
processArgs [] _ =
  do
  putStrLn $ helpMsg "Show"
  putStrLn $ helpMsg "UpdateRef"
processArgs args gitDir =
  case head args of
    "Show" ->
      case tail args of
        [] -> putStrLn "Error: haskgit Show requires an argument <object>"
        [object] -> gitShow (B.pack object) gitDir
        _ -> putStrLn "Error: haskgit Show only has one argument <object>"
    "UpdateRef" ->
      case tail args of
        [refdest, refsrc] -> gitUpdateRef refdest refsrc gitDir
        _ -> putStrLn "Usage: UpdateRef refdest refsrc"
    "help" ->
      case tail args of
        [cmd] -> putStrLn $ helpMsg cmd
        _ -> putStrLn "Error: haskgit help only has one argument <command-name>"
    _ -> putStrLn $ "haskgit: " ++ head args ++ " is not a haskgit command"

helpMsg :: String -> String
helpMsg cmd =
  case cmd of
    "Show" -> "haskgit Show - Show various types of objects"
    "UpdateRef" -> "haskgit UpdateRef - Update the object name stored in a ref safely"
    _ -> "Error: the command `" ++ cmd ++ "` doesn't exist"
