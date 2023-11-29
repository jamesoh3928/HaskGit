-- {-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import qualified Data.ByteString.Char8 as B
import HaskGit
-- import System.Console.CmdArgs
import System.Environment (getArgs)

main :: IO ()
main = do
  argsRaw <- getArgs
  processArgs argsRaw

processArgs :: [String] -> IO ()  
processArgs args =
  case head args of
    "Show" ->
      case tail args of
        [] -> putStrLn "Error: haskgit Show requires an argument <object>"
        [object] -> gitShow (B.pack object)
        _ -> putStrLn "Error: haskgit Show only has one argument <object>"
    "UpdateRef" ->
      case tail args of
        [refdest, refsrc] -> gitUpdateRef refdest refsrc
        _ -> putStrLn "Usage: UpdateRef refdest refsrc"
    _ -> putStrLn $ "haskgit: " ++ (head args) ++ " is not a haskgit command"

-- data HaskGit
--   = Show {hash :: String}
--   | UpdateRef {refdest :: String, refsrc :: String}
--   deriving (Data, Typeable, Show, Eq)

-- showMode :: HaskGit
-- showMode =
--   Show {hash = def &= argPos 0}
--     &= help "haskgit show <object>\nShows various types of git objects"

-- updateRefMod :: HaskGit
-- updateRefMod =
--   UpdateRef {refdest = def &= argPos 0, refsrc = def &= argPos 1}
--     &= help "haskgit updateref <ref> <obj>\nUpdate reference to new object"

-- cmdModes :: HaskGit
-- cmdModes = modes [showMode, updateRefMod]

-- runCommand :: HaskGit -> IO ()
-- runCommand cmd = case cmd of
--   Show hash -> gitShow (B.pack hash)
--   UpdateRef refdest refsrc -> gitUpdateRef refdest refsrc

-- main :: IO ()
-- main = do
--   cmd <- cmdArgs cmdModes
--   runCommand cmd
