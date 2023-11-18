{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import qualified Data.ByteString.Char8 as B
import HaskGit
import System.Console.CmdArgs

data HaskGit
  = Show {hash :: String}
  | UpdateRef {refdest :: String, refsrc :: String}
  deriving (Data, Typeable, Show, Eq)

showMode :: HaskGit
showMode =
  Show {hash = def &= argPos 0}
    &= help "haskgit show <object>\nShows various types of git objects"

updateRefMod :: HaskGit
updateRefMod =
  UpdateRef {refdest = def &= argPos 0, refsrc = def &= argPos 1}
    &= help "haskgit updateref <ref> <obj>\nUpdate reference to new object"

cmdModes :: HaskGit
cmdModes = modes [showMode, updateRefMod]

runCommand :: HaskGit -> IO ()
runCommand cmd = case cmd of
  Show hash -> gitShow (B.pack hash)
  UpdateRef refdest refsrc -> gitUpdateRef refdest refsrc

main :: IO ()
main = do
  cmd <- cmdArgs cmdModes
  runCommand cmd
