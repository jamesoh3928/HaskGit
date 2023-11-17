{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import qualified Data.ByteString.Char8 as B
import Experiment
import HaskGit
import System.Console.CmdArgs

data HaskGit
  = Show {hash :: String}
  | WriteTree {filename :: FilePath}
  | HashObject {filename :: FilePath}
  | UpdateRef {refdest :: String, refsrc :: String}
  deriving (Data, Typeable, Show, Eq)

-- NOTE: Functionality is not implemented yet
writeTreeMode :: HaskGit
writeTreeMode =
  WriteTree {filename = def &= typFile &= argPos 0}
    &= help "haskgit writeTree <filename>\nCreate a tree object from the current index"

showMode :: HaskGit
showMode =
  Show {hash = def &= argPos 0}
    &= help "haskgit show <object>\nShows various types of git objects"

hashObjectMode :: HaskGit
hashObjectMode =
  HashObject {filename = def &= typFile &= argPos 0}
    &= help "haskgit hashobject <filename>\nCompute object ID"

updateRefMode :: HaskGit
updateRefMode =
  UpdateRef
    { refdest = def &= argPos 0,
      refsrc = def &= argPos 1
    }
    &= auto
    &= help "haskgit updateref <ref_dest> <ref_src> \n Update the ref."

cmdModes :: HaskGit
cmdModes = modes [showMode, hashObjectMode, updateRefMode]

runCommand :: HaskGit -> IO ()
runCommand cmd = case cmd of
  Show hash -> gitShow (B.pack hash)
  HashObject filename -> testHash filename
  UpdateRef refdest refsrc -> gitUpdateRef refdest refsrc

main :: IO ()
main = do
  cmd <- cmdArgs cmdModes
  runCommand cmd
