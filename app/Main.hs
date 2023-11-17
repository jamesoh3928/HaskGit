{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import HaskGit

import System.Console.CmdArgs
import qualified Data.ByteString.Char8 as B

data HaskGit
  = Show {hash :: String}
  | WriteTree {filename :: FilePath}
  | HashObject {filename :: FilePath}
  deriving (Data, Typeable, Show, Eq)

-- NOTE: Functionality is not implemented yet
writeTreeMode :: HaskGit
writeTreeMode = WriteTree {filename = def &= typFile &= argPos 0} &=
  help "haskgit writeTree <filename>\nCreate a tree object from the current index"

showMode :: HaskGit
showMode = Show {hash = def &= argPos 0}&=
  help "haskgit show <object>\nShows various types of git objects" 

cmdModes :: HaskGit
cmdModes = modes [showMode, writeTreeMode]

runCommand :: HaskGit -> IO ()
runCommand cmd = case cmd of
  Show hash -> gitShow (B.pack hash)

main :: IO ()
main = do
  cmd <- cmdArgs cmdModes
  runCommand cmd
