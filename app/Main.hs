{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import HaskGit (testHash)
import System.Console.CmdArgs
import Data.ByteString (ByteString)
-- import Lib

-- you can run it by `stack exec -- -f <filename`
-- FIXME: use command instead of option
data HaskGit =
    TestHash {filename :: FilePath}
    | Help
    deriving (Data, Typeable, Show, Eq)

testHashMode = TestHash {filename = def &= typFile &= help "TestHash :: String -> IO()"}
cmdModes = modes [testHashMode]

runCommand :: HaskGit -> IO ()
runCommand cmd = case cmd of
  TestHash filename -> testHash filename
import qualified Data.ByteString.Char8 as B
import GitObject
import HaskGit
import Index
import System.Console.CmdArgs

-- import Lib

data HaskGit
  = TestHash {filename :: FilePath}
  | WriteTree {filename :: FilePath}
  | Show {hash :: String}
  deriving (Data, Typeable, Show, Eq)

-- NOTE: Functionality is not implemented yet
writeTreeMode :: HaskGit
writeTreeMode = WriteTree {filename = def &= typFile &= argPos 0} &=
  help "haskgit show <filename>\nCreate a tree object from the current index"

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

-- loadIndex : IO ByteString
-- loadIndex = undefined

saveIndex :: ByteString -> IO ()
saveIndex = undefined

loadGitObject :: ByteString -> IO ByteString
loadGitObject = undefined

saveGitObject :: ByteString -> ByteString -> IO ()
saveGitObject = undefined

-- GitMonad: wrap

-- Question: having load and save in the main is not ideal?
-- Question about the scope (if it become infeasible, can we move branching to stretch goal)
