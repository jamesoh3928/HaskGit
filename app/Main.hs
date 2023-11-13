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

main :: IO ()
main = do
  cmd <- cmdArgs cmdModes
  runCommand cmd

loadIndex :: IO ByteString
loadIndex = undefined

saveIndex :: ByteString -> IO ()
saveIndex = undefined

loadGitObject :: ByteString -> IO ByteString
loadGitObject = undefined

saveGitObject :: ByteString -> ByteString -> IO ()
saveGitObject = undefined

-- GitMonad: wrap

-- Question: having load and save in the main is not ideal?
-- Question about the scope (if it become infeasible, can we move branching to stretch goal)