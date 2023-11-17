module Util
  ( getGitDirectory,
    gitHashObject,
    saveGitObject,
    refToFilePath,
    hashToFilePath,
    gitRefToCommit,
  )
where

import Codec.Compression.Zlib (compress, decompress)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import GitObject (GitCommit, GitObject (..), GitTree, gitObjectSerialize)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents, listDirectory)
import System.FilePath

-- Given hash value, return corresponding git directory
-- Example:
-- hashToFilePath "f6f754dbe0808826bed2237eb651558f75215cc6"
-- output: IO ".haskgit/objects/f6/f754dbe0808826bed2237eb651558f75215cc6"
hashToFilePath :: String -> IO FilePath
hashToFilePath hash = do
  gitdir <- getGitDirectory
  return (gitdir ++ "/objects/" ++ take 2 hash ++ "/" ++ drop 2 hash)

-- Returns path to reference
-- Example:
-- refToFilePath refs/heads/main
-- ".haskgit/refs/heads/main"
refToFilePath :: String -> IO FilePath
refToFilePath ref = do
  gitdir <- getGitDirectory
  return (gitdir ++ "/" ++ ref)

getGitDirectory :: IO FilePath
getGitDirectory = do
  curr <- getCurrentDirectory
  findGitDirectory curr

findGitDirectory :: FilePath -> IO FilePath
findGitDirectory fp = do
  if fp == "~" || fp == "/"
    then return fp
    else do
      xs <- getDirectoryContents fp
      if ".haskgit" `elem` xs
        then return (fp ++ "/.haskgit")
        else findGitDirectory (takeDirectory fp)

-- take GitObject and save in .haskgit/objects file
saveGitObject :: ByteString -> GitObject -> IO ()
saveGitObject hash obj = do
  let content = compress (BSLC.fromStrict (gitObjectSerialize obj))
  path <- hashToFilePath (B.unpack (encode hash))
  createDirectoryIfMissing True (takeDirectory path)
  BSLC.writeFile path content

-- List of plumbing commands

-- This command computes the SHA-1 hash of Git objects.
gitHashObject :: GitObject -> ByteString
gitHashObject obj = SHA1.hash (gitObjectSerialize obj)

-- Given ref, return hash of the commit object.
-- Input: ref
-- Output: hash of commit object
-- if ref is pointing to other ref, recurse it until it finds commit hash
gitRefToCommit :: String -> IO (Maybe String)
gitRefToCommit ref = do
  refPath <- refToFilePath ref
  fileExist <- doesFileExist refPath
  if fileExist
    then do
      content <- readFile refPath
      let obj = head (lines content)
      if take 5 obj == "ref: "
        then gitRefToCommit (drop 5 obj)
        else return (Just obj)
    else do
      return Nothing