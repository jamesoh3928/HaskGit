module HaskGit
  (
  )
where

import Codec.Compression.Zlib (compress, decompress)
import qualified Crypto.Hash.SHA1 as SHA1
-- import Data.ByteString (ByteString)

import Data.ByteString.Lazy (ByteString, pack)
-- import Data.ByteString.Lazy.Char8 (pack)
import Data.Time.Clock (UTCTime)
import GitObject (GitCommit, GitObject, GitTree)
import Index
import Ref

-- List of plumbing commands

testGitHashBlob :: String -> ByteString
testGitHashBlob content = gitHashObject ((compress (pack content), "test"))

-- This command computes the SHA-1 hash of Git objects.
gitHashObject :: GitObject -> Bool -> ByteString
-- gitHashObject = undefined
-- when object is blob
gitHashObject (bs, s) _ = SHA1.hashlazy bs

-- This command creates a tree object from the current index (staging area).
gitWriteTree :: GitIndex -> ByteString
gitWriteTree = undefined

-- This command reads a tree object and checks it out in the working directory.
gitReadTree :: ByteString -> GitIndex
gitReadTree = undefined

-- This command creates a new commit object based on a tree object and parent commits.
gitCommitTree :: GitTree -> [GitCommit] -> String -> String -> String -> UTCTime -> GitCommit
gitCommitTree = undefined

-- Update the object name stored in a ref safely
-- TODO: may need to update the signature
gitUpdateRef :: GitRef -> GitRef
gitUpdateRef = undefined

-- This command is used to add or update index entries (staging area).
gitUpdateIndex :: GitIndex -> ByteString
gitUpdateIndex = undefined

-- This command reads the index file, which represents the current state of the working directory.
gitReadCache :: ByteString -> GitIndex
gitReadCache = undefined

-- This command provides a way to traverse and filter the commit history in various ways
gitRevList :: ByteString -> [GitCommit]
gitRevList = undefined

--
serializeGitObject :: GitObject -> ByteString
serializeGitObject = undefined

--
deserializeGitObject :: ByteString -> GitObject
deserializeGitObject = undefined

-- List of porcelain commands
gitInit :: () -> ByteString
gitInit = undefined

gitAdd :: ByteString -> ByteString
gitAdd = undefined

gitStatus :: ByteString -> String
gitStatus = undefined

gitCommit :: ByteString -> ByteString
gitCommit = undefined

gitRestore :: ByteString -> ByteString
gitRestore = undefined

gitReset :: ByteString -> ByteString
gitReset = undefined

gitRm :: ByteString -> ByteString
gitRm = undefined

gitCheckout :: ByteString -> ByteString
gitCheckout = undefined

gitBranch :: ByteString -> ByteString
gitBranch = undefined

gitShow :: ByteString -> String
gitShow = undefined

gitLog :: ByteString -> String
gitLog = undefined

gitRebase :: ByteString -> ByteString
gitRebase = undefined

gitRevert :: ByteString -> ByteString
gitRevert = undefined