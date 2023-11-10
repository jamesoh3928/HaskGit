module HaskGit
  (
  )
where

import Codec.Compression.Zlib (compress, decompress)
-- import qualified Data.ByteString.Char8 as BS

-- import qualified Data.ByteString.Lazy as BSL

import Control.Monad
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Time.Clock (UTCTime)
import GitObject (GitCommit, GitObject, GitTree, gitObjectSerialize, gitShowStr, newGitObjectHash)
import GitParser (parseGitObject)
import Index
import Ref
import Text.Parsec (parse)

-- List of plumbing commands

-- This command computes the SHA-1 hash of Git objects.
gitHashObject :: GitObject -> Bool -> ByteString
gitHashObject obj _ = SHA1.hash (gitObjectSerialize obj)

-- gitHashObject obj _ = SHA1.hash (gitObjectToBS (getBlobContent obj))

testHash :: String -> IO ()
testHash filename = do
  content <- BSLC.readFile filename
  -- hash with the header
  -- -- -- putStrLn (show content)
  -- -- -- putStrLn (BSLC.unpack (decompress content))
  -- let b = newBlob 899 (BSLC.unpack (decompress content)) "Blob_test"
  -- let h = gitHashObject b True
  -- let hex = encode h
  -- -- putStrLn ("Blob object show: " ++ (show b))
  -- -- putStrLn (BSLC.unpack (decompress content))
  -- putStrLn ("Hash in hexadecimal with header: " ++ show hex)

  -- hashing without the header
  case parse parseGitObject "" (BSLC.unpack (decompress content)) of
    Left err -> Prelude.putStrLn $ "Parse error: " ++ show err
    Right result -> do
      print (encode (gitHashObject result False))

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

-- Test in GHCI:
-- Blob: gitShow (B.pack "f6f754dbe0808826bed2237eb651558f75215cc6")
-- Tree: gitShow (B.pack "f6e1af0b636897ed62c8c6dad0828f1172b9b82a")
-- Commit: gitShow (B.pack "562c9c7b09226b6b54c28416d0ac02e0f0336bf6")
gitShow :: ByteString -> IO ()
gitShow hash = do
  -- TODO: need to convert bytestring to the actual string
  -- TODO: find the git directory based on the filename (right now, assuming we are in root)
  -- 2 hexadecimal = 4 bytes
  let filename = ".git/objects/" ++ take 2 (B.unpack hash) ++ "/" ++ drop 2 (B.unpack hash)
  filecontent <- BSLC.readFile filename
  case parse parseGitObject "" (BSLC.unpack (decompress filecontent)) of
    Left err -> Prelude.putStrLn $ "Git show parse error: " ++ show err
    Right gitObj -> Prelude.putStrLn $ gitShowStr (newGitObjectHash gitObj hash)

gitLog :: ByteString -> String
gitLog = undefined

gitRebase :: ByteString -> ByteString
gitRebase = undefined

gitRevert :: ByteString -> ByteString
gitRevert = undefined