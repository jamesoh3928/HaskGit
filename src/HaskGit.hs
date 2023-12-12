module HaskGit
  ( gitShow,
    hashObject,
    hashAndSaveObject,
    gitUpdateRef,
  )
where

import Codec.Compression.Zlib (compress, decompress)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Time.Clock (UTCTime)
import GHC.ExecutionStack (Location (objectName))
import GitHash (GitHash, bsToHash, gitHashValue)
import GitObject (GitCommit, GitObject (..), GitTree, gitObjectSerialize, gitShowStr, saveGitObject)
import GitParser (parseGitObject)
import Index (GitIndex, gitIndexSerialize)
import Ref (GitRef)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath
import Text.Parsec (parse)
import Util

-- Computes the SHA-1 hash of Git objects.
hashObject :: GitObject -> GitHash
hashObject obj = bsToHash (SHA1.hash (gitObjectSerialize obj))

-- Computes the SHA-1 hash of Git objects and save it.
hashAndSaveObject :: GitObject -> FilePath -> IO GitHash
hashAndSaveObject obj gitDir = do
  let content = gitObjectSerialize obj
  let hash = SHA1.hash content
  saveGitObject hash content gitDir
  return (bsToHash hash)

-------------------------- List of plumbing commands --------------------------

-- This command creates a tree object from the current index (staging area).
-- TODO: James
gitWriteTree :: GitIndex -> IO ()
gitWriteTree = undefined

-- This command reads a tree object and checks it out in the working directory.
gitReadTree :: ByteString -> GitIndex
gitReadTree = undefined

-- This command creates a new commit object based on a tree object and parent commits.
gitCommitTree :: GitTree -> [GitCommit] -> String -> String -> String -> UTCTime -> GitCommit
gitCommitTree = undefined

-- Update the object name stored in a ref safely.
-- git update-ref <refname> <new-object-name>
-- e.g. gitUpdateRef "refs/heads/main" hashvalue
gitUpdateRef :: String -> String -> FilePath -> IO ()
gitUpdateRef ref obj gitDir = do
  path <- refToFilePath ref gitDir
  -- Check if obj is already commit hash
  hashPath <- hashToFilePath obj gitDir
  hashExist <- doesFileExist hashPath
  if hashExist
    then writeFile path (obj ++ "\n")
    else -- else save commit hash that ref is pointing
    do
      objHash <- gitRefToCommit obj gitDir
      case objHash of
        Nothing -> putStrLn "Invalid argument. Provided object doesn't exist."
        Just hash ->
          do
            createDirectoryIfMissing True (takeDirectory path)
            writeFile path (hash ++ "\n")

-- This command is used to add or update index entries (staging area).
gitUpdateIndex :: GitIndex -> ByteString
gitUpdateIndex = undefined

-- This command reads the index file, which represents the current state of the working directory.
gitReadCache :: ByteString -> GitIndex
gitReadCache = undefined

-- This command provides a way to traverse and filter the commit history in various ways
gitRevList :: ByteString -> [GitCommit]
gitRevList = undefined

-------------------------- List of porcelain commands --------------------------
gitAdd :: ByteString -> IO ()
gitAdd = undefined

gitStatus :: ByteString -> IO ()
gitStatus = undefined

gitCommit :: ByteString -> IO ()
gitCommit = undefined

gitReset :: ByteString -> IO ()
gitReset = undefined

gitCheckout :: ByteString -> IO ()
gitCheckout = undefined

gitBranch :: ByteString -> IO ()
gitBranch = undefined

-- Test in GHCI:
-- Blob: gitShow (B.pack "f6f754dbe0808826bed2237eb651558f75215cc6")
-- Tree: gitShow (B.pack "f6e1af0b636897ed62c8c6dad0828f1172b9b82a")
-- Commit: gitShow (B.pack "562c9c7b09226b6b54c28416d0ac02e0f0336bf6")
--
-- Display the contents of the git object for the given hash.
gitShow :: ByteString -> FilePath -> IO ()
gitShow hash gitdir = do
  -- 2 hexadecimal = 4 bytes
  let hashHex = BSC.unpack hash
  let filename = gitdir ++ "/objects/" ++ take 2 hashHex ++ "/" ++ drop 2 hashHex
  filecontent <- BSLC.readFile filename
  case parse parseGitObject "" (BSLC.unpack (decompress filecontent)) of
    Left err -> Prelude.putStrLn $ "Git show parse error: " ++ show err
    Right gitObj -> case gitHashValue hash of
      Nothing -> Prelude.putStrLn "Invalid hash value given"
      Just hashV -> Prelude.putStrLn $ gitShowStr (gitObj, hashV)

gitLog :: ByteString -> IO String
gitLog = undefined

gitRevert :: ByteString -> IO ()
gitRevert = undefined
