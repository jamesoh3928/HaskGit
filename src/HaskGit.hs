module HaskGit
  ( gitShow,
    gitHashObject,
    gitHashCommand,
  )
where

import Codec.Compression.Zlib (compress, decompress)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Time.Clock (UTCTime)
import GitObject (GitCommit, GitObject (..), GitTree, gitObjectSerialize, gitShowStr, newGitObjectHash)
import GitParser (parseGitObject)
import Index (GitIndex, gitIndexSerialize)
import Ref (GitRef)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath
import Text.Parsec (parse)
import Util

testHashCommand :: String -> IO ()
testHashCommand filename = do
  content <- BSLC.readFile filename
  -- hashing without the header
  case parse parseGitObject "" (BSLC.unpack (decompress content)) of
    Left err -> Prelude.putStrLn $ "Parse error: " ++ show err
    Right result -> do
      -- print (gitHashObject result)
      hash <- gitHashCommand result True
      print (encode hash)

gitHashCommand :: GitObject -> Bool -> IO ByteString
gitHashCommand obj b = do
  let hash = gitHashObject obj
  if b
    then do
      saveGitObject hash obj
      return hash
    else return hash

-- This command creates a tree object from the current index (staging area).
gitWriteTree :: GitIndex -> IO ()
gitWriteTree = undefined

-- This command reads a tree object and checks it out in the working directory.
gitReadTree :: ByteString -> GitIndex
gitReadTree = undefined

-- This command creates a new commit object based on a tree object and parent commits.
gitCommitTree :: GitTree -> [GitCommit] -> String -> String -> String -> UTCTime -> GitCommit
gitCommitTree = undefined

-- Update the object name stored in a ref safely
-- Ref mostly contains hash but can also store symbolic ref
-- git update-ref <refname> <new-object-name>
-- ref can be also symbolic ref such as HEAD
-- gitUpdateRef "refs/heads/main" hashvalue
-- gitUpdateRef "refs/heads/main" "refs/heads/test"
gitUpdateRef :: String -> String -> IO ()
gitUpdateRef ref obj = do
  path <- refToFilePath ref
  -- Check if obj is alreayd commit hash
  hashPath <- hashToFilePath obj
  hashExist <- doesFileExist hashPath
  if hashExist
    then writeFile path (obj ++ "\n")
    else -- else save commit hash that ref is pointing
    do
      objHash <- gitRefToCommit obj
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

-- List of porcelain commands
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
gitShow :: ByteString -> IO ()
gitShow hash = do
  -- 2 hexadecimal = 4 bytes
  gitdir <- getGitDirectory
  let hashHex = B.unpack hash
  let filename = gitdir ++ "/objects/" ++ take 2 hashHex ++ "/" ++ drop 2 hashHex
  filecontent <- BSLC.readFile filename
  case parse parseGitObject "" (BSLC.unpack (decompress filecontent)) of
    Left err -> Prelude.putStrLn $ "Git show parse error: " ++ show err
    Right gitObj -> Prelude.putStrLn $ gitShowStr (newGitObjectHash gitObj hash)

gitLog :: ByteString -> IO String
gitLog = undefined

gitRevert :: ByteString -> IO ()
gitRevert = undefined
