module HaskGit
  ( gitHashObject,
  )
where

import Codec.Compression.Zlib (compress, decompress)
-- import qualified Data.ByteString.Char8 as BS
-- import qualified Data.ByteString.Lazy as BSL

import Control.Monad
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Builder as BSL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Time.Clock (UTCTime)
import GitObject (GitCommit, GitObject, GitTree, gitObjectSerialize, gitShowStr, newGitObjectHash)
import GitParser (parseGitObject)
import Index
import Ref
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents, listDirectory)
import System.FilePath
import Text.Parsec (parse)

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

-- take GitObject and save
saveGitObject :: ByteString -> GitObject -> IO ()
saveGitObject hash obj = do
  let content = compress (BSLC.fromStrict (gitObjectSerialize obj))
  -- (compress (BSLC.pack content))
  path <- hashToFilePath (B.unpack (encode hash))
  -- b <- doesDirectoryExist path
  -- if b
  --   then BSLC.writeFile (path ++ "/" ++ drop 2 h) content
  --   else do
  createDirectoryIfMissing True (takeDirectory path)
  BSLC.writeFile path content

-- List of plumbing commands

-- This command computes the SHA-1 hash of Git objects.
gitHashObject :: GitObject -> ByteString
gitHashObject obj = SHA1.hash (gitObjectSerialize obj)

gitHashCommand :: GitObject -> Bool -> IO ByteString
gitHashCommand obj b = do
  let hash = gitHashObject obj
  if b
    then do
      saveGitObject hash obj
      return hash
    else return hash

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
-- Ref mostly contains hash but can also store symbolic ref
-- TODO: may need to update the signature
-- git update-ref <refname> <new-object-name>
-- refs/heads/branch-name
gitUpdateRef :: String -> String -> IO ()
gitUpdateRef ref obj = do
  path <- refToFilePath ref
  objHashPath <- hashToFilePath obj
  objRefPath <- refToFilePath obj
  hashCheck <- doesFileExist objHashPath
  refCheck <- doesFileExist objRefPath
  if not (validateRef ref)
    then putStrLn "Invalid reference. Please provide either HEAD or refs/..."
    else
      if not hashCheck
        then
          if not refCheck
            then putStrLn "Invalid new object. Please provide hash of exsting commit object or ref."
            else -- when objects is ref
              writeFile path ("refs: " ++ obj ++ "\n")
        else do
          createDirectoryIfMissing True (takeDirectory path)
          writeFile path (obj ++ "\n")

-- ref is valid if it starts with 'refs/' or it is HEAD
validateRef :: String -> Bool
validateRef ref = ref == "HEAD" || take 5 ref == "refs/"

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

-- To see git objects in .haskgit
haskGitShow :: ByteString -> IO ()
haskGitShow hash = do
  -- TODO: need to convert bytestring to the actual string
  -- TODO: find the git directory based on the filename (right now, assuming we are in root)
  -- 2 hexadecimal = 4 bytes
  let filename = ".haskgit/objects/" ++ take 2 (B.unpack hash) ++ "/" ++ drop 2 (B.unpack hash)
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