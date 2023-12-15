module HaskGit
  ( gitAdd,
    gitShow,
    hashObject,
    hashAndSaveObject,
    gitUpdateRef,
    gitUpdateSymbRef,
    gitListBranch,
  )
where

import Codec.Compression.Zlib (compress, decompress)
import qualified Control.Monad
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List
import Data.Time.Clock (UTCTime)
import GHC.ExecutionStack (Location (objectName))
import GitHash (GitHash, bsToHash, gitHashValue)
import GitObject (GitCommit, GitObject (..), GitTree, gitObjectSerialize, gitShowStr, saveGitObject)
import GitParser (parseGitObject, parseIndexFile, readObjectByHash)
import Index (GitIndex (GitIndex), GitIndexEntry (..), addOrUpdateEntries, gitIndexSerialize, removeEntries, saveIndexFile)
import Ref (GitRef)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
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
  let hash = bsToHash (SHA1.hash content)
  saveGitObject hash content gitDir
  return hash

-------------------------- List of plumbing commands --------------------------

-- This command creates a tree object from the current index (staging area).
-- TODO: James
gitWriteTree :: GitIndex -> FilePath -> IO ()
gitWriteTree = undefined

-- This command Reads tree information into the index.
gitReadTree :: ByteString -> FilePath -> IO ()
gitReadTree treeHash gitDir = do
  -- Read tree
  treePath <- hashToFilePath (BSC.unpack treeHash) gitDir
  treeContent <- BSLC.readFile treePath
  -- Read in the index file located in gitDir/.haskgit/index
  indexContent <- BSC.readFile (gitDir ++ "/index")

  -- Parse tree
  case parse parseGitObject "" (BSLC.unpack (decompress treeContent)) of
    Left err -> Prelude.putStrLn $ "Git show parse error: " ++ show err
    Right gitObj ->
      case gitObj of
        (Tree treeObj) ->
          case gitHashValue treeHash of
            Nothing -> Prelude.putStrLn "Invalid hash value given"
            Just hashV -> case parse parseIndexFile "" (BSC.unpack indexContent) of
              Left err -> Prelude.putStrLn $ "index parse error: " ++ show err
              Right index -> do
                -- Add or update entries that exists in tree object
                pathsToAdd <- getIndexEntryToAdd treeObj index ""
                indexTmp <- addOrUpdateEntries pathsToAdd index
                -- Remove entries that does not exists in tree object
                let pathsToRemove = getIndexEntryToRemove treeObj indexTmp
                let newIndex = removeEntries pathsToRemove indexTmp
                saveIndexFile (gitIndexSerialize newIndex) gitDir
        _ -> putStrLn "Invalid input, must input a tree hash value."
  where
    -- Return true if hash exists in the index
    hashExistIndex :: ByteString -> GitIndex -> Bool
    hashExistIndex _ (GitIndex []) = False
    hashExistIndex hash (GitIndex (x : xs)) = (sha x == hash) || hashExistIndex hash (GitIndex xs)

    -- Return file paths that need to be added or updated to index based on treeobj
    getIndexEntryToAdd :: GitTree -> GitIndex -> FilePath -> IO [FilePath]
    getIndexEntryToAdd (i, []) _ _ = return []
    getIndexEntryToAdd (i, (fmode, name, hash) : xs) index path =
      if hashExistIndex hash index
        then do
          gitObj <- readObjectByHash hash gitDir
          case gitObj of
            Nothing -> getIndexEntryToAdd (i, xs) index path
            Just (Tree tree, _) -> do
              nested_path <- getIndexEntryToAdd tree index (path ++ "/" ++ name)
              curr_path <- getIndexEntryToAdd (i, xs) index path
              return (nested_path ++ curr_path)
            Just (Blob blob, _) -> do
              curr_path <- getIndexEntryToAdd (i, xs) index path
              return ((path ++ "/" ++ name) : curr_path)
        else getIndexEntryToAdd (i, xs) index path

    -- Return true if hash exists in the tree
    hashExistTree :: ByteString -> GitTree -> Bool
    hashExistTree _ (i, []) = False
    hashExistTree hash (i, (_, _, hashV) : xs) = (hashV == hash) || hashExistTree hash (i, xs)

    -- Return file paths that need to be removed based on treeobj
    getIndexEntryToRemove :: GitTree -> GitIndex -> [FilePath]
    getIndexEntryToRemove _ (GitIndex []) = []
    getIndexEntryToRemove treeObj (GitIndex (x : xs)) =
      if hashExistTree (sha x) treeObj
        then getIndexEntryToRemove treeObj (GitIndex xs)
        else (name x) : getIndexEntryToRemove treeObj (GitIndex xs)

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

-- Given name and ref, update the symbolic ref.
-- e.g. gitUpdateSymbRef HEAD "refs/head/main"
gitUpdateSymbRef :: String -> String -> FilePath -> IO ()
gitUpdateSymbRef symb ref gitDir = do
  let path = gitDir ++ "/" ++ symb
  refPath <- refToFilePath ref gitDir
  fileExist <- doesFileExist refPath
  if fileExist
    then writeFile path ("ref: " ++ ref)
    else putStrLn "Ref does not exist."

-- List all the branch
-- print "*" next to current branch
-- If current HEAD is detacahed, print "* (no branch)" on top
gitListBranch :: FilePath -> IO ()
gitListBranch gitdir = do
  let branchPath = gitdir ++ "/refs/heads"
  branches <- listDirectory branchPath
  -- Different cases when symbolic ref
  head <- readFile (gitdir ++ "/HEAD")
  -- If HEAD is not pointing to symbolic link, print * (no branch) on top
  Control.Monad.when (take 5 head /= "ref: ") $ putStrLn "* (no branch)"
  -- Print branches
  printBranch (sort branches) head
  where
    printBranch [] _ = return ()
    printBranch (x : xs) head = do
      if drop 5 head == ("refs/heads/" ++ x) then putStrLn ("* " ++ x) else putStrLn ("  " ++ x)
      printBranch xs head

-- Create a new branch which will point to commit that HEAD is currently pointing.
-- If branch aleready exists, just update the branch pointer.
-- If HEAD is pointing ref, find the commit that ref is pointing and let branch point
-- If HEAD is pointing hash, let branch point to hash.
gitCreateBranch :: String -> FilePath -> IO ()
gitCreateBranch name gitDir = do
  let path = gitDir ++ "/refs/heads/" ++ name
  head <- readFile (gitDir ++ "/HEAD")
  if take 5 head == "ref: "
    then do
      hash <- gitRefToCommit (drop 5 head) gitDir
      case hash of
        Nothing -> putStrLn "HEAD is pointing to invalid ref."
        Just h -> writeFile path h
    else writeFile path head

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
gitAdd :: [FilePath] -> FilePath -> IO ()
gitAdd paths gitDir = do
  -- Read in the index file located in gitDir/.haskgit/index
  indexContent <- BSC.readFile (gitDir ++ "/index")
  case parse parseIndexFile "" (BSC.unpack indexContent) of
    Left err -> Prelude.putStrLn $ "haskgit add parse error: " ++ show err
    -- Remove the files from the index if they exist and add given files to the index
    Right index -> do
      newIndex <- addOrUpdateEntries paths index
      saveIndexFile (gitIndexSerialize newIndex) gitDir

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
gitShow hash gitDir = do
  gitObj <- readObjectByHash hash gitDir
  case gitObj of
    Nothing -> return ()
    Just (gitObj, hashV) -> Prelude.putStrLn $ gitShowStr (gitObj, hashV)

gitLog :: ByteString -> IO String
gitLog = undefined

gitRevert :: ByteString -> IO ()
gitRevert = undefined
