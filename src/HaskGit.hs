module HaskGit
  ( gitAdd,
    gitShow,
    hashObject,
    hashAndSaveObject,
    gitUpdateRef,
    gitUpdateSymbRef,
    gitListBranch,
    gitRevList,
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
import GitHash (GitHash, bsToHash, getHash, gitHashValue)
import GitObject (GitCommit, GitObject (..), GitTree, gitObjectSerialize, gitShowStr, saveGitObject)
import GitParser (parseGitObject, parseIndexFile)
import Index (GitIndex, addOrUpdateEntries, gitIndexSerialize, saveIndexFile)
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

-- This command reads a tree object and checks it out in the working directory.
gitReadTree :: GitHash -> FilePath -> GitIndex
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

-- | return a list of parents (commit type only)
gitRevList :: ByteString -> FilePath -> IO ()
gitRevList hash gitdir = do
  parents <- gitParentList hash gitdir
  hashList <-
    concat
      <$> Control.Monad.forM
        parents
        ( \(Commit (_, _, ps, _, _, _)) ->
            Control.Monad.forM ps (return . getHash)
        )
  if not (null hashList)
    then do
      putStrLn $ BSC.unpack hash
      mapM_ (putStrLn . BSC.unpack) hashList
    else print $ show hash ++ " is not a commit"

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

-- | Get a list of parent (Git Object) in the tree
gitParentList :: ByteString -> FilePath -> IO [GitObject]
gitParentList hash gitdir = do
  obj <- hash2CommitObj hash gitdir
  case obj of
    Nothing -> return []
    Just cmt@(Commit (_, _, parents, _, _, _)) -> do
      recur <- Control.Monad.forM parents (\p -> gitParentList (getHash p) gitdir)
      return (cmt : concat recur)
    _ -> return []

-- |
-- decompress gitObject and unpack ByteString to String
gitObjectContent :: ByteString -> FilePath -> IO String
gitObjectContent hash gitdir = do
  let hashHex = BSC.unpack hash
  let filename = gitdir ++ "/objects/" ++ take 2 hashHex ++ "/" ++ drop 2 hashHex
  filecontent <- BSLC.readFile filename
  return (BSLC.unpack (decompress filecontent))

-- |
-- Convert the hash to Git Object of Commit (only)
hash2CommitObj :: ByteString -> FilePath -> IO (Maybe GitObject)
hash2CommitObj hash gitdir = do
  content <- gitObjectContent hash gitdir
  case parse parseGitObject "" content of
    Left error -> return Nothing
    Right gitObject -> case gitHashValue hash of
      Just _ ->
        case gitObject of
          Commit (_, _, _, _, _, _) -> return (Just gitObject)
          _ -> return Nothing
      _ -> return Nothing

gitLog :: ByteString -> IO String
gitLog = undefined

gitRevert :: ByteString -> IO ()
gitRevert = undefined
