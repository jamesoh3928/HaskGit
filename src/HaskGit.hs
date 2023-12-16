module HaskGit
  ( gitAdd,
    gitShow,
    gitUpdateRef,
    gitUpdateSymbRef,
    gitListBranch,
    gitRevList,
    gitLog,
    gitRevList,
    gitLog,
  )
where

import Codec.Compression.Zlib (compress, decompress)
import qualified Control.Monad
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List
import Data.Time (getCurrentTimeZone)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.ExecutionStack (Location (objectName))
import GitHash (GitHash, bsToHash, getHash, gitHashValue)
import GitObject
import GitParser (parseGitObject, parseIndexFile, readObjectByHash)
import Index (GitIndex (GitIndex), GitIndexEntry (..), addOrUpdateEntries, gitIndexSerialize, removeEntries, saveIndexFile)
import Ref (GitRef)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath
import Text.Parsec (parse)
import Util

-------------------------- List of plumbing commands --------------------------

-- This command creates a tree object from the current index (staging area).
gitWriteTree :: FilePath -> IO GitHash
gitWriteTree gitDir = undefined

-- TODO: continue James
-- do
-- index <- parseIndexFile (gitDir ++ "/index")
-- -- let tree = GitTree (addOrUpdateEntries [] index)
-- hashAndSaveObject tree gitDir

-- This command Reads tree information into the index.
-- TODO: Test gitReadTree manually and add tasty test
gitReadTree :: ByteString -> FilePath -> IO ()
gitReadTree treeHash gitDir = do
  -- Read tree
  treePath <- hashToFilePath (bsToHash treeHash) gitDir
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
                indexTmp <- addOrUpdateEntries pathsToAdd index gitDir
                -- Remove entries that does not exists in tree object
                let pathsToRemove = getIndexEntryToRemove treeObj indexTmp
                let newIndex = removeEntries pathsToRemove indexTmp
                saveIndexFile (gitIndexSerialize newIndex) gitDir
        _ -> putStrLn "Invalid input, must input a tree hash value."
  where
    -- Return true if hash exists in the index
    hashExistIndex :: ByteString -> GitIndex -> Bool
    hashExistIndex _ (GitIndex []) = False
    hashExistIndex hash (GitIndex (x : xs)) = (sha x == bsToHash hash) || hashExistIndex hash (GitIndex xs)

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
      if hashExistTree (getHash (sha x)) treeObj
        then getIndexEntryToRemove treeObj (GitIndex xs)
        else name x : getIndexEntryToRemove treeObj (GitIndex xs)

-- | This command creates a new commit object based on a tree object and parent commits.
gitCommitTree :: GitHash -> [GitHash] -> GitAuthor -> GitCommitter -> String -> UTCTime -> FilePath -> IO GitHash
gitCommitTree treeHash parents author committer message time =
  hashAndSaveObject (Commit (0, treeHash, parents, author, committer, message))

-- | Update the object name stored in a ref safely.
-- The second argument (object name) can be a commit hash or a name ref.
-- git update-ref <refname> <new-object-name>
-- e.g. gitUpdateRef "refs/heads/main" object_hashvalue ".test_haskgit"
gitUpdateRef :: String -> String -> FilePath -> IO ()
gitUpdateRef ref obj gitDir = do
  path <- refToFilePath ref gitDir
  -- No need to convert to GitHash type in this function
  let hashPath = gitDir ++ "/objects/" ++ take 2 obj ++ "/" ++ drop 2 obj
  -- Check if obj is already commit hash
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
    else putStrLn ("Ref does not exist, Ref: " ++ ref ++ ", refPath: " ++ refPath)

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
-- haskgit revList 3154bdc4928710b08f61297e87c4900e0f9b5869
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
-- | return a list of parents (commit type only)
-- haskgit revList 3154bdc4928710b08f61297e87c4900e0f9b5869
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

-- | Add the given files to the index.
-- The git add can be ran in the any directory in the repository.
-- The given paths must be relative to the current directory.
gitAdd :: [FilePath] -> FilePath -> IO ()
gitAdd paths gitDir = do
  -- Convert all the paths to path relative to the repository
  repoDirectory <- getRepoDirectory
  absPaths <- mapM relativeToAbolutePath paths
  -- (strip of repository path in the beginning)
  let pathsWithMaybe = map (stripPrefix (repoDirectory ++ "/")) absPaths
  let relativePaths = map (\(Just x) -> x) pathsWithMaybe

  -- Read in the index file located in gitDir/.haskgit/index
  indexContent <- BSC.readFile (gitDir ++ "/index")
  case parse parseIndexFile "" (BSC.unpack indexContent) of
    Left err -> Prelude.putStrLn $ "haskgit add parse error: " ++ show err
    -- Remove the files from the index if they exist and add given files to the index
    Right index -> do
      newIndex <- addOrUpdateEntries relativePaths index gitDir
      saveIndexFile (gitIndexSerialize newIndex) gitDir

gitStatus :: ByteString -> IO ()
gitStatus = undefined

-- We first need to convert the index into a tree object, generate and store the corresponding commit object, and update the current branch to the new commit (remember: a branch is just a ref to a commit).
gitCommit :: FilePath -> String -> IO ()
gitCommit gitDir message = do
  -- Call writeTree to create a tree object from the current index
  treeHash <- gitWriteTree gitDir
  head <- readFile (gitDir ++ "/HEAD")
  curCommitMaybe <- gitRefToCommit (drop 5 head) gitDir
  curCommitHash <- case curCommitMaybe of
    Nothing -> return []
    Just hash -> return [bsToHash (BSC.pack hash)]

  -- Get all data we need to create commit object
  utcTime <- getCurrentTime
  timezone <- getCurrentTimeZone
  let unixTS = floor (utcTimeToPOSIXSeconds utcTime) -- Unix time in seconds
  -- TODO: double check
  let authorInfo = ("author1", "author1@cs.rit.edu", unixTS, show timezone)
  let committerInfo = ("commiter1", "committer1@cs.rit.edu", unixTS, show timezone)

  -- Create a new commit object based on the tree object, parent commits, and other data
  newCommitHash <- gitCommitTree treeHash curCommitHash authorInfo committerInfo message utcTime gitDir

  -- Convert gitHash to encoded string hash value
  let newCommitHashStr = BSC.unpack (getHash newCommitHash)

  -- Call updateRef to update the current branch to the new commit hash
  -- If we are in branch, move branch pointer, otherwise, move the HEAD to the new commit hash
  case curCommitMaybe of
    Nothing -> gitUpdateRef (drop 5 head) newCommitHashStr gitDir
    Just _ -> gitUpdateRef "HEAD" newCommitHashStr gitDir

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
-- TODO: delete in the future
--
-- Display the contents of the git object for the given hash.
gitShow :: ByteString -> FilePath -> IO ()
gitShow hash gitDir = do
  gitObj <- readObjectByHash hash gitDir
  case gitObj of
    Nothing -> return ()
    Just (gitObj, hashV) -> Prelude.putStrLn $ gitShowStr (gitObj, hashV)

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
gitShow hash gitDir = do
  gitObj <- readObjectByHash hash gitDir
  case gitObj of
    Nothing -> return ()
    Just (gitObj, hashV) -> Prelude.putStrLn $ gitShowStr (gitObj, hashV)

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

-- | a list of Commit w/ git show, start from provided hash
-- haskgit log 3154bdc4928710b08f61297e87c4900e0f9b5869
gitLog :: ByteString -> FilePath -> IO ()
gitLog hash gitdir = do
  parents <- gitParentList hash gitdir
  mapM_
    ( \cmt@(Commit (_, hs, _, _, _, _)) ->
        putStrLn $ gitShowStr (cmt, hs)
    )
    parents
