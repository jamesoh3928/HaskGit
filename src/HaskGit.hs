{-# LANGUAGE LambdaCase #-}

module HaskGit
  ( gitAdd,
    gitShow,
    gitUpdateRef,
    gitListBranch,
    gitCreateBranch,
    gitLog,
    gitCommit,
    gitRevList,
    gitReadTree,
    gitHashObject,
    gitCheckout,
    gitStatus,
    gitReset,
    gitResetSoft,
    gitResetMixed,
    gitResetHard,
    gitUpdateSymbRef,
  )
where

import Codec.Compression.Zlib (decompress)
import qualified Control.Monad
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Graph (path)
import Data.List (find, isPrefixOf, nub, sort, sortOn, stripPrefix)
import qualified Data.Map as Map
import Data.Time (getCurrentTimeZone)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format
import GitHash (GitHash, bsToHash, getHash)
import GitObject
import GitParser (parseGitObject, parseIndexFile, readObjectByHash)
import Index
import Ref (GitRef)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, removeFile)
import System.FilePath
import System.IO (readFile')
import Text.Parsec (parse)
import Text.Printf (printf)
import Util

-------------------------- List of plumbing commands --------------------------

-- | Creates a tree object from the current index file
gitWriteTree :: FilePath -> IO (Maybe GitHash)
gitWriteTree gitDir = do
  index <- BSC.readFile (gitDir ++ "/index")
  -- Create a map of directory to files (dict), also create a sorted list of keys (sortedKeys)
  case parse parseIndexFile "" (BSC.unpack index) of
    Left err -> return Nothing
    Right gitIndex -> do
      let (GitIndex entries) = gitIndex
      -- Transcode the mode: the entry stores it as integers, but need an octal ASCII representation for tree
      let dict = Map.fromListWith (++) (map (\ie -> (takeDirectory (name ie), [(printf "%02o%04o" (modeType ie) (modePerms ie), name ie, sha ie)])) entries)
      -- Sort the `keys` from longest length to shortest length
      let sortedKeys = sortOn (\x -> -1 * length x) (Map.keys dict)
      -- Traverse the sorted keys, create and save the tree object and if the parent directory is in the dict, add the tree object to the parent directory
      res <- traverse sortedKeys dict
      case res of
        Nothing -> return Nothing
        -- Return the hash of the root tree object after traversing all the keys
        Just hash -> return (Just hash)
  where
    -- Given a list of keys, and the dict, create and save the tree object and add to dict if the parent directory is in the dict
    traverse :: [String] -> Map.Map FilePath [(String, FilePath, GitHash)] -> IO (Maybe GitHash)
    traverse [] _ = return Nothing
    traverse (k : ks) dict = do
      -- Create the tree object (bytesize is placeholder since it will be calculated in serialize function)
      -- Also sort the entries based on the name
      let tree = Tree (0, sortOn (\(_, name, _) -> name) (dict Map.! k))
      -- Save the tree object
      treeHash <- hashAndSaveObject tree gitDir
      -- If the parent directory is in the dict
      let parentDir = takeDirectory k
      case k of
        -- Finished traversing the keys, return the hash of the root tree object (base case)
        "." -> return $ Just treeHash
        _ ->
          if Map.member parentDir dict
            then do
              -- Add the tree object to the parent directory
              let (Just entries) = Map.lookup parentDir dict
              -- Directory mode is 040000, permissions is rwxr-xr-x
              let newEntries = (printf "%02o%04o" (0o040000 :: Int) (0o0755 :: Int), parentDir, treeHash) : entries
              -- Add the new tree object to the dict
              let newDict' = Map.insert parentDir newEntries dict
              -- Recurse with the rest of the keys
              traverse ks newDict'
            else -- Recurse with the rest of the keys
              traverse ks dict

-- -- This command Reads tree information into the index.
gitReadTree :: ByteString -> FilePath -> IO ()
gitReadTree treeH gitDir = do
  treeHash <- case bsToHash treeH of
    Nothing -> error "Invalid hash value given to gitReadTree. Please input a valid hash value."
    Just hashV -> return hashV
  -- Read tree
  let treePath = hashToFilePath treeHash gitDir
  treeContent <- BSLC.readFile treePath
  -- Read in the index file located in gitDir/.haskgit/index
  indexContent <- BSC.readFile (gitDir ++ "/index")

  -- Parse tree
  case parse parseGitObject "" (BSLC.unpack (decompress treeContent)) of
    Left err -> Prelude.putStrLn $ "Git show parse error: " ++ show err
    Right gitObj ->
      case gitObj of
        (Tree treeObj) ->
          case parse parseIndexFile "" (BSC.unpack indexContent) of
            Left err -> Prelude.putStrLn $ "index parse error: " ++ show err
            Right index -> do
              newIndex <- treeObjToIndex treeObj index ""
              saveIndexFile (gitIndexSerialize newIndex) gitDir
        _ -> putStrLn "Invalid input to gitReadTree, must input a tree hash value."
  where
    -- Tree object to index
    -- Iterate treeObj and add/update existing files
    treeObjToIndex :: GitTree -> GitIndex -> String -> IO GitIndex
    treeObjToIndex (i, []) _ _ = return (GitIndex [])
    treeObjToIndex (i, (fmode, name, hash) : xs) oldIndex path = do
      -- print name
      case getIndexEntryByHash hash oldIndex of
        -- If hash doesn't exist in index
        Nothing -> do
          gitObj <- readObjectByHash hash gitDir
          case gitObj of
            -- Move to next object due to parse error
            Nothing -> treeObjToIndex (i, xs) oldIndex path
            -- if object is blob turn blob into indexEntry
            Just (Blob blob, _) -> do
              blobIndex <- blobToIndexEntry hash (if path == "" then name else path ++ "/" ++ name)
              GitIndex rest <- treeObjToIndex (i, xs) oldIndex path
              return (GitIndex (blobIndex : rest))
            -- if object is tree recurse the treeObjToIndex
            Just (Tree tree, _) -> do
              GitIndex nested <- treeObjToIndex tree oldIndex (path ++ "/" ++ name)
              GitIndex rest <- treeObjToIndex (i, xs) oldIndex path
              return (GitIndex (nested ++ rest))
            _ -> treeObjToIndex (i, xs) oldIndex path
        -- if hash exist reuse existing entry in index
        Just indexEntry -> do
          GitIndex rest <- treeObjToIndex (i, xs) oldIndex path
          return (GitIndex (indexEntry : rest))

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
  let path = refToFilePath (removeCorrupts ref) gitDir
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
  let refPath = refToFilePath ref gitDir
  fileExist <- doesFileExist refPath
  if fileExist
    then writeFile path ("ref: " ++ ref)
    else putStrLn ("Ref does not exist, Ref: " ++ ref ++ ", refPath: " ++ refPath)

-- | List all existing branches
-- print "*" next to current branch
-- If current HEAD is detached, print "* (no branch)" on top
gitListBranch :: FilePath -> IO ()
gitListBranch gitdir = do
  let branchPath = gitdir ++ "/refs/heads"
  branches <- listDirectory branchPath
  -- Different cases when symbolic ref
  head <- readFile' (gitdir ++ "/HEAD")
  -- If HEAD is not pointing to symbolic link, print * (no branch) on top
  Control.Monad.when (take 5 head /= "ref: ") $ putStrLn "* (no branch)"
  -- Print branches
  putStr $ branchString (sort branches) head
  where
    printBranch [] _ = return ()
    printBranch (x : xs) head = do
      if drop 5 head == "refs/heads/" ++ x then putStrLn ("* " ++ x) else putStrLn ("  " ++ x)
      printBranch xs head
    removeNewLine :: String -> String
    removeNewLine [] = []
    removeNewLine xs = if last xs == '\n' then init xs else xs
    branchString :: [String] -> String -> String
    branchString [] _ = []
    branchString (x : xs) head =
      -- drop "ref: refs/heads/"
      if length head >= 16 && removeNewLine (drop 16 head) == x
        then ("* " ++ x ++ "\n") ++ branchString xs head
        else ("  " ++ x ++ "\n") ++ branchString xs head

-- | Create a new branch which will point to commit that HEAD is currently pointing.
-- If branch aleready exists, just update the branch pointer.
-- If HEAD is pointing ref, find the commit that ref is pointing and let branch point
-- If HEAD is pointing hash, let branch point to hash.
gitCreateBranch :: String -> FilePath -> IO ()
gitCreateBranch name gitDir = do
  let path = gitDir ++ "/refs/heads/" ++ name
  head <- readFile' (gitDir ++ "/HEAD")
  if take 5 head == "ref: "
    then do
      hash <- gitRefToCommit (drop 5 head) gitDir
      case hash of
        Nothing -> putStrLn "HEAD is pointing to invalid ref."
        Just h -> writeFile path h
    else writeFile path head

-- | return a list of parents (commit type only)
-- haskgit revList 3154bdc4928710b08f61297e87c4900e0f9b5869
gitRevList :: ByteString -> FilePath -> IO ()
gitRevList hashBs gitdir = do
  case bsToHash hashBs of
    Nothing -> putStrLn "Invalid hash value given. Please input a valid hash value."
    Just hash -> do
      parents <- gitParentList hash gitdir
      hashList <-
        concat
          <$> Control.Monad.mapM
            ( \(Commit (_, _, ps, _, _, _), _) ->
                Control.Monad.mapM (return . getHash) ps
            )
            parents
      if not (null hashList)
        then do
          putStrLn $ BSC.unpack (getHash hash)
          mapM_ (putStrLn . BSC.unpack) hashList
        else print $ show (getHash hash) ++ " is not a commit"

-- | Copy files from the index to the working tree
gitCheckoutIndex :: FilePath -> IO ()
gitCheckoutIndex gitDir = do
  -- Parse the index
  repoDir <- getRepoDirectory gitDir
  fullGitDir <- getGitDirectory gitDir
  filesFullPath <- listFilesRecursively repoDir fullGitDir
  let files = map (makeRelative repoDir) filesFullPath
  indexContent <- BSC.readFile (fullGitDir ++ "/index")
  case parse parseIndexFile "" (BSC.unpack indexContent) of
    Left err -> Prelude.putStrLn $ "index parse error: " ++ show err
    Right index -> do
      -- List of files in index
      let indexFiles = extractNameIndex index

      -- Get list of hashes of files
      hashFiles <- hashListFiles filesFullPath

      -- If file name doesn't exist in index entries, delete file
      deleteFile files indexFiles repoDir

      -- -- If hash does not exist in the working directory, overwrite
      addOrUpdateFile index hashFiles repoDir

      -- Update index metadata
      newIndex <- updateMetaData index repoDir gitDir
      saveIndexFile (gitIndexSerialize newIndex) gitDir
  where
    -- Delete the file if the it doesnt exist in list of file
    -- arg1: currentl files in repo
    -- arg2: list of file name to keep
    deleteFile :: [FilePath] -> [String] -> String -> IO ()
    deleteFile [] _ _ = return ()
    deleteFile (x : xs) indexFiles repoDir =
      if x `notElem` indexFiles
        then do
          removeFile (repoDir ++ "/" ++ x)
          deleteFile xs indexFiles repoDir
        else deleteFile xs indexFiles repoDir

    -- Add or update files in index if the hash of the index entry doesn't exist in current working directory.
    -- Finds the hash in object file to create a file.
    addOrUpdateFile :: GitIndex -> [GitHash] -> FilePath -> IO ()
    addOrUpdateFile (GitIndex []) _ _ = return ()
    addOrUpdateFile (GitIndex (x : xs)) hashFiles repoDir =
      if sha x `notElem` hashFiles
        then do
          let path = hashToFilePath (sha x) gitDir
          -- parse blob
          cont <- BSC.readFile path
          let content = BSLC.fromStrict cont
          case parse parseGitObject "" (BSLC.unpack (decompress content)) of
            Left err -> Prelude.putStrLn $ "Parse error: " ++ show err
            Right gitObj ->
              case gitObj of
                (Blob (_, blobContent)) -> do
                  writeFile (repoDir ++ "/" ++ name x) blobContent
                  addOrUpdateFile (GitIndex xs) hashFiles repoDir
                _ -> putStrLn "Not a blob."
        else addOrUpdateFile (GitIndex xs) hashFiles repoDir

-------------------------- List of porcelain commands --------------------------

-- | Add the given files to the index.
-- The git add can be ran in the any directory in the repository.
-- The given paths must be relative to the current directory.
gitAdd :: [FilePath] -> FilePath -> IO ()
gitAdd paths gitDir = do
  -- Convert all the paths to path relative to the repository
  repoDirectory <- getRepoDirectory gitDir
  absPaths <- mapM relativeToAbolutePath paths

  -- (strip of repository path in the beginning)
  let pathsWithMaybe = map (stripPrefix (repoDirectory ++ "/")) absPaths
  let relativePaths =
        map
          ( \case
              Just x -> x
              Nothing ->
                -- We should abort program if there is an invalid path
                error "Invalid path given to gitAdd. Please input a valid path."
          )
          pathsWithMaybe

  -- Read in the index file located in gitDir/.haskgit/index
  indexContent <- BSC.readFile (gitDir ++ "/index")
  case parse parseIndexFile "" (BSC.unpack indexContent) of
    Left err -> Prelude.putStrLn $ "haskgit add parse error: " ++ show err
    -- Remove the files from the index if they exist and add given files to the index
    Right index -> do
      newIndex <- addOrUpdateEntries relativePaths index gitDir
      saveIndexFile (gitIndexSerialize newIndex) gitDir

-- Commit the current index (staging area) to the repository with given message.
-- Out git commit does not gurantee that it will abort when there is no files to commit.
gitCommit :: String -> FilePath -> IO ()
gitCommit message gitDir = do
  -- Call writeTree to create a tree object from the current index
  treeHashM <- gitWriteTree gitDir
  case treeHashM of
    Nothing -> putStrLn "Failed to create tree object from the current index file. Please check if your index file is corrupted."
    Just treeHash -> do
      -- Get the current commit hash
      branchP <- readFile' (gitDir ++ "/HEAD")
      curCommitMaybe <- gitRefToCommit (drop 5 branchP) gitDir
      curCommitHash <- case curCommitMaybe of
        Nothing -> return []
        Just hash -> case bsToHash (BSC.pack hash) of
          Nothing -> return []
          Just hashV -> return [hashV]

      -- Get all data we need to create commit object
      utcTime <- getCurrentTime
      timezone <- getCurrentTimeZone
      -- Convert timezone string in format of "-0500"
      let timezoneStr = formatTime defaultTimeLocale "%z" timezone
      let unixTS = floor (utcTimeToPOSIXSeconds utcTime) -- Unix time in seconds
      let authorInfo = ("Codey Devinson", "codey@example.com", unixTS, timezoneStr)
      let committerInfo = ("Codey Devinson", "codey@example.com", unixTS, timezoneStr)

      -- Create a new commit object based on the tree object, parent commits, and other data
      newCommitHash <- gitCommitTree treeHash curCommitHash authorInfo committerInfo message utcTime gitDir

      -- Convert gitHash to encoded string hash value
      let newCommitHashStr = BSC.unpack (getHash newCommitHash)
      -- Real git prints how many files and lines changed, but we are printing created commit
      putStrLn $ "Created commit " ++ newCommitHashStr

      -- Call updateRef to update the current branch to the new commit hash
      -- If we are in branch, move branch pointer, otherwise, move the HEAD to the new commit hash
      if take 5 branchP == "ref: "
        then gitUpdateRef (drop 5 branchP) newCommitHashStr gitDir
        else gitUpdateRef "HEAD" newCommitHashStr gitDir

-- | Unstage, (cancel add and remove all “changed” from index)
gitReset :: FilePath -> IO ()
gitReset gitDir = do
  commitHash <- gitHeadCommit gitDir
  commitObj <- hash2CommitObj commitHash gitDir
  case commitObj of
    Just (Commit (_, treeHash, _, _, _, _)) -> do
      gitReadTree (getHash treeHash) gitDir
    Nothing -> putStrLn "Error: invalid commit hash."

-- | Change the branch pointer to point commit hash
-- Return commitObject if hash is valid
-- Note: HEAD must be pointing to valid branch
gitResetSoft :: String -> FilePath -> IO (Maybe GitCommit)
gitResetSoft commit gitDir = do
  -- Update the branch pointer
  let commitHashM = bsToHash (BSC.pack commit)
  case commitHashM of
    -- Check if hash is valid
    Just commitHash -> do
      fullGitDir <- getGitDirectory gitDir
      ref <- readFile' (fullGitDir ++ "/HEAD")
      -- Check if HEAD is pointing to branch.
      if take 5 ref == "ref: "
        then do
          -- Check if hash is commit
          commitObj <- hash2CommitObj commitHash gitDir
          case commitObj of
            Just (Commit commitObj) -> do
              -- Update branch pointer
              gitUpdateRef (drop 5 ref) commit gitDir
              return (Just commitObj)
            _ -> do
              putStrLn "Error: Hash must be commit hash."
              return Nothing
        else do
          putStrLn "Error: HEAD is not pointing to branch. Make sure HEAD is on branch."
          return Nothing
    _ -> do
      putStrLn "Error: invalid hash value. Please give valid commit hash."
      return Nothing

-- | Default reset command when commit hash is given
-- Change the branch pointer and update index based on commitHsah
gitResetMixed :: String -> FilePath -> IO ()
gitResetMixed commit gitDir = do
  -- Update branch pointer
  commitObj <- gitResetSoft commit gitDir
  case commitObj of
    Just (_, treeHash, _, _, _, _) ->
      -- Update the index
      gitReadTree (getHash treeHash) gitDir
    Nothing -> return ()

-- Change the branch pointer and update index and working directory based on commitHsah
gitResetHard :: String -> FilePath -> IO ()
gitResetHard commit gitDir = do
  -- Update branch pointer
  commitObj <- gitResetSoft commit gitDir
  case commitObj of
    Just (_, treeHash, _, _, _, _) -> do
      -- Update the index and working directory
      gitReadTree (getHash treeHash) gitDir
      gitCheckoutIndex gitDir
    Nothing -> return ()

-- | Checkout the given branch or commit hash.
-- when arg is branch name, change branch pointer and checkout.
-- when arg is commit hash, let HEAD poitnt to the commit hash.
-- After updating refs, working directory will be updated to appropriate hashes.
gitCheckout :: String -> FilePath -> IO ()
gitCheckout arg gitDir = do
  let ref = "refs/heads/" ++ arg

  refHash <- gitRefToCommit ref gitDir
  case refHash of
    -- when arg is branch name
    Just commit -> do
      -- Load hash
      let commitHashM = bsToHash (BSC.pack commit)
      case commitHashM of
        Nothing -> putStrLn "Invalid hash value  found in refs. Check your refs file."
        Just commitHash -> do
          commitObj <- hash2CommitObj commitHash gitDir
          case commitObj of
            Just (Commit (_, treeHash, _, _, _, _)) -> do
              -- Move the HEAD to new branch
              gitUpdateSymbRef "HEAD" ref gitDir
              -- Update index with treeHash
              gitReadTree (getHash treeHash) gitDir
              -- Update working directory
              gitCheckoutIndex gitDir
            _ -> putStrLn "Branch is pointing to invalid hash. "

    -- When arg is hash
    Nothing -> do
      let hashM = bsToHash (BSC.pack arg)
      case hashM of
        Nothing -> putStrLn "Invalid input given. Please input a valid hash value or branch name."
        Just hash -> do
          commitObj <- hash2CommitObj hash gitDir
          case commitObj of
            Just (Commit (_, treeHash, _, _, _, _)) -> do
              -- Update HEAD to commit has
              gitUpdateRef "HEAD" (BSC.unpack (getHash hash)) gitDir
              -- Update index with treehash
              gitReadTree (getHash treeHash) gitDir
              -- Update working directory
              gitCheckoutIndex gitDir
            _ -> putStrLn "Invalid input given. Please input a valid hash value or branch name"

-- Display the contents of the git object for the given hash.
gitShow :: ByteString -> FilePath -> IO ()
gitShow hash gitDir = do
  case bsToHash hash of
    Nothing -> do
      Prelude.putStrLn "Invalid hash value given. Please input a valid hash value."
    Just hashV -> do
      gitObj <- readObjectByHash hashV gitDir
      case gitObj of
        Nothing -> return ()
        Just (gitObj, hashV) -> Prelude.putStrLn $ gitShowStr (gitObj, hashV)

-- | Get a list of parent (Git Commit) in the tree
gitParentList :: GitHash -> FilePath -> IO [GitObjectHash]
gitParentList hash gitdir = do
  obj <- hash2CommitObj hash gitdir
  case obj of
    Nothing -> return []
    Just cmt@(Commit (_, _, parents, _, _, _)) -> do
      recur <- Control.Monad.forM parents (`gitParentList` gitdir)
      return ((cmt, hash) : concat recur)
    _ -> return []

-- |
-- decompress gitObject and unpack ByteString to String
gitObjectContent :: GitHash -> FilePath -> IO String
gitObjectContent hash gitdir = do
  let hashHex = BSC.unpack (getHash hash)
  let filename = gitdir ++ "/objects/" ++ take 2 hashHex ++ "/" ++ drop 2 hashHex
  filecontent <- BSLC.readFile filename
  return (BSLC.unpack (decompress filecontent))

-- |
-- Convert the hash to Git Object of Commit (only)
hash2CommitObj :: GitHash -> FilePath -> IO (Maybe GitObject)
hash2CommitObj hash gitdir = do
  content <- gitObjectContent hash gitdir
  case parse parseGitObject "" content of
    Left error -> return Nothing
    Right gitObject -> case gitObject of
      Commit (_, _, _, _, _, _) -> return (Just gitObject)
      _ -> return Nothing

-- | a list of Commit w/ git show, start from provided hash
-- haskgit log 3154bdc4928710b08f61297e87c4900e0f9b5869
gitLog :: Maybe ByteString -> FilePath -> IO ()
gitLog hashBsM gitdir = do
  hash <- case hashBsM of
    Nothing -> gitHeadCommit gitdir
    Just hashBs -> case bsToHash hashBs of
      Nothing -> error "Invalid hash value given to gitLog. Please input a valid hash value."
      Just hash -> return hash
  parents <- gitParentList hash gitdir
  mapM_
    ( \(cmt, hs) -> do
        putStrLn $ gitShowStr (cmt, hs)
    )
    parents

-- GitIndexEntry
unpackIndex :: FilePath -> IO (Maybe GitIndex)
unpackIndex gitDir = do
  content <- BSLC.readFile $ gitDir </> "index"
  case parse parseIndexFile "" (BSLC.unpack content) of
    Left err -> return Nothing
    Right ls -> return $ Just ls

-- | list files in the index with the actual working directory list
gitListFiles :: FilePath -> IO ()
gitListFiles gitDir = do
  content <- BSLC.readFile $ gitDir </> "index"
  case parse parseIndexFile "" (BSLC.unpack content) of
    Left err -> Prelude.putStrLn $ "Parse error: " ++ show err
    Right (GitIndex ls) -> mapM_ (putStrLn . name) ls

hashBlob :: FilePath -> IO ByteString
hashBlob file = do
  content <- BSC.readFile file
  let len = BSC.length content
      header = BSC.pack $ "blob " ++ show len ++ "\0"
      hash = SHA1.hash (header `BSC.append` content)
  return hash

-- TODO in the future
hashTree :: FilePath -> IO ByteString
hashTree path = undefined

-- | used to look up hash of a path that is stored in the .git/index
-- haskgit index "test/Spec.hs"
getIndexEntry :: FilePath -> FilePath -> IO ()
getIndexEntry gitDir file = do
  ls <- unpackIndex gitDir
  case ls of
    Nothing -> putStrLn "Error: the index is unable to unpack"
    Just (GitIndex ls) -> do
      Control.Monad.forM_
        ls
        ( \e ->
            Control.Monad.when (name e == file) $ do
              print (getHash (sha e))
        )

data Status
  = Modified (FilePath, GitHash)
  | Added (FilePath, GitHash)
  | Deleted (FilePath, GitHash)
  | Untracked (FilePath, GitHash)
  deriving (Show)

gitHashObject :: FilePath -> IO ()
gitHashObject file = do
  exists <- doesPathExist file
  if exists
    then do
      isDir <- doesDirectoryExist file
      if isDir
        then hashTree file >>= putStrLn . (BSC.unpack . encode)
        else hashBlob file >>= putStrLn . (BSC.unpack . encode)
    else putStrLn $ "The path " ++ file ++ " doesn't exist"

-- | get the most updated commit
-- extract ./haskgit/HEAD to get the path that contains the commit
--   because the path would be different for different branch
gitHeadCommit :: FilePath -> IO GitHash
gitHeadCommit gitdir = do
  head <- readFile' (gitdir ++ "/HEAD")
  if take 5 head == "ref: "
    then -- HEAD is pointing to branch
    do
      commit <- gitRefToCommit (drop 5 head) gitdir
      case commit of
        -- Should never occur
        Nothing -> error "HEAD is pointing to invalid ref. Please check your .haskgit/HEAD file."
        Just cmt -> case bsToHash $ BSC.pack cmt of
          Nothing -> error "HEAD is pointing to invalid ref (incvalid hash value). Please check your .haskgit/HEAD file."
          Just hash -> return hash
    else -- HEAD is pointing to commit hash
    case bsToHash $ BSC.pack (removeCorrupts head) of
      Nothing -> error "HEAD is pointing to invalid ref (invalid hash value). Please check your .haskgit/HEAD file."
      Just hash -> return hash

-- | this function is used to convert hash of a tree into tree object
-- NOTE this one can be merged w/ hash2CommitObj
-- hash2TreeList :: GitHash -> FilePath -> IO (Maybe [(FilePath, GitHash)])
hash2TreeList :: GitHash -> FilePath -> IO (Maybe [(String, String, GitHash)])
hash2TreeList hash gitDir = do
  content <- gitObjectContent hash gitDir
  case parse parseGitObject "" content of
    Left error -> fail $ "<hash2TreeList>: " ++ show error
    Right gitObject -> case gitObject of
      Tree (_, ls) -> return (Just ls)
      _ -> return Nothing

hash2Obj :: GitHash -> FilePath -> IO (Maybe GitObject)
hash2Obj hash gitDir = do
  content <- gitObjectContent hash gitDir
  case parse parseGitObject "" content of
    Left error -> fail $ "<hash2Obj>: " ++ show error
    Right gitObject -> case gitObject of
      Tree _ -> return (Just gitObject)
      Commit _ -> return (Just gitObject)
      Blob _ -> return (Just gitObject)

-- | as there exists entry that is not in the filesystem
-- this function will skip (checking if)
-- - entry if not found (in the future, this part may be merged w/ status for deleted)
--  - it skips any directory (dir should not be appeared in index)
--  - in the test, it skips something like ".test_read_tree"
gitStatusModified :: FilePath -> IO [Status]
gitStatusModified gitDir = do
  ls <- unpackIndex gitDir
  case ls of
    Nothing -> error "Error: the index is unable to unpack"
    Just (GitIndex ls) -> do
      modified <-
        Control.Monad.filterM
          ( \e -> do
              let path = name e
              exists <- doesPathExist path
              isDir <- doesDirectoryExist path
              if exists && not isDir
                then do
                  hash <- hashBlob path
                  return $ getHash (sha e) /= encode hash
                else return False
          )
          ls
      return $ map (\i -> Modified (name i, sha i)) modified

-- |
-- ignore
--  - what is stated in .gitignore
--  - ignore the content under a new directory (just display the newDir/)
--  - ignore empty directory, or directory with sub-directory only
-- FIXME: (in the future)
--  for subdirectory like this "docs/test/test1/file"
--      return docs/test;
--      the algo for testing if the directory has file doesn't work for it.
--      it will stop at docs/test/. SEE function `getEntries`
gitStatusUntracked :: FilePath -> IO [Status]
gitStatusUntracked gitDir = do
  entries <- getEntries "."
  ls <- unpackIndex gitDir
  case ls of
    Nothing -> error "Error: the index is unable to unpack"
    Just (GitIndex ls) -> do
      let untracked = filter (not . hasFile (GitIndex ls)) entries
          skippedPath = skipNewDirContent . skipNoUpdatedDir $ untracked
      mapM untrackedS (sort skippedPath)
  where
    untrackedS p = do
      hash <- blobToHash p
      return $ Untracked (p, hash)
    -- implement it to skip something like
    -- "NewDir/wow" if exist "NewDir/" in the list
    skipNewDirContent paths =
      filter (\path -> not (any (\dir -> dir /= path && dir `isPrefixOf` path) paths)) paths
    -- skip Dir/ without file under this dir is new
    skipNoUpdatedDir paths =
      filter (\path -> last path /= '/' || any (\dir -> dir /= path && path `isPrefixOf` dir && last dir /= '/') paths) paths

gitStatusDeleted :: FilePath -> IO [Status]
gitStatusDeleted gitDir = do
  entries <- getFullEntries "."
  ls <- unpackIndex gitDir
  case ls of
    Nothing -> error "Error: the index is unable to unpack"
    Just (GitIndex ls) -> do
      let deleted = filter (\d -> all (\e -> name d /= e) entries) ls
      mapM (\p -> return $ Untracked (name p, sha p)) deleted

gitFlatTree :: FilePath -> IO [(String, FilePath, GitHash)]
gitFlatTree gitDir =
  do
    cmt <- gitHeadCommit gitDir
    obj <- hash2CommitObj cmt gitDir
    case obj of
      Nothing -> fail $ error "This Hash is failed to covert to GitObject for Commit"
      Just (Commit (_, tree, _, _, _, _)) -> do
        list <- hash2TreeList tree gitDir
        case list of
          Nothing -> error "gitFlatTree list"
          Just ls ->
            do
              result <- recTreeList "." gitDir ls
              return $ map (\(s, p, g) -> (s, makeRelative "." p, g)) result

-- | starting from the tree the HEAD points to,
-- get a full list of "blob" node in this tree and parent trees
recTreeList :: FilePath -> FilePath -> [(String, FilePath, GitHash)] -> IO [(String, FilePath, GitHash)]
recTreeList _ _ [] = return []
recTreeList parentPath gitDir (ts@(mode, path, hash) : xs) = do
  obj <- hash2Obj hash gitDir
  case obj of
    Nothing -> fail $ error "<recTreeList>"
    Just obj ->
      case obj of
        Blob (_, _) ->
          recTreeList parentPath gitDir xs
            >>= \list -> return (appendParent parentPath ts : list)
        Commit (_, _, _, _, _, _) -> recTreeList parentPath gitDir xs
        Tree (_, tr) ->
          recTreeList (parentPath </> path) gitDir tr
            >>= \list ->
              recTreeList parentPath gitDir xs
                >>= \list' -> return (list ++ list')
  where
    appendParent :: FilePath -> (String, FilePath, GitHash) -> (String, FilePath, GitHash)
    appendParent p1 (mode, ph, hs) = (mode, p1 </> ph, hs)

-- Tree

-- | used for detecting added object (on the stage /index) but not in HEAD
-- deleted: in head but not in index
-- new: in index but not in head
-- modfied: both in index and in head, but hash differs
-- it is a bit of difficult to do everything under one loop
-- this algo does two comparsions
--    entries in HEAD compare with index entries to know if any file changed or deleted

---   index entries compare with entries in HEAD to know if any file added
gitStatusStaged :: FilePath -> IO [Status]
gitStatusStaged gitDir = do
  indexL <- unpackIndex gitDir
  case indexL of
    Nothing -> fail $ error "<gitStatusStaged>: fail to unpack index"
    Just (GitIndex is) -> do
      headL <- gitFlatTree gitDir
      return (loopHeadCompareIndex headL is ++ loopIndexCompareHead headL is)
  where
    loopHeadCompareIndex :: [(String, FilePath, GitHash)] -> [GitIndexEntry] -> [Status]
    loopHeadCompareIndex [] _ = []
    loopHeadCompareIndex (h : hs) is =
      case headCompareIndex h is of
        Nothing -> loopHeadCompareIndex hs is
        Just status -> status : loopHeadCompareIndex hs is

    loopIndexCompareHead :: [(String, FilePath, GitHash)] -> [GitIndexEntry] -> [Status]
    loopIndexCompareHead [] _ = []
    loopIndexCompareHead hs [] = []
    loopIndexCompareHead hs (i : is) =
      case indexCompareHead hs i of
        Nothing -> loopIndexCompareHead hs is
        Just status -> status : loopIndexCompareHead hs is

    indexCompareHead :: [(String, FilePath, GitHash)] -> GitIndexEntry -> Maybe Status
    indexCompareHead hs i =
      if not (any (\h -> headPath h == name i) hs)
        then Just (Added (name i, sha i))
        else Nothing

    headCompareIndex :: (String, FilePath, GitHash) -> [GitIndexEntry] -> Maybe Status
    headCompareIndex h is =
      let fl = Data.List.find (\i -> name i == headPath h) is
       in case fl of
            Nothing -> Just (Deleted (headPath h, headHash h))
            Just index ->
              if sha index == headHash h
                then Nothing
                else Just (Modified (headPath h, headHash h))
    headPath (_, ph, _) = ph
    headHash (_, _, hs) = hs

gitStatus :: FilePath -> IO ()
gitStatus gitDir = do
  modified <- gitStatusModified gitDir
  untracked <- gitStatusUntracked gitDir
  deleted <- gitStatusDeleted gitDir
  staged <- gitStatusStaged gitDir

  Control.Monad.unless (null staged) $ do
    putStrLn "Staged: "
    mapM_ printStatusInfo staged

  Control.Monad.unless (null modified) $ do
    putStrLn "Modified: "
    mapM_ printStatusInfo modified

  Control.Monad.unless (null deleted) $ do
    putStrLn "Deleted: "
    mapM_ printStatusInfo deleted

  Control.Monad.unless (null untracked) $ do
    putStrLn "Untracked: "
    mapM_ printStatusInfo untracked

-- TODO for future, there are two colors (red and green) to indicate modified and deleted
--    to distinguish if the blob is staged (green for True, and red for false)
printStatusInfo :: Status -> IO ()
printStatusInfo s =
  case s of
    Modified (f, _) -> putStrLn $ "M  " ++ f
    Added (f, _) -> putStrLn $ "A  " ++ f
    Deleted (f, _) -> putStrLn $ "D  " ++ f
    -- In git, it uses two colors (red and green) of "M"
    --  in this case, use "U".
    Untracked (f, _) -> putStrLn $ "\t\t" ++ f
