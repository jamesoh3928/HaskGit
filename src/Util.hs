module Util
  ( getGitDirectory,
    refToFilePath,
    hashToFilePath,
    gitRefToCommit,
    unixToUTCTime,
    formatUTCTimeWithTimeZone,
    relativeToAbolutePath,
    getRepoDirectory,
    getEntries,
    getFullEntries,
    removeCorrupts,
    listFilesRecursively,
    hashAndNameFiles,
    blobToHash,
  )
where

import Codec.Compression.Zlib (compress, decompress)
import qualified Control.Monad
import qualified Control.Monad as Control.Monads
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import Data.ByteString.Base16
import qualified Data.ByteString.Char8 as BSC
import Data.Graph (path)
import Data.List
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GitHash (GitHash, bsToHash, getHash)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents, listDirectory, removeDirectory)
import System.FilePath
import System.FilePath.Glob
import System.IO (readFile')

-- | Given hash value, return corresponding git directory
-- Example input: hashToFilePath "f6f754dbe0808826bed2237eb651558f75215cc6"
-- Example output: IO ".haskgit/objects/f6/f754dbe0808826bed2237eb651558f75215cc6"
hashToFilePath :: GitHash -> FilePath -> FilePath
hashToFilePath hash gitDir = gitDir ++ "/objects/" ++ take 2 hashStr ++ "/" ++ drop 2 hashStr
  where
    hashStr = BSC.unpack (getHash hash)

-- | Returns path to reference
-- Example input: refToFilePath refs/heads/main
-- Example output: ".haskgit/refs/heads/main"
refToFilePath :: String -> FilePath -> FilePath
refToFilePath ref gitDir = gitDir ++ "/" ++ ref

-- | Returns path to git directory (climb until it finds git directory).
-- If it cannot find git directory, return "/" or "~".
getGitDirectory :: FilePath -> IO FilePath
getGitDirectory gitDir = do
  curr <- getCurrentDirectory
  findGitDirectory curr gitDir

-- | Given filepath, find git directory. If it cannot find git directory, return "/" or "~".
findGitDirectory :: FilePath -> FilePath -> IO FilePath
findGitDirectory fp gitDir = do
  if fp == "~" || fp == "/"
    then return fp
    else do
      isGitDir <- doesDirectoryExist (fp ++ "/" ++ gitDir)
      if isGitDir then return (fp ++ "/" ++ gitDir) else findGitDirectory (takeDirectory fp) gitDir

-- | Given ref, return hash of the commit object.
-- If ref is pointing to other ref, recurse it until it finds commit hash value.
-- Invariants:
--  arg1: valid git reference which are either refs or HEAD. (e.g. refs/heads/main, HEAD)
--  arg2: valid git directory path (.git, .haskgit)
gitRefToCommit :: String -> FilePath -> IO (Maybe String)
gitRefToCommit ref gitDir = do
  let refPath = refToFilePath (removeCorrupts ref) gitDir
  fileExist <- doesFileExist refPath
  if fileExist
    then do
      content <- readFile' refPath
      let obj = head (lines content)
      if take 5 obj == "ref: "
        then gitRefToCommit (drop 5 obj) gitDir
        else return (Just obj)
    else do
      return Nothing

-- Remove all characters that should not exist in the branch name
removeCorrupts :: [Char] -> [Char]
removeCorrupts = filter (not . isEscape)
  where
    isEscape c = c == '\n' || c == '\r'

-- | Convert unix time integer value to UTCTime
unixToUTCTime :: Integer -> UTCTime
unixToUTCTime unixTime = posixSecondsToUTCTime $ fromInteger unixTime

-- | Format UTCTime with timezone offset
formatUTCTimeWithTimeZone :: String -> UTCTime -> String
formatUTCTimeWithTimeZone timezoneOffset utcTime = formatTime defaultTimeLocale "%a %b %-d %T %Y " utcTimeWithTZ ++ timezoneOffset
  where
    -- Parse the timezone offset from the format "-0500"
    hours = read (take 3 timezoneOffset) :: Int
    minutes = read (drop 3 timezoneOffset) :: Int
    timezoneMinutes = hours * 60 + minutes
    utcTimeWithTZ = utcTime {utctDayTime = utctDayTime utcTime + fromIntegral (timezoneMinutes * 60)}

-- | Convert a relative path to an absolute path
relativeToAbolutePath :: FilePath -> IO FilePath
relativeToAbolutePath relativePath = do
  currentDir <- getCurrentDirectory
  return $ normalise $ currentDir </> relativePath

-- | Get a list of all valid entries in current directory
--  without the speical entries . and .., empty directories, ignored entires
recurEntries :: FilePath -> FilePath -> IO [FilePath]
recurEntries dir gitDir = do
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      entriesCurrDir <- listDirectory dir
      entriesCurrDirS <- mapM (appendSlash dir) (filter (\x -> x /= gitDir && x /= ".git") entriesCurrDir)
      nonEmptyEntries <-
        Control.Monads.filterM
          ( \f ->
              if last f == '/'
                then do hasFiles f
                else return True
          )
          entriesCurrDirS
      validEntries <- filterIgnoredPath nonEmptyEntries gitDir
      concat <$> mapM (`recurEntries` gitDir) validEntries
    else return [dir]

-- | get all valid entries
getEntries :: FilePath -> FilePath -> IO [FilePath]
getEntries dir gitDir = do
  paths <- recurEntries dir gitDir
  let relPaths = Data.List.nub $ map (makeRelative dir) paths
  return relPaths

fullRecurEntries :: FilePath -> FilePath -> IO [FilePath]
fullRecurEntries dir gitDir = do
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      entriesCurrDir <- listDirectory dir
      entriesCurrDirS <- mapM (appendSlash dir) (filter (\x -> x /= ".git" && x /= gitDir) entriesCurrDir)
      concat <$> mapM (`fullRecurEntries` gitDir) entriesCurrDirS
    else return [dir]

getFullEntries :: FilePath -> FilePath -> IO [FilePath]
getFullEntries dir gitDir = do
  paths <- fullRecurEntries dir gitDir
  return $ map (makeRelative dir) (Data.List.nub paths)

appendSlash :: [Char] -> [Char] -> IO FilePath
appendSlash dir entry = do
  let assumeDir = dir </> entry ++ "/"
  isDir <- doesDirectoryExist assumeDir
  if isDir then return $ dir </> entry ++ "/" else return $ dir </> entry

-- | git only works for files (not directory)
hasFiles :: FilePath -> IO Bool
hasFiles dir = do
  entries <- listDirectory dir
  let paths = map (dir ++) entries
  results <- mapM checkPath paths
  return $ or results
  where
    checkPath path = do
      isDir <- doesDirectoryExist path
      if isDir
        then hasFiles path
        else doesFileExist path

-- | get pattern list for ignoring file/dir
getGitIgnores :: IO [FilePath]
getGitIgnores = do
  isIgnore <- doesFileExist ".gitignore"
  if isIgnore
    then do
      content <- readFile ".gitignore"
      return $ lines content
    else return []

ifIgnored :: FilePath -> FilePath -> Bool
ifIgnored path pattern = match (compile pattern) path

-- filter ignored Path
filterIgnoredPath :: [FilePath] -> FilePath -> IO [FilePath]
filterIgnoredPath ls gitDir = do
  patterns <- getGitIgnores
  return $ filter (\path -> not (any (ifIgnored path) patterns)) ls

getRepoDirectory :: FilePath -> IO FilePath
getRepoDirectory gitDir = takeDirectory <$> getGitDirectory gitDir

-- | Hash a blob file in given path.
hashBlob :: FilePath -> IO ByteString
hashBlob file = do
  content <- BSC.readFile file
  let len = BSC.length content
      header = BSC.pack $ "blob " ++ show len ++ "\0"
      hash = SHA1.hash (header `BSC.append` content)
  return hash

-- | Print the hash of a blob file in given path.
gitHashObject :: FilePath -> IO ()
gitHashObject file = do
  hash <- hashBlob file
  putStrLn $ (BSC.unpack . encode) hash

-- | Takes a directory path and returns a list of all file paths in that directory and its subdirectories
-- Returns "full" path, not relative
listFilesRecursively :: FilePath -> FilePath -> IO [FilePath]
listFilesRecursively path gitDir = do
  if path == gitDir
    then return []
    else do
      isFile <- doesFileExist path
      isDirectory <- doesDirectoryExist path
      if isFile
        then do
          return [path]
        else
          if isDirectory
            then do
              xs <- listDirectory path
              -- Exclude gitdir
              let files = filter (/= gitDir) xs
              listFilesOfDirectories files path
            else return []
  where
    listFilesOfDirectories :: [FilePath] -> FilePath -> IO [FilePath]
    listFilesOfDirectories [] _ = return []
    listFilesOfDirectories (x : xs) path = do
      res <- listFilesRecursively (path ++ "/" ++ x) gitDir
      rest <- listFilesOfDirectories xs path
      return (res ++ rest)

-- | Take a path to blob file and return a hash of the blob file
blobToHash :: FilePath -> IO GitHash
blobToHash file = do
  -- cont <- readFile' file
  content <- BSC.readFile file
  -- let content = BSC.pack cont
  let len = BSC.length content
      header = BSC.pack $ "blob " ++ show len ++ "\0"
      hash = case bsToHash $ encode (SHA1.hash (header `BSC.append` content)) of
        Just h -> h
        -- Should never happen since we are hashing a blob ourselves
        Nothing -> error "Invalid hash value was computed from SHA1.hash function in blobToHash"
  return hash

-- | Take a list of file paths and return a list of hashes of the blob files
hashAndNameFiles :: [FilePath] -> IO [(GitHash, FilePath)]
hashAndNameFiles [] = return []
hashAndNameFiles (x : xs) = do
  hash <- blobToHash x
  rest <- hashAndNameFiles xs
  return ((hash, x) : rest)
