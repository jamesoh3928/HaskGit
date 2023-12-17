module Util
  ( getGitDirectory,
    refToFilePath,
    hashToFilePath,
    gitRefToCommit,
    unixToUTCTime,
    formatUTCTimeWithTimeZone,
    relativeToAbolutePath,
    getRepoDirectory,
    removeCorrupts,
    listFilesRecursively,
    hashListFiles,
  )
where

import Codec.Compression.Zlib (compress, decompress)
import qualified Control.Monad
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import Data.ByteString.Base16
import qualified Data.ByteString.Char8 as BSC
import Data.Graph (path)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GitHash (GitHash, bsToHash, getHash)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents, listDirectory, removeDirectory)
import System.FilePath
import System.IO (readFile')

-- | Given hash value, return corresponding git directory
-- Example input: hashToFilePath "f6f754dbe0808826bed2237eb651558f75215cc6"
-- Example output: IO ".haskgit/objects/f6/f754dbe0808826bed2237eb651558f75215cc6"
hashToFilePath :: GitHash -> FilePath -> IO FilePath
hashToFilePath hash gitDir = do
  let hashStr = BSC.unpack (getHash hash)
  return (gitDir ++ "/objects/" ++ take 2 hashStr ++ "/" ++ drop 2 hashStr)

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

-- xs <- getDirectoryContents fp
-- if gitDir `elem` xs
--   then return (fp ++ "/" ++ gitDir)
--   else findGitDirectory (takeDirectory fp) gitDir

-- | Given ref, return hash of the commit object.
-- If ref is pointing to other ref, recurse it until it finds commit hash value.
-- Invariant:
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

-- | Get the directory of the repository
getRepoDirectory :: FilePath -> IO FilePath
getRepoDirectory gitDir = takeDirectory <$> getGitDirectory gitDir

hashBlob :: FilePath -> IO ByteString
hashBlob file = do
  content <- BSC.readFile file
  let len = BSC.length content
      header = BSC.pack $ "blob " ++ show len ++ "\0"
      hash = SHA1.hash (header `BSC.append` content)
  return hash

-- TODO: hash a object that is a directory
gitHashObject :: FilePath -> IO ()
gitHashObject file = do
  hash <- hashBlob file
  putStrLn $ (BSC.unpack . encode) hash

-- | Takes a directory path and returns a list of all file paths in that directory and its subdirectories
-- | Returns "full" path, not relative
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
              -- putStrLn ("isDirectory" ++ show files)
              listFilesOfDirectories files path
            else return []
  where
    listFilesOfDirectories :: [FilePath] -> FilePath -> IO [FilePath]
    listFilesOfDirectories [] _ = return []
    listFilesOfDirectories (x : xs) path = do
      res <- listFilesRecursively (path ++ "/" ++ x) gitDir
      rest <- listFilesOfDirectories xs path
      return (res ++ rest)

blobToHash :: FilePath -> IO GitHash
blobToHash file = do
  -- cont <- readFile' file
  content <- BSC.readFile file
  -- let content = BSC.pack cont
  let len = BSC.length content
      header = BSC.pack $ "blob " ++ show len ++ "\0"
      hash = SHA1.hash (header `BSC.append` content)
  return (bsToHash hash)

hashListFiles :: [FilePath] -> IO [GitHash]
hashListFiles [] = return []
hashListFiles (x : xs) = do
  hash <- blobToHash x
  rest <- hashListFiles xs
  return (hash : rest)