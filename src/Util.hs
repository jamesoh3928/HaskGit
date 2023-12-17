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
  )
where

import Codec.Compression.Zlib (compress, decompress)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import Data.ByteString.Base16
import qualified Data.ByteString.Char8 as BSC
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GitHash (GitHash, bsToHash, getHash)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents, listDirectory)
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

-- | Returns path to .haskgit directory (climb until it finds .haskgit directory).
-- If it cannot find .haskgit directory, return "/" or "~".
getGitDirectory :: IO FilePath
getGitDirectory = do
  curr <- getCurrentDirectory
  findGitDirectory curr

-- | Given filepath, find .haskgit directory. If it cannot find .haskgit directory, return "/" or "~".
findGitDirectory :: FilePath -> IO FilePath
findGitDirectory fp = do
  if fp == "~" || fp == "/"
    then return fp
    else do
      xs <- getDirectoryContents fp
      if ".haskgit" `elem` xs
        then return (fp ++ "/.haskgit")
        else findGitDirectory (takeDirectory fp)

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
getRepoDirectory :: IO FilePath
getRepoDirectory = takeDirectory <$> getGitDirectory

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