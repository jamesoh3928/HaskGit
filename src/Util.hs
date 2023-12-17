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
  )
where

import Codec.Compression.Zlib (compress, decompress)
import qualified Control.Monad as Control.Monads
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.List
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GitHash (GitHash, bsToHash, getHash)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents, listDirectory)
import System.FilePath
import System.FilePath.Glob
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
refToFilePath :: String -> FilePath -> IO FilePath
refToFilePath ref gitDir = return (gitDir ++ "/" ++ ref)

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
gitRefToCommit :: String -> FilePath -> IO (Maybe String)
gitRefToCommit ref gitDir = do
  refPath <- refToFilePath ref gitDir
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

-- | Get a list of all valid entries in current directory
--  without the speical entries . and .., empty directories, ignored entires
recurEntries :: FilePath -> IO [FilePath]
recurEntries dir = do
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      entriesCurrDir <- listDirectory dir
      entriesCurrDirS <- mapM appendSlash (filter (/= ".git") entriesCurrDir)
      nonEmptyEntries <-
        Control.Monads.filterM
          ( \f ->
              if last f == '/'
                then do hasFiles f
                else return True
          )
          entriesCurrDirS
      validEntries <- filterIgnoredPath nonEmptyEntries
      subEntries <- concat <$> mapM recurEntries validEntries
      return (validEntries ++ subEntries)
    else return [dir]
  where
    appendSlash entry = do
      let assumeDir = dir </> entry ++ "/"
      isDir <- doesDirectoryExist assumeDir
      if isDir then return $ dir </> entry ++ "/" else return $ dir </> entry

-- | get all valid entries
getEntries :: FilePath -> IO [FilePath]
getEntries dir = do
  paths <- recurEntries dir
  let relPaths = Data.List.nub $ map (makeRelative dir) paths
  return relPaths

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
  content <- readFile ".gitignore"
  return $ lines content

-- TOCHECK if it is applicable to pattern for sub-directories
ifIgnored :: FilePath -> FilePath -> Bool
ifIgnored path pattern = match (compile pattern) path

-- filter ignored Path
filterIgnoredPath :: [FilePath] -> IO [FilePath]
filterIgnoredPath ls = do
  patterns <- getGitIgnores
  return $ filter (\path -> not (any (ifIgnored path) patterns)) ls
