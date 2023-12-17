module Index
  ( GitIndex (..),
    GitIndexEntry (..),
    gitIndexSerialize,
    intToBytes,
    saveIndexFile,
    addOrUpdateEntries,
    hasFile,
    hasHash,
    removeEntries,
    getIndexEntryByHash,
    blobToIndexEntry,
  )
where

import Control.Monad
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as B16 (decode, encode)
import qualified Data.ByteString.Char8 as BSC
import Data.Char (chr)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GitHash (GitHash, getHash)
import GitObject (GitBlob, GitObject (..), GitTree, hashAndSaveObject)
import System.IO (readFile')
import System.Posix.Files

data GitIndexEntry = GitIndexEntry
  { ctimeS :: Int,
    ctimeNs :: Int,
    mtimeS :: Int,
    mtimeNs :: Int,
    dev :: Int,
    ino :: Int,
    modeType :: Int,
    modePerms :: Int,
    uid :: Int,
    gid :: Int,
    fsize :: Int,
    sha :: GitHash,
    flagAssumeValid :: Bool,
    flagStage :: Int,
    name :: String
  }
  deriving (Show)

newtype GitIndex = GitIndex [GitIndexEntry]
  deriving (Show)

-- Convert integer to string (opposite of parseInt function in GitParser.hs).
intToBytes :: Int -> Int -> String
intToBytes length n = reverse (aux length n)
  where
    aux 0 _ = []
    aux length 0 = replicate length '\0'
    aux length n = chr (n `mod` 256) : aux (length - 1) (n `div` 256)

-- Convert int to 4 bytes string.
intTo4Bytes :: Int -> String
intTo4Bytes = intToBytes 4

-- Convert int to 2 bytes string.
intTo2Bytes :: Int -> String
intTo2Bytes = intToBytes 2

-- Serialize GitIndexEntry to ByteString.
gitIndexEntrySerialize :: GitIndexEntry -> ByteString
gitIndexEntrySerialize entry =
  mconcat
    ( BSC.pack
        <$> [ intTo4Bytes (ctimeS entry),
              intTo4Bytes (ctimeNs entry),
              intTo4Bytes (mtimeS entry),
              intTo4Bytes (mtimeNs entry),
              intTo4Bytes (dev entry),
              intTo4Bytes (ino entry),
              "\NUL\NUL",
              -- Concat modeType and modePerms
              intTo2Bytes (modeType entry * 4096 + modePerms entry),
              intTo4Bytes (uid entry),
              intTo4Bytes (gid entry),
              intTo4Bytes (fsize entry),
              -- Decode the sha hew ByteString because index must store in binary not hex representation
              case B16.decode (getHash (sha entry)) of
                -- Should not be reachable
                Left err -> error ("Invalid hash fo base16 decoding - " ++ err ++ "\n sha entry: " ++ show (sha entry))
                Right result -> BSC.unpack result,
              -- Concat flagAssumeValid, flagStage, and nameLength
              intTo2Bytes
                ( if flagAssumeValid entry
                    then 0x8000
                    else 0x0000 + flagStage entry + length (name entry)
                ),
              -- If length is less than 0xFFF, then add null character
              if length (name entry) < 0xFFF
                then name entry
                else name entry ++ "\NUL",
              -- Padding
              replicate (8 - ((62 + length (name entry)) `mod` 8)) '\NUL'
            ]
    )

-- Serialize GitIndex to ByteString.
-- Header: 12 bytes - "DIRC" + version (4 bytes - always 2 for mvp) + number of entries (4 bytes).
gitIndexSerialize :: GitIndex -> ByteString
gitIndexSerialize (GitIndex entries) = content <> checkSum
  where
    content = BSC.pack "DIRC" <> BSC.pack "\0\0\0\2" <> BSC.pack (intTo4Bytes (length entries)) <> mconcat (fmap gitIndexEntrySerialize entries)
    -- hash the content at the end of index file
    checkSum = SHA1.hash content

-- Save the index file to {gitDir}/index
saveIndexFile :: ByteString -> FilePath -> IO ()
saveIndexFile content gitDir = do
  BSC.writeFile (gitDir ++ "/index") content

-- Check if the entry is the same file
isEntryNotSameFile :: GitIndexEntry -> FilePath -> Bool
isEntryNotSameFile entry path = name entry /= path

-- Check if the index has the file
hasFile :: GitIndex -> FilePath -> Bool
hasFile (GitIndex entries) path = any (\x -> name x == path) entries

-- Check if the index has hash
hasHash :: GitIndex -> GitHash -> Bool
hasHash (GitIndex entries) hash = any (\x -> sha x == hash) entries

-- Remove the file if it exists in the index
removeEntry :: FilePath -> GitIndex -> GitIndex
removeEntry path (GitIndex entries) = GitIndex (filter (`isEntryNotSameFile` path) entries)

-- Remove the files from the index if they exist
removeEntries :: [FilePath] -> GitIndex -> GitIndex
removeEntries paths index = foldr removeEntry index paths

-- | Add the file to the index
-- The given paths must be relative to the repository
addEntry :: FilePath -> GitIndex -> FilePath -> IO GitIndex
addEntry gitDir (GitIndex entries) path = do
  -- Get the metadata of the file (ctime_s, ctime_ns, mtime_s, mtime_ns, dev, ino, mode, uid, gid, fsize, and sha)
  metadata <- getFileStatus path
  let ctime = floor (statusChangeTimeHiRes metadata * 1e9)
      ctimeS = ctime `div` (10 ^ 9)
      ctimeNS = ctime `mod` (10 ^ 9)
      mtime = floor (modificationTimeHiRes metadata * (10 ^ 9))
      mtimeS = mtime `div` (10 ^ 9)
      mtimeNS = mtime `mod` (10 ^ 9)
      dev = fromIntegral (deviceID metadata)
      ino = fromIntegral (fileID metadata)
      mode = fromIntegral (fileMode metadata)
      uid = fromIntegral (fileOwner metadata)
      gid = fromIntegral (fileGroup metadata)
      fsize = fromIntegral (fileSize metadata)
  file <- readFile' path

  -- Use hashAndSaveObject to save the blob object as well
  let blob = Blob (0, file)
  -- TODO: double check if we can refactor somehow
  sha <- hashAndSaveObject blob gitDir

  -- -- Add the entry to the index
  return $ GitIndex (GitIndexEntry ctimeS ctimeNS mtimeS mtimeNS dev ino mode 0o100644 uid gid fsize sha False 0 path : entries)

-- | Call add entry for each path and then save the final index file
addEntries :: [FilePath] -> GitIndex -> FilePath -> IO GitIndex
addEntries paths index gitDir = foldM (addEntry gitDir) index paths

-- | Add or update given file paths to the index
-- | The given paths must be relative to the repository
addOrUpdateEntries :: [FilePath] -> GitIndex -> FilePath -> IO GitIndex
addOrUpdateEntries paths index gitDir = do
  let index' = removeEntries paths index
  addEntries paths index' gitDir

extractHashIndex :: GitIndex -> [ByteString]
extractHashIndex (GitIndex []) = []
extractHashIndex (GitIndex (x : xs)) = getHash (sha x) : extractHashIndex (GitIndex xs)

extractNameIndex :: GitIndex -> [String]
extractNameIndex (GitIndex []) = []
extractNameIndex (GitIndex (x : xs)) = name x : extractNameIndex (GitIndex xs)

getIndexEntryByHash :: GitHash -> GitIndex -> Maybe GitIndexEntry
getIndexEntryByHash _ (GitIndex []) = Nothing
getIndexEntryByHash hash (GitIndex (x : xs)) = if hash == sha x then Just x else getIndexEntryByHash hash (GitIndex xs)

-- Return a GitINdexEntry with the hash value.
-- All the metadata is set as 0 and time is set as current time
blobToIndexEntry :: GitHash -> String -> IO GitIndexEntry
blobToIndexEntry hash path = do
  time <- getPOSIXTime
  -- Since file doesn't exist
  let mtime = floor time
      ctime = mtime
      ctimeS = ctime `div` (10 ^ 9)
      ctimeNS = ctime `mod` (10 ^ 9)
      mtimeS = mtime `div` (10 ^ 9)
      mtimeNS = mtime `mod` (10 ^ 9)
      dev = 0
      ino = 0
      mode = 0
      uid = 0
      gid = 0
      fsize = 0

  return (GitIndexEntry ctimeS ctimeNS mtimeS mtimeNS dev ino mode 0o100644 uid gid fsize hash False 0 path)
