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
    extractNameIndex,
    extractHashesIndex,
    updateMetaData,
    parseIndexFile,
  )
where

import Control.Monad
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as B16 (decode, encode)
import qualified Data.ByteString.Char8 as BSC
import Data.Char (chr, ord)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GitHash (GitHash, bsToHash, getHash)
import GitObject (GitBlob, GitObject (..), GitTree, hashAndSaveObject, hashObject)
import System.IO (readFile')
import System.Posix.Files
import Text.ParserCombinators.Parsec

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

-- | Convert integer to string (opposite of parseInt function in GitParser.hs).
intToBytes :: Int -> Int -> String
intToBytes length n = reverse (aux length n)
  where
    aux 0 _ = []
    aux length 0 = replicate length '\0'
    aux length n = chr (n `mod` 256) : aux (length - 1) (n `div` 256)

-- | Convert int to 4 bytes string.
intTo4Bytes :: Int -> String
intTo4Bytes = intToBytes 4

-- | Convert int to 2 bytes string.
intTo2Bytes :: Int -> String
intTo2Bytes = intToBytes 2

-- | Serialize GitIndexEntry to ByteString.
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

-- | Serialize GitIndex to ByteString.
-- Header: 12 bytes - "DIRC" + version (4 bytes - always 2 for mvp) + number of entries (4 bytes).
gitIndexSerialize :: GitIndex -> ByteString
gitIndexSerialize (GitIndex entries) = content <> checkSum
  where
    content = BSC.pack "DIRC" <> BSC.pack "\0\0\0\2" <> BSC.pack (intTo4Bytes (length entries)) <> mconcat (fmap gitIndexEntrySerialize entries)
    -- hash the content at the end of index file
    checkSum = SHA1.hash content

-- | Save the index file to {gitDir}/index
saveIndexFile :: ByteString -> FilePath -> IO ()
saveIndexFile content gitDir = do
  BSC.writeFile (gitDir ++ "/index") content

-- | Check if the entry is the same file
isEntryNotSameFile :: GitIndexEntry -> FilePath -> Bool
isEntryNotSameFile entry path = name entry /= path

-- | Check if the index has the file
hasFile :: GitIndex -> FilePath -> Bool
hasFile (GitIndex entries) path = any (\x -> name x == path) entries

-- | Check if the index has hash
hasHash :: GitIndex -> GitHash -> Bool
hasHash (GitIndex entries) hash = any (\x -> sha x == hash) entries

-- | Remove the file if it exists in the index
removeEntry :: FilePath -> GitIndex -> GitIndex
removeEntry path (GitIndex entries) = GitIndex (filter (`isEntryNotSameFile` path) entries)

-- | Remove the files from the index if they exist
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
  sha <- hashAndSaveObject blob gitDir

  -- -- Add the entry to the index
  return $ GitIndex (GitIndexEntry ctimeS ctimeNS mtimeS mtimeNS dev ino mode 0o100644 uid gid fsize sha False 0 path : entries)

-- | Call add entry for each path and then save the final index file
addEntries :: [FilePath] -> GitIndex -> FilePath -> IO GitIndex
addEntries paths index gitDir = foldM (addEntry gitDir) index paths

-- | Add or update given file paths to the index
-- The given paths must be relative to the repository
addOrUpdateEntries :: [FilePath] -> GitIndex -> FilePath -> IO GitIndex
addOrUpdateEntries paths index gitDir = do
  let index' = removeEntries paths index
  addEntries paths index' gitDir

-- | Extract the hash values (in bytestring forms) from the index
extractHashesIndex :: GitIndex -> [GitHash]
extractHashesIndex (GitIndex []) = []
extractHashesIndex (GitIndex (x : xs)) = sha x : extractHashesIndex (GitIndex xs)

-- | Extract the file names from the index
extractNameIndex :: GitIndex -> [String]
extractNameIndex (GitIndex []) = []
extractNameIndex (GitIndex (x : xs)) = name x : extractNameIndex (GitIndex xs)

-- | Get the index entry by hash value
getIndexEntryByHash :: GitHash -> GitIndex -> Maybe GitIndexEntry
getIndexEntryByHash _ (GitIndex []) = Nothing
getIndexEntryByHash hash (GitIndex (x : xs)) = if hash == sha x then Just x else getIndexEntryByHash hash (GitIndex xs)

-- | Return a GitINdexEntry with the hash value.
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

-- | Returns new index with updated meta data
updateMetaData :: GitIndex -> FilePath -> FilePath -> IO GitIndex
updateMetaData (GitIndex []) _ _ = return (GitIndex [])
updateMetaData (GitIndex (x : xs)) repoDir gitDir = do
  let fp = repoDir ++ "/" ++ name x
  metadata <- getFileStatus fp
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
  file <- readFile' fp
  -- Use hashAndSaveObject to save the blob object as well
  let blob = Blob (0, file)
  let sha = hashObject blob

  (GitIndex rest) <- updateMetaData (GitIndex xs) repoDir gitDir
  -- -- Add the entry to the index
  return $ GitIndex (GitIndexEntry ctimeS ctimeNS mtimeS mtimeNS dev ino mode 0o100644 uid gid fsize sha False 0 (name x) : rest)

--------------------------------- Parser ---------------------------------

-- | Parse n-byte integer in network byte order (big-endian)
parseInt :: Int -> Parser Int
parseInt n = do
  ints <- (ord <$>) <$> count n anyChar
  -- Loop through int list and get actualy decimal value
  return (foldl (\v x -> v * 256 + x) 0 ints)

-- | Parse 4-byte integer in network byte order (big-endian)
parseInt32 :: Parser Int
parseInt32 = parseInt 4

-- | Parse 2-byte integer in network byte order (big-endian)
parseInt16 :: Parser Int
parseInt16 = parseInt 2

-- | Parse a Git index entry (for mvp, assuming version 2)
-- Followed git index format documentation: https://github.com/git/git/blob/master/Documentation/gitformat-index.txt
parseGitIndexEntry :: Parser GitIndexEntry
parseGitIndexEntry = do
  ctimeS' <- parseInt32
  ctimeNs' <- parseInt32
  mtimeS' <- parseInt32
  mtimeNs' <- parseInt32
  dev' <- parseInt32
  ino' <- parseInt32
  _ <- count 2 (char '\NUL') -- Ignored
  mode <- parseInt16
  let modeType' = shiftR mode 12
      -- 9-bit unix permission. Only 0755 and 0644 are valid for regular files.
      modePerms' = mode .&. 0x01FF
  uid' <- parseInt32
  gid' <- parseInt32
  fsize' <- parseInt32
  -- Encode it to hexadecimal representation because index is storing not encoded hash
  shaBS <- B16.encode . BSC.pack <$> count 20 anyChar
  let sha' = case bsToHash shaBS of
        Just sha'' -> sha''
        Nothing -> error "Invalid hash value in index file found during parsing"
  flags <- parseInt16
  let flagAssumeValid' = flags .&. 0x8000 /= 0
      -- flagExtended, ignoring for mvp
      _ = flags .&. 0x4000 /= 0
      flagStage' = flags .&. 0x3000
      nameLength = flags .&. 0x0FFF

  -- If version > 2, there is another 16 bit but skipping for MVP

  name' <-
    if nameLength < 0xFFF
      then do
        -- _ <- char '\NUL' -- Ensure termination with null character
        count nameLength anyChar
      else do
        -- _ <- string "\NUL\NUL\NUL\NUL" -- Ensure termination with four null characters
        manyTill anyChar (char '\NUL')

  -- If version == 4, there is extra data but skipping for MVP

  -- Skip padding so we can start at the right bit for next entry
  -- Data is padded on multiples of eight bytes for pointer
  -- alignment, so we skip as many bytes as we need for the next
  -- read to start at the right position
  let padding = 8 - ((62 + Prelude.length name') `mod` 8)
  _ <- count padding anyChar

  return $
    GitIndexEntry
      ctimeS'
      ctimeNs'
      mtimeS'
      mtimeNs'
      dev'
      ino'
      modeType'
      modePerms'
      uid'
      gid'
      fsize'
      sha'
      flagAssumeValid'
      flagStage'
      name'

-- | Parse index file (which is in binary format)
-- Ignoring the extension data for now
parseIndexFile :: Parser GitIndex
parseIndexFile = do
  -- 4-byte signature (DIRC)
  _ <- string "DIRC"
  -- 4-byte version number (our mvp only takes version 2)
  _ <- string "\0\0\0\2"
  numEntries <- parseInt32
  entries <- count numEntries parseGitIndexEntry
  -- Ignoring the extensions and cache tree
  return (GitIndex entries)
