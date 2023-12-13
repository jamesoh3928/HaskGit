module Index
  ( GitIndex (..),
    GitIndexEntry (..),
    gitIndexSerialize,
    intToBytes,
    saveIndexFile,
  )
where

import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as B16 (decode)
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr)

-- Based on the documentation: https://github.com/vaibhavsagar/duffer/blob/master/duffer/src/Duffer/Plumbing.hs
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
    sha :: ByteString,
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
    ( BC.pack
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
              -- Decode the sha string to ByteString
              case B16.decode (sha entry) of
                Left err -> error err
                Right result -> BC.unpack result,
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
    content = BC.pack "DIRC" <> BC.pack "\0\0\0\2" <> BC.pack (intTo4Bytes (length entries)) <> mconcat (fmap gitIndexEntrySerialize entries)
    -- hash the content at the end of index file
    checkSum = SHA1.hash content

-- Save the index file to {gitDir}/index
saveIndexFile :: ByteString -> FilePath -> IO ()
saveIndexFile content gitDir = do
  BC.writeFile (gitDir ++ "/index") content

-- Check if the entry is the same file
isEntrySameFile :: GitIndexEntry -> FilePath -> Bool
isEntrySameFile entry path = name entry == path

-- Remove the file if it exists in the index
removeEntry :: FilePath -> GitIndex -> GitIndex
removeEntry path (GitIndex entries) = GitIndex (filter (`isEntrySameFile` path) entries)

-- Remove the files from the index if they exist
removeEntries :: [FilePath] -> GitIndex -> GitIndex
removeEntries paths index = foldr removeEntry index paths

-- Add the file to the index
addEntry :: FilePath -> GitIndex -> IO ()
addEntry path (GitIndex entries) = undefined

-- do
-- Get the metadata of the file (ctime_s, ctime_ns, mtime_s, mtime_ns, dev, ino, mode, uid, gid, fsize, and sha)
-- metadata <- getFileMetadata path
-- Get the hash of the file
-- Read the file content

-- getIndexEntry :: FilePath -> IO GitIndexEntry
-- getIndexEntry path = do
--   -- Get the metadata of the file (ctime_s, ctime_ns, mtime_s, mtime_ns, dev, ino, mode, uid, gid, fsize, and sha)
--   metadata <- getFileMetadata path
--   -- Get the hash of the file
--   -- Read the file content

--   hash <- getHashOfFile path
--   -- Add the entry to the index
--   let newEntry = GitIndexEntry (ctimeS metadata) (ctimeNs metadata) (mtimeS metadata) (mtimeNs metadata) (dev metadata) (ino metadata) (modeType metadata) (modePerms metadata) (uid metadata) (gid metadata) (fsize metadata) hash False 0 path
--   return newEntry
