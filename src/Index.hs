module Index
  ( GitIndex (..),
    GitIndexEntry (..),
    gitIndexSerialize,
    intToBytes,
    saveIndexFile,
    addOrUpdateEntries
  )
where

import Control.Monad
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as B16 (decode)
import qualified Data.ByteString.Char8 as BSC
import Data.Char (chr)
import qualified GHC.Read as BSC
import System.Posix.Files

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
              -- Decode the sha string to ByteString
              case B16.decode (sha entry) of
                Left err -> error err
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
isEntrySameFile :: GitIndexEntry -> FilePath -> Bool
isEntrySameFile entry path = name entry == path

-- Remove the file if it exists in the index
removeEntry :: FilePath -> GitIndex -> GitIndex
removeEntry path (GitIndex entries) = GitIndex (filter (`isEntrySameFile` path) entries)

-- Remove the files from the index if they exist
removeEntries :: [FilePath] -> GitIndex -> GitIndex
removeEntries paths index = foldr removeEntry index paths

-- Add the file to the index
addEntry :: GitIndex -> FilePath -> IO GitIndex
addEntry (GitIndex entries) path = do
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
  file <- BSC.readFile path
  -- -- Hash the ("blob" + bytesize + file content)
  let sha = SHA1.hash (BSC.pack ("blob " ++ show (BSC.length file) ++ "\0") <> file)

  -- -- Add the entry to the index
  return $ GitIndex (GitIndexEntry ctimeS ctimeNS mtimeS mtimeNS dev ino mode 0o100644 uid gid fsize sha False 0 path : entries)

addEntries :: [FilePath] -> GitIndex -> IO GitIndex
-- Call add entry for each path and then save the final index file
addEntries paths index = foldM addEntry index paths

addOrUpdateEntries :: [FilePath] -> GitIndex -> IO GitIndex
-- First remove all existing entries and then add the entries
addOrUpdateEntries paths index = do
  let index' = removeEntries paths index
  addEntries paths index'