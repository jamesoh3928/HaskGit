module GitParser
  ( parseGitObject,
    parseInt32,
    parseIndexFile,
    readObjectByHash,
    parseTree,
  )
where

import Codec.Compression.Zlib (decompress)
import Data.Bits
import Data.ByteString as B (ByteString, length)
import Data.ByteString.Base16 as B16 (encode)
import Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Char (ord)
import GitHash (GitHash, bsToHash, gitHashValue)
import GitObject (GitObject (..), GitObjectHash)
import Index (GitIndex (..), GitIndexEntry (..))
import Text.ParserCombinators.Parsec
import Text.Read (readMaybe)

-- | Helper function to get byte size of string
byteSize :: String -> Int
byteSize s = B.length (BC.pack s)

-- | Parse the blob object.
parseBlob :: Parser GitObject
parseBlob = do
  _ <- string "blob "
  bytesizeString <- manyTill digit (char '\0')
  case readMaybe bytesizeString of
    Nothing -> fail "Not a valid byte size in blob file"
    Just bytesize -> do
      content <- many anyChar
      -- data integrity check
      if bytesize /= byteSize content
        then fail "Byte size does not match in blob file"
        else return (Blob (bytesize, content))

-- | Parse the tree object from the binary object (Tree is binary object unlike commit and blob).
parseTree :: Parser GitObject
parseTree = do
  _ <- string "tree "
  bytesizeString <- manyTill digit (char '\0')
  case readMaybe bytesizeString of
    Nothing -> fail "Not a valid byte size in tree file"
    Just bytesize -> do
      elems <- manyTill parseGitTreeEntry eof
      return (Tree (bytesize, elems))
  where
    parseGitTreeEntry :: Parser (String, String, ByteString)
    parseGitTreeEntry = do
      filemode <- manyTill digit (char ' ') :: Parser String
      filename <- manyTill anyChar (char '\0')
      -- Read 20 bytes of SHA-1 hash
      sha' <- BC.pack <$> count 20 anyChar
      return (filemode, filename, sha')

-- | Parse the commit object.
parseCommit :: Parser GitObject
parseCommit = do
  _ <- string "commit "
  bytesizeString <- manyTill digit (char '\0')
  case readMaybe bytesizeString of
    Nothing -> fail "Not a valid byte size in commit file"
    Just bytesize -> do
      _ <- string "tree "
      rootTree <- manyTill anyChar (char '\n')
      --   Assuming only one parent for MVP
      _ <- string "parent "
      parent <- manyTill anyChar (char '\n')
      _ <- string "author "
      authorNameWithSpace <- manyTill (noneOf "<") (char '<')
      authorEmail <- manyTill (noneOf ">") (char '>')
      spaces
      authorTimestamp <- manyTill digit (char ' ')
      authorTimeZone <- manyTill anyChar newline
      _ <- string "committer "
      committerNameWithSpace <- manyTill (noneOf "<") (char '<')
      committerEmail <- manyTill (noneOf ">") (char '>')
      _ <- spaces
      committerTimestamp <- manyTill digit (char ' ')
      committerTimeZone <- manyTill anyChar newline
      _ <- string "\n"
      message <- manyTill anyChar eof
      let authorInfo = (init authorNameWithSpace, authorEmail, read authorTimestamp, authorTimeZone)
          committerInfo = (init committerNameWithSpace, committerEmail, read committerTimestamp, committerTimeZone)
      case (gitHashValue (BC.pack rootTree), gitHashValue (BC.pack parent)) of
        (Just rootTreeHash, Just parentHash) ->
          return
            ( Commit
                ( bytesize,
                  rootTreeHash,
                  [parentHash],
                  authorInfo,
                  committerInfo,
                  message
                )
            )
        _ -> fail "Invalid hash value in commit file"

-- | Parse the git object.
parseGitObject :: Parser GitObject
parseGitObject = parseBlob <|> parseTree <|> parseCommit

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
  shaBS <- B16.encode . BC.pack <$> count 20 anyChar
  let sha' = bsToHash shaBS
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

-- Given hash value, return corresponding git object
-- Use maybe type to indicate parse failure
readObjectByHash :: ByteString -> FilePath -> IO (Maybe GitObjectHash)
readObjectByHash hash gitdir = do
  case gitHashValue hash of
    Nothing -> do
      Prelude.putStrLn "Invalid hash value given."
      return Nothing
    Just hashV -> do
      -- 2 hexadecimal = 4 bytes
      let hashHex = BSC.unpack hash
      let filename = gitdir ++ "/objects/" ++ take 2 hashHex ++ "/" ++ drop 2 hashHex
      filecontent <- BSLC.readFile filename
      case parse parseGitObject "" (BSLC.unpack (decompress filecontent)) of
        Left err -> do
          Prelude.putStrLn $ "Git parse error: " ++ show err
          return Nothing
        Right gitObj -> return (Just (gitObj, hashV))