{-# LANGUAGE InstanceSigs #-}

module GitObject
  ( GitBlob,
    GitTree,
    GitCommit,
    GitObject (..),
    GitObjectHash (..),
    GitAuthor,
    GitCommitter,
    gitObjectSerialize,
    gitShowStr,
    saveGitObject,
    hashObject,
    hashAndSaveObject,
    parseGitObject,
    readObjectByHash,
  )
where

import Codec.Compression.Zlib (compress, decompress)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString as BS (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Base16 as B16 (decode, encode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import GitHash (GitHash, bsToHash, getHash)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath
import Text.ParserCombinators.Parsec
import Text.Read (readMaybe)
import Util (formatUTCTimeWithTimeZone, hashToFilePath, unixToUTCTime)

-- | GitBlob = (byteSize, file content in binary)
type GitBlob = (Int, String)

-- | GitTree = (byteSize, [(filemode bits, name of file/directory, sha1 hash)])
-- Filemode bits are stored in octal ASCII representation for the tree.
type GitTree = (Int, [(String, String, GitHash)])

-- | GitAuthor = (name, email, date - unix time in seconds, timezone string)
type GitAuthor = (String, String, Int, String)

-- | GitCommitter = (name, email, date - unix time in seconds, timezone string)
type GitCommitter = (String, String, Int, String)

-- | GitCommit = (bytesize, tree hash, parent hashes, author, committer, message)
type GitCommit = (Int, GitHash, [GitHash], GitAuthor, GitCommitter, String)

-- | datatype GitObject: Tree, Commit, Blob
data GitObject = Tree GitTree | Commit GitCommit | Blob GitBlob

instance Show GitObject where
  show :: GitObject -> String
  show (Tree tree) = "Tree " ++ show tree
  show (Blob blob) = "Blob " ++ show blob
  show (Commit commit) = "Commit " ++ show commit

-- | (GitObject, GitHash)
type GitObjectHash = (GitObject, GitHash)

-- | with a tuple of git object and hash, get decompressed content respect to their type
-- Function that returns the string that will be used for git show command
gitShowStr :: GitObjectHash -> String
gitShowStr (Blob (_, content), _) = content
gitShowStr (Tree (_, elems), treeHash) = "tree " ++ BSC.unpack (getHash treeHash) ++ "\n\n" ++ filesDirs
  where
    filesDirs = concatMap (\(_, name, _) -> name ++ "\n") elems
gitShowStr (Commit (_, _, _, authorInfo, _, message), commitHash) = "commit " ++ BSC.unpack (getHash commitHash) ++ "\nAuthor: " ++ authorName ++ " <" ++ authorEmail ++ ">\nDate:   " ++ authorTS ++ "\n\n    " ++ message ++ "\n"
  where
    (authorName, authorEmail, authorUnixTS, authorTimeZone) = authorInfo
    authorTS = formatUTCTimeWithTimeZone authorTimeZone (unixToUTCTime (toInteger authorUnixTS))

-- | Convert the gitObject to content of object file (this function does not compress with zllib)
-- * Blob: Header + filecontent
-- * Tree: Header + concatenation of Blobs and subtrees within Tree
-- * Commit: header + concatenation of contents inside
gitObjectSerialize :: GitObject -> ByteString
gitObjectSerialize (Blob (_, content)) = BSC.pack ("blob " ++ show (length content) ++ "\0" ++ content)
gitObjectSerialize (Tree (_, xs)) = BSC.pack ("tree " ++ show (length treeContent) ++ "\0" ++ treeContent)
  where
    -- Based on GitHash invariant, decoding should never fail
    decodeHash :: GitHash -> String
    decodeHash gh = case decode (getHash gh) of
      Left err -> ""
      Right h -> BSC.unpack h
    content :: [(String, String, GitHash)] -> String
    content [] = ""
    content [(permission_bit, name, hash)] = permission_bit ++ " " ++ name ++ "\0" ++ decodeHash hash
    content ((permission_bit, name, hash) : xxs) = permission_bit ++ " " ++ name ++ "\0" ++ decodeHash hash ++ content xxs
    treeContent = content xs
gitObjectSerialize (Commit (_, treeHash, parentHashes, authorObj, committerObj, message)) = BSC.pack ("commit " ++ show (length content) ++ "\0" ++ content)
  where
    (aName, aEmail, aDate, aTimeStamp) = authorObj
    (cName, cEmail, cDate, cTimeStamp) = committerObj
    content = "tree " ++ BSC.unpack (getHash treeHash) ++ "\n" ++ concatMap ((\x -> "parent " ++ BSC.unpack x ++ "\n") . getHash) parentHashes ++ gitAuthor ++ gitCommitter ++ message
    gitAuthor = "author " ++ aName ++ " <" ++ aEmail ++ "> " ++ show aDate ++ " " ++ aTimeStamp ++ "\n"
    gitCommitter = "committer " ++ cName ++ " <" ++ cEmail ++ "> " ++ show cDate ++ " " ++ cTimeStamp ++ "\n\n"

-- | Take hash and GitObject and save it to .haskgit/objects
saveGitObject :: GitHash -> ByteString -> FilePath -> IO ()
saveGitObject hash content gitDir = do
  let obj = compress (BSC.fromStrict content)
  let path = hashToFilePath hash gitDir
  createDirectoryIfMissing True (takeDirectory path)
  BSC.writeFile path (BSC.toStrict obj)

-- | Computes the SHA-1 hash of Git objects.
hashObject :: GitObject -> GitHash
hashObject obj = case bsToHash $ encode (SHA1.hash (gitObjectSerialize obj)) of
  -- Should never happen since we are hashing ourself
  Nothing -> error "Invalid hash value was computed from SHA1.hash function"
  Just hash -> hash

-- | Computes the SHA-1 hash of Git objects and save it.
-- Returns the hash of the object that is saved if succeed.
hashAndSaveObject :: GitObject -> FilePath -> IO GitHash
hashAndSaveObject obj gitDir = do
  -- Not calling hashObject function to avoid two serializations
  let content = gitObjectSerialize obj
  -- Need to store encode hash to follow invariant of GitHash
  case bsToHash (encode $ SHA1.hash content) of
    -- Should never happen since we are hashing ourself
    Nothing -> error "Invalid hash value was computed from SHA1.hash function"
    Just hash -> do
      saveGitObject hash content gitDir
      return hash

-- Given hash value, return corresponding git object
-- Use maybe type to indicate parse failure
readObjectByHash :: GitHash -> FilePath -> IO (Maybe GitObjectHash)
readObjectByHash hash gitdir = do
  let hashHex = BSC.unpack (getHash hash)
  let filename = gitdir ++ "/objects/" ++ take 2 hashHex ++ "/" ++ drop 2 hashHex
  fileExist <- doesFileExist filename
  if fileExist
    then do
      filecontent <- BSLC.readFile filename
      case parse parseGitObject "" (BSLC.unpack (decompress filecontent)) of
        Left err -> do
          Prelude.putStrLn $ "Git parse error: " ++ show err
          return Nothing
        Right gitObj -> return (Just (gitObj, hash))
    else do
      return Nothing

-------------------------------- Parser --------------------------------

-- | Helper function to get byte size of string
byteSize :: String -> Int
byteSize s = B.length (BSC.pack s)

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
    parseGitTreeEntry :: Parser (String, String, GitHash)
    parseGitTreeEntry = do
      filemode <- manyTill digit (char ' ') :: Parser String
      filename <- manyTill anyChar (char '\0')
      -- Read 20 bytes of SHA-1 hash
      sha' <- encode . BSC.pack <$> count 20 anyChar
      return
        ( case bsToHash sha' of
            Just sha'' -> (filemode, filename, sha'')
            Nothing -> error "Invalid hash value in tree object file  found during parsing"
        )

-- | Parse the commit object.
parseCommit :: Parser GitObject
parseCommit = do
  _ <- string "commit "
  bytesizeString <- manyTill digit (char '\0')
  case readMaybe bytesizeString of
    Nothing -> fail "Not a valid byte size in commit file"
    Just bytesize -> do
      rootTree <- string "tree " >> manyTill anyChar (char '\n')
      -- There can be multiple parents (although merge conflict is not handled in mvp)
      parent <- many (string "parent " >> manyTill anyChar (char '\n'))
      authorNameWithSpace <- string "author " >> manyTill (noneOf "<") (char '<')
      authorEmail <- manyTill (noneOf ">") (char '>')
      spaces
      authorTimestamp <- manyTill digit (char ' ')
      authorTimeZone <- manyTill anyChar newline
      committerNameWithSpace <- string "committer " >> manyTill (noneOf "<") (char '<')
      committerEmail <- manyTill (noneOf ">") (char '>')
      _ <- spaces
      committerTimestamp <- manyTill digit (char ' ')
      committerTimeZone <- manyTill anyChar newline
      _ <- string "\n"
      message <- manyTill anyChar eof
      let authorInfo = (init authorNameWithSpace, authorEmail, read authorTimestamp, authorTimeZone)
          committerInfo = (init committerNameWithSpace, committerEmail, read committerTimestamp, committerTimeZone)

      let parentHashMs = Prelude.map (bsToHash . BSC.pack) parent
      let rootTreeHashM = bsToHash (BSC.pack rootTree)
      case (rootTreeHashM, hashesMtoHashes parentHashMs) of
        (Just rootTreeHash, Just parentHashes) ->
          return
            ( Commit
                ( bytesize,
                  rootTreeHash,
                  parentHashes,
                  authorInfo,
                  committerInfo,
                  message
                )
            )
        _ -> fail "Invalid hash value in commit file  found during parsing"
  where
    -- If at least one of the hashes in parentHashes or rootTreeHash is invalid, fail
    hashesMtoHashes :: [Maybe GitHash] -> Maybe [GitHash]
    hashesMtoHashes [] = Just []
    hashesMtoHashes (x : xs) = case x of
      Just x' -> case hashesMtoHashes xs of
        Just xs' -> Just (x' : xs')
        Nothing -> Nothing
      Nothing -> Nothing

-- | Parse the git object.
parseGitObject :: Parser GitObject
parseGitObject = parseBlob <|> parseTree <|> parseCommit