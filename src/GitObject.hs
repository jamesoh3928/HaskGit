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
  )
where

import Codec.Compression.Zlib (compress, decompress)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as B16 (decode, encode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import GitHash (GitHash, bsToHash, getHash)
import System.Directory (createDirectoryIfMissing)
import System.FilePath
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
  path <- hashToFilePath hash gitDir
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
