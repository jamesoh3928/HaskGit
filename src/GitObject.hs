{-# LANGUAGE InstanceSigs #-}

module GitObject
  ( GitBlob,
    GitTree,
    GitCommit,
    GitObject (..),
    GitObjectHash (..),
    gitObjectSerialize,
    gitShowStr,
    saveGitObject,
  )
where

import Codec.Compression.Zlib (compress, decompress)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as B16 (decode, encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import GitHash (GitHash, getHash)
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import Util (formatUTCTimeWithTimeZone, hashToFilePath, unixToUTCTime)

-- | GitBlob = (byteSize, file content in binary)
type GitBlob = (Int, String)

-- | GitTree = (byteSize, [(filemode bits, name of file/directory, sha1 hash)])
type GitTree = (Int, [(String, String, ByteString)])

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
gitShowStr (Tree (_, elems), treeHash) = "tree " ++ B.unpack (getHash treeHash) ++ "\n\n" ++ filesDirs
  where
    filesDirs = concatMap (\(_, name, _) -> name ++ "\n") elems
gitShowStr (Commit (_, _, _, authorInfo, _, message), commitHash) = "commit " ++ B.unpack (getHash commitHash) ++ "\nAuthor: " ++ authorName ++ " <" ++ authorEmail ++ ">\nDate:   " ++ authorTS ++ "\n\n    " ++ message
  where
    (authorName, authorEmail, authorUnixTS, authorTimeZone) = authorInfo
    authorTS = formatUTCTimeWithTimeZone authorTimeZone (unixToUTCTime (toInteger authorUnixTS))

-- | Convert the gitObject to content of object file (this function does not compress with zllib)
-- * Blob: Header + filecontent
-- * Tree: Header + concatenation of Blobs and subtrees within Tree
-- * Commit: header + concatenation of contents inside
gitObjectSerialize :: GitObject -> ByteString
gitObjectSerialize (Blob (byteSize, content)) = BSC.pack ("blob " ++ show byteSize ++ "\0" ++ content)
gitObjectSerialize (Tree (byteSize, xs)) = BSC.pack ("tree " ++ show byteSize ++ "\0" ++ content xs)
  where
    decodeHash :: ByteString -> ByteString
    decodeHash hash = case B16.decode hash of
      Left err -> error err
      Right x -> x
    content :: [(String, String, ByteString)] -> String
    content [] = ""
    content [(permission_bit, name, hash)] = permission_bit ++ " " ++ name ++ "\0" ++ (BSC.unpack . decodeHash) hash
    content ((permission_bit, name, hash) : xxs) = permission_bit ++ " " ++ name ++ "\0" ++ (BSC.unpack . decodeHash) hash ++ content xxs
gitObjectSerialize (Commit (byteSize, treeHash, parentHashes, authorObj, committerObj, message)) = BSC.pack ("commit " ++ show byteSize ++ "\0" ++ content)
  where
    (aName, aEmail, aDate, aTimeStamp) = authorObj
    (cName, cEmail, cDate, cTimeStamp) = committerObj
    content = "tree " ++ BSC.unpack (getHash treeHash) ++ "\n" ++ concatMap ((\x -> "parent " ++ BSC.unpack x ++ "\n") . getHash) parentHashes ++ gitAuthor ++ gitCommitter ++ message
    gitAuthor = "author " ++ aName ++ " <" ++ aEmail ++ "> " ++ show aDate ++ " " ++ aTimeStamp ++ "\n"
    gitCommitter = "committer " ++ cName ++ " <" ++ cEmail ++ "> " ++ show cDate ++ " " ++ cTimeStamp ++ "\n\n"

-- | Take hash and GitObject and save it to .haskgit/objects
saveGitObject :: GitHash -> ByteString -> FilePath -> IO ()
saveGitObject hash content gitDir = do
  let obj = compress (BSLC.fromStrict content)
  path <- hashToFilePath (B.unpack (getHash hash)) gitDir
  createDirectoryIfMissing True (takeDirectory path)
  BSLC.writeFile path obj
