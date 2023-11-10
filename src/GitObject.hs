{-# LANGUAGE InstanceSigs #-}

module GitObject
  ( GitBlob,
    GitTree,
    -- GitNode,
    GitCommit,
    GitObject,
    GitObjectHash,
    newBlob,
    newTree,
    newCommit,
    gitObjectToBS,
    getBlobContent,
  )
where

import qualified Codec.Compression.Zlib as Zlib
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Time.Clock (UTCTime)

-- GitBlob = (byteSize, file content in binary)
type GitBlob = (Int, String)

newBlob :: Int -> String -> GitObject
newBlob byteSize content = Blob (byteSize, content)

getBlobContent :: GitObject -> String
getBlobContent (Blob (_, content)) = content
getBlobContent _ = ""

-- GitTree = (byteSize, [(filemode bits, name of file/directory, sha1 hash)])
type GitTree = (Int, [(String, String, ByteString)])

newTree :: Int -> [(String, String, ByteString)] -> GitObject
newTree byteSize elems = Tree (byteSize, elems)

-- GitAuthor = (name, email, date - unix timestamp)
type GitAuthor = (String, String, String)

-- GitCommitter = (name, email, date - unix timestamp)
type GitCommitter = (String, String, String)

-- GitCommit = (bytesize, tree hash, parent hashes, author, committer, message)
type GitCommit = (Int, ByteString, [ByteString], GitAuthor, GitCommitter, String)

data GitObject = Tree GitTree | Commit GitCommit | Blob GitBlob

newCommit :: Int -> ByteString -> [ByteString] -> GitAuthor -> GitCommitter -> String -> GitObject
newCommit bytesize tree parents authorInfo committerInfo message = Commit (bytesize, tree, parents, authorInfo, committerInfo, message)

type GitObjectHash = (GitObject, ByteString)

-- TODO: delete
-- newTree :: GitTree -> GitObject
-- newTree tree = Tree tree

-- newCommit :: GitCommit -> GitObject
-- newCommit commit = Commit commit

-----------------------------------------------
-- instance Show GitNode where
--   show :: GitNode -> String
--   show (TreeNode tree) = "TreeNode " ++ show tree
--   show (BlobNode blob) = "BlobNode " ++ show blob

instance Show GitObject where
  show :: GitObject -> String
  show (Tree tree) = "Tree " ++ show tree
  show (Blob blob) = "Blob " ++ show blob
  show (Commit commit) = "Commit " ++ show commit

gitShowStr :: GitObject -> String
gitShowStr (Blob (_, content)) = content
gitShowStr (Tree (_, _)) = "Tree"

-- gitShowStr (Tree (_, [(String, String, ByteString)], String)) = "Tree " ++ show tree
-- gitShowStr (Commit commit) = "Commit " ++ show commit

gitObjectToBS :: GitObject -> ByteString
-- gitObjectToBS (Blob (content, _)) = BSL.toStrict (Zlib.compress (BSLC.pack content))
gitObjectToBS (Blob (byteSize, content)) = BSL.toStrict (BSLC.pack ("blob " ++ show byteSize ++ "\0" ++ content))
-- (header + concatenation of Blobs and subtrees within Tree)
gitObjectToBS (Tree (byteSize, xs)) = BSL.toStrict (BSLC.pack ("tree " ++ show byteSize ++ "\0" ++ content xs))
  where
    -- tree = [(permission_bit, name, hash)]
    content :: [(String, String, ByteString)] -> String
    content [] = ""
    content [(permission_bit, name, hash)] = permission_bit ++ " " ++ name ++ "\0" ++ BS.unpack hash
    content ((permission_bit, name, hash) : xxs) = permission_bit ++ " " ++ name ++ "\0" ++ BS.unpack hash ++ content xxs
gitObjectToBS (Commit _) = empty

-- gitObjectToBS obj = BSL.toStrict (Zlib.compress (BSL.fromStrict (getBlobContent obj)))