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
  )
where

import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)

-- GitBlob = (byteSize, file content in binary, filename)
type GitBlob = (Int, ByteString, String)

newBlob :: Int -> ByteString -> String -> GitObject
newBlob byteSize content filename = Blob (byteSize, content, filename)

-- GitTree = (byteSize, [(permission bits, name, sha1 hash)])
type GitTree = (Int, [(String, String, ByteString)])

newTree :: Int -> [(String, String, ByteString)] -> GitObject
newTree byteSize elems = Tree (byteSize, elems)

-- GitCommit = (tree hash, parent hashes, author, committer, message)
newtype GitCommit = GitCommit (ByteString, [ByteString], String, String, String, UTCTime)
  deriving (Show)

data GitObject = Tree GitTree | Commit GitCommit | Blob GitBlob

newCommit :: ByteString -> [ByteString] -> String -> String -> String -> UTCTime -> GitObject
newCommit tree parents author commiter message timestamp = Commit (GitCommit (tree, parents, author, commiter, message, timestamp))

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

-- TODO: delete
-- show (Commit (GitCommit (gt, parents, author, commiter, message, timestamp))) = "Commit " ++ "(" ++ commitStr ++ ")"
--   where
--     commitStr = show gt ++ ", " ++ show parents ++ ", " ++ author ++ ", " ++ commiter ++ ", " ++ message ++ ", " ++ show timestamp