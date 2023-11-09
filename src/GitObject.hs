{-# LANGUAGE InstanceSigs #-}

module GitObject
  ( GitBlob,
    GitTree,
    GitNode,
    GitCommit,
    GitObject,
    GitObjectHash,
    newBlob,
  )
where

import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)

-- GitBlob = (file content in binary, filename)
type GitBlob = (String, String)

newBlob :: String -> String -> GitObject
newBlob content filename = Blob (content, filename)

-- GitNode
data GitNode = TreeNode GitTree | BlobNode GitBlob

-- GitTree = list of files and subdirectories
type GitTree = [GitNode]

-- GitCommit = (tree, parent, author, committer, message, timestamp)
newtype GitCommit = GitCommit (GitTree, Maybe [GitCommit], String, String, String, UTCTime)

data GitObject = Tree GitTree | Commit GitCommit | Blob GitBlob

type GitObjectHash = (GitObject, ByteString)

-- newTree :: GitTree -> GitObject
-- newTree tree = Tree tree

-- newCommit :: GitCommit -> GitObject
-- newCommit commit = Commit commit

-----------------------------------------------
instance Show GitNode where
  show :: GitNode -> String
  show (TreeNode tree) = "TreeNode " ++ show tree
  show (BlobNode blob) = "BlobNode " ++ show blob

instance Show GitObject where
  show :: GitObject -> String
  show (Tree tree) = "Tree " ++ show tree
  show (Blob blob) = "Blob " ++ show blob
  show (Commit (GitCommit (gt, parents, author, commiter, message, timestamp))) = "Commit " ++ "(" ++ commitStr ++ ")"
    where
      commitStr = show gt ++ ", " ++ parentStr ++ ", " ++ author ++ ", " ++ commiter ++ ", " ++ message ++ ", " ++ show timestamp
      parentStr = case parents of
        Nothing -> ""
        -- TODO: maybe we can try to print the hash of parents but current structure does not allow it
        Just _ -> "parents"