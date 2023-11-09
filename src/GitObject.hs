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
    gitObjectToBS,
  )
where

import qualified Codec.Compression.Zlib as Zlib
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Time.Clock (UTCTime)

-- GitBlob = (file content in binary, filename)
type GitBlob = (ByteString, String)

newBlob :: ByteString -> String -> GitObject
newBlob content filename = Blob (content, filename)

-- GitTree = [(permission bits, name, sha1 hash)]
type GitTree = [(String, String, ByteString)]

newTree :: GitTree -> GitObject
newTree = Tree

-- GitCommit = (tree, parent, author, committer, message, timestamp)
newtype GitCommit = GitCommit (GitTree, Maybe [ByteString], String, String, String, UTCTime)

data GitObject = Tree GitTree | Commit GitCommit | Blob GitBlob

type GitObjectHash = (GitObject, ByteString)

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
  show (Commit (GitCommit (gt, parents, author, commiter, message, timestamp))) = "Commit " ++ "(" ++ commitStr ++ ")"
    where
      commitStr = show gt ++ ", " ++ parentStr ++ ", " ++ author ++ ", " ++ commiter ++ ", " ++ message ++ ", " ++ show timestamp
      parentStr = case parents of
        Nothing -> ""
        -- TODO: maybe we can try to print the hash of parents but current structure does not allow it
        Just _ -> "parents"