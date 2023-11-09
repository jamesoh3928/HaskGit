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
    getBlobContent,
  )
where

import qualified Codec.Compression.Zlib as Zlib
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Time.Clock (UTCTime)

-- GitBlob = (file content in binary, filename)
type GitBlob = (String, String)

newBlob :: String -> String -> GitObject
newBlob content filename = Blob (content, filename)

getBlobContent :: GitObject -> String
getBlobContent (Blob (content, _)) = content
getBlobContent _ = ""

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

gitObjectToBS :: GitObject -> ByteString
-- gitObjectToBS (Blob (content, _)) = BSL.toStrict (Zlib.compress (BSLC.pack content))
gitObjectToBS (Blob (content, _)) = BSL.toStrict (BSLC.pack content)

-- gitObjectToBS obj = BSL.toStrict (Zlib.compress (BSL.fromStrict (getBlobContent obj)))