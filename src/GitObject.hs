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
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Time.Clock (UTCTime)

-- GitBlob = (byteSize, file content in binary, filename)
type GitBlob = (Int, String, String)

newBlob :: Int -> String -> String -> GitObject
newBlob byteSize content filename = Blob (byteSize, content, filename)

getBlobContent :: GitObject -> String
getBlobContent (Blob (_, content, _)) = content
getBlobContent _ = ""

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

gitObjectToBS :: GitObject -> ByteString
-- gitObjectToBS (Blob (content, _)) = BSL.toStrict (Zlib.compress (BSLC.pack content))
gitObjectToBS (Blob (byteSize, content, _)) = BSL.toStrict (BSLC.pack ("blob " ++ (show byteSize) ++ "\0" ++ content))

-- gitObjectToBS obj = BSL.toStrict (Zlib.compress (BSL.fromStrict (getBlobContent obj)))