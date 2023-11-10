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
import Text.Read (Lexeme (String))

-- GitBlob = (byteSize, file content in binary, blobHash)
type GitBlob = (Int, String, String)

newBlob :: Int -> String -> String -> GitObject
newBlob byteSize content blobHash = Blob (byteSize, content, blobHash)

getBlobContent :: GitObject -> String
getBlobContent (Blob (_, content, _)) = content
getBlobContent _ = ""

-- GitTree = (byteSize, [(filemode bits, name, sha1 hash)], filename)
type GitTree = (Int, [(String, String, ByteString)], String)

newTree :: Int -> [(String, String, ByteString)] -> String -> GitObject
newTree byteSize elems treeHash = Tree (byteSize, elems, treeHash)

-- GitAuthor = (name, email, date - unix timestamp)
type GitAuthor = (String, String, String)

-- GitCommitter = (name, email, date - unix timestamp)
type GitCommitter = (String, String, String)

-- GitCommit = (bytesize, tree hash, parent hashes, author, committer, message)
type GitCommit = (Int, ByteString, [ByteString], GitAuthor, GitCommitter, String, String)

data GitObject = Tree GitTree | Commit GitCommit | Blob GitBlob

newCommit :: Int -> ByteString -> [ByteString] -> GitAuthor -> GitCommitter -> String -> String -> GitObject
newCommit bytesize tree parents authorInfo committerInfo message commitHash = Commit (bytesize, tree, parents, authorInfo, committerInfo, message, commitHash)

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
gitShowStr (Blob blob) = "Blob " ++ show blob
gitShowStr (Tree tree) = "Tree " ++ show tree
gitShowStr (Commit commit) = "Commit " ++ show commit

gitObjectToBS :: GitObject -> ByteString
-- gitObjectToBS (Blob (content, _)) = BSL.toStrict (Zlib.compress (BSLC.pack content))
gitObjectToBS (Blob (byteSize, content, _)) = BSL.toStrict (BSLC.pack ("blob " ++ show byteSize ++ "\0" ++ content))
-- (header + concatenation of Blobs and subtrees within Tree)
gitObjectToBS (Tree (byteSize, xs, _)) = BSL.toStrict (BSLC.pack ("tree " ++ show byteSize ++ "\0" ++ content xs))
  where
    content :: [(String, String, ByteString)] -> String
    content [] = ""
    content [(permission_bit, name, hash)] = permission_bit ++ " " ++ name ++ "\0" ++ BS.unpack hash
    content ((permission_bit, name, hash) : xxs) = permission_bit ++ " " ++ name ++ "\0" ++ BS.unpack hash ++ content xxs
-- GitCommit = (tree hash, parent hashes, author, committer, message)
-- newtype GitCommit = GitCommit (ByteString, [ByteString], GitAuthor, GitCommitter, String, String)
gitObjectToBS (Commit (byteSize, treeHash, parentHashes, (aName, aEmail, aDate), committer, message)) = BSL.toStrict (BSLC.pack ("commit " ++ show byteSize ++ "\0" ++ content))
  where
    content :: String
    content = "tree " ++ BS.unpack treeHash ++ "\n" ++ concatMap (\x -> "parent " ++ BS.unpack x ++ "\n") parentHashes ++ "author " ++ 

-- gitObjectToBS obj = BSL.toStrict (Zlib.compress (BSL.fromStrict (getBlobContent obj)))