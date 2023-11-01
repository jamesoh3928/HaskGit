module GitObject
  ( GitBlob,
    GitTree,
    GitNode,
    -- TODO: maybe need to add (..) in the future
    GitCommit,
    GitObject,
    GitObjectHash,
  )
where

import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)

-- GitBlob = (file content in binary, filename)
type GitBlob = (ByteString, String)

-- GitTree = list of files and subdirectories
data GitNode = TreeNode GitTree | BlobNode GitBlob

type GitTree = [GitNode]

-- GitCommit = (tree, parent, author, committer, message, timestamp)
newtype GitCommit = GitCommit (GitTree, Maybe [GitCommit], String, String, String, UTCTime)

data GitObject = Tree GitTree | Commit GitCommit | Blob GitBlob

type GitObjectHash = (GitObject, ByteString)