module GitObject
  ( GitBlob,
    GitTree,
    GitNode,
    -- TODO: maybe need to add (..) in the future
    GitCommit,
    GitObject,
    GitObjectHash,
    newBlob,
    gitObjectToBS,
  )
where

import qualified Codec.Compression.Zlib as Zlib
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Time.Clock (UTCTime)

-- GitBlob = (file content in binary, filename)
type GitBlob = (String, String)

data GitNode = TreeNode GitTree | BlobNode GitBlob

-- GitTree = list of files and subdirectories
type GitTree = [GitNode]

-- GitCommit = (tree, parent, author, committer, message, timestamp)
newtype GitCommit = GitCommit (GitTree, Maybe [GitCommit], String, String, String, UTCTime)

data GitObject = Tree GitTree | Commit GitCommit | Blob GitBlob

type GitObjectHash = (GitObject, ByteString)

newBlob :: String -> String -> GitObject
newBlob content filename = Blob (content, filename)

-- newTree :: GitTree -> GitObject
-- newTree tree = Tree tree

-- newCommit :: GitCommit -> GitObject
-- newCommit commit = Commit commit

gitObjectToBS :: GitObject -> ByteString
gitObjectToBS (Blob (content, filename)) = BSL.toStrict (Zlib.compress (BSLC.pack content))