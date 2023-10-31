module GitObject
    ( 
    ) where

-- -- GitBlob = (file content in binary, filename)
-- type GitBlob = (ByteString, String)

-- -- GitTree = list of files and subdirectories
-- type GitTree = [Tree | Blob]

-- -- GitCommit = (tree, parent, author, committer, message, timestamp)
-- type GitCommit = (GitTree, [GitCommit], String, String, String, UTCTime)

-- data GitObject = GitTree | GitCommit | GitBlob
-- data GitObjectHash = (GitObject, ByteString)