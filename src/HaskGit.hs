module HaskGit
    (
    ) where

-- type GitIndex = GitTree

-- -- List of plumbing commands

-- -- This command computes the SHA-1 hash of Git objects.
-- gitHashObject :: GitObject -> Bool -> ByteString
-- gitHashObject = undefined

-- -- This command creates a tree object from the current index (staging area).
-- gitWriteTree :: GitIndex -> ByteString
-- gitWriteTree = undefined

-- -- This command reads a tree object and checks it out in the working directory.
-- gitReadTree :: ByteString -> GitIndex
-- gitReadTree = undefined

-- -- This command creates a new commit object based on a tree object and parent commits.
-- gitCommitTree :: GitTree -> [GitCommit] -> String -> String -> String -> UTCTime -> GitCommit
-- gitCommitTree = undefined

-- -- This command is used to add or update index entries (staging area).
-- gitUpdateIndex :: GitIndex -> ByteString
-- gitUpdateIndex = undefined

-- -- This command reads the index file, which represents the current state of the working directory.
-- gitReadCache :: ByteString -> GitIndex
-- gitReadCache = undefined

-- -- This command is used to write changes to the index back to the index file.
-- gitWriteIndex :: GitIndex -> ByteString
-- gitWriteIndex = undefined

-- -- This command provides a way to traverse and filter the commit history in various ways
-- gitRevList :: ByteString -> [GitCommit]
-- gitRevList = undefined

-- -- 
-- serializeGitObject ::  GitObject -> ByteString
-- serializeGitObject = undefined

-- -- 
-- deserializeGitObject :: ByteString -> GitObject
-- deserializeGitObject = undefined

-- -- List of porcelain commands
-- gitInit :: () -> ByteString
-- gitInit = undefined

-- gitAdd :: ByteString -> ByteString
-- gitAdd = undefined

-- gitStatus :: ByteString -> String
-- gitStatus = undefined

-- gitCommit :: ByteString -> ByteString
-- gitCommit = undefined

-- gitRestore :: ByteString -> ByteString
-- gitRestore = undefined

-- gitReset :: ByteString -> ByteString
-- gitReset = undefined

-- gitRm :: ByteString -> ByteString
-- gitRm = undefined

-- gitCheckout :: ByteString -> ByteString
-- gitCheckout = undefined

-- gitBranch :: ByteString -> ByteString
-- gitBranch = undefined

-- gitShow :: ByteString -> String
-- gitShow = undefined

-- gitLog :: ByteString -> String
-- gitLog = undefined

-- gitRebase :: ByteString -> ByteString
-- gitRebase = undefined

-- gitRevert :: ByteString -> ByteString
-- gitRevert = undefined