{-# LANGUAGE InstanceSigs #-}

module GitObject
  ( GitBlob,
    GitTree,
    GitCommit,
    GitObject,
    GitObjectHash,
    newBlob,
    newTree,
    newCommit,
    newGitObjectHash,
    gitObjectSerialize,
    gitShowStr,
    getBlobContent,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- GitBlob = (byteSize, file content in binary)
type GitBlob = (Int, String)

newBlob :: Int -> String -> GitObject
newBlob byteSize content = Blob (byteSize, content)

getBlobContent :: GitObject -> String
getBlobContent (Blob (_, content)) = content
getBlobContent _ = ""

-- GitTree = (byteSize, [(filemode bits, name of file/directory, sha1 hash)])
type GitTree = (Int, [(String, String, ByteString)])

newTree :: Int -> [(String, String, ByteString)] -> GitObject
newTree byteSize elems = Tree (byteSize, elems)

-- GitAuthor = (name, email, date - unix time in seconds, timezone string)
type GitAuthor = (String, String, Int, String)

-- GitCommitter = (name, email, date - unix time in seconds, timezone string)
type GitCommitter = (String, String, Int, String)

-- GitCommit = (bytesize, tree hash, parent hashes, author, committer, message)
type GitCommit = (Int, ByteString, [ByteString], GitAuthor, GitCommitter, String)

data GitObject = Tree GitTree | Commit GitCommit | Blob GitBlob

newCommit :: Int -> ByteString -> [ByteString] -> GitAuthor -> GitCommitter -> String -> GitObject
newCommit bytesize tree parents authorInfo committerInfo message = Commit (bytesize, tree, parents, authorInfo, committerInfo, message)

type GitObjectHash = (GitObject, ByteString)

newGitObjectHash :: GitObject -> ByteString -> GitObjectHash
newGitObjectHash obj objHash = (obj, objHash)

instance Show GitObject where
  show :: GitObject -> String
  show (Tree tree) = "Tree " ++ show tree
  show (Blob blob) = "Blob " ++ show blob
  show (Commit commit) = "Commit " ++ show commit

gitShowStr :: GitObjectHash -> String
gitShowStr (Blob (_, content), _) = content
gitShowStr (Tree (_, elems), treeHash) = "tree " ++ B.unpack treeHash ++ "\n\n" ++ filesDirs
  where
    filesDirs = concatMap (\(_, name, _) -> name ++ "\n") elems
gitShowStr (Commit (_, _, _, authorInfo, _, message), commitHash) = "commit " ++ B.unpack commitHash ++ "\nAuthor: " ++ authorName ++ "<" ++ authorEmail ++ ">\nDate:   " ++ authorTS ++ "\n\n    " ++ message
  where
    (authorName, authorEmail, authorUnixTS, authorTimeZone) = authorInfo
    authorTS = formatUTCTimeWithTimeZone authorTimeZone (unixToUTCTime (toInteger authorUnixTS))

-- Convert the gitObject
gitObjectSerialize :: GitObject -> ByteString
-- Blob: Header + filecontent
gitObjectSerialize (Blob (byteSize, content)) = BSL.toStrict (BSLC.pack ("blob " ++ show byteSize ++ "\0" ++ content))
-- (header + concatenation of Blobs and subtrees within Tree)
gitObjectSerialize (Tree (byteSize, xs)) = BSL.toStrict (BSLC.pack ("tree " ++ show byteSize ++ "\0" ++ content xs))
  where
    content :: [(String, String, ByteString)] -> String
    content [] = ""
    content [(permission_bit, name, hash)] = permission_bit ++ " " ++ name ++ "\0" ++ BS.unpack hash
    content ((permission_bit, name, hash) : xxs) = permission_bit ++ " " ++ name ++ "\0" ++ BS.unpack hash ++ content xxs
-- Commit: header + concatenation of content inside
gitObjectSerialize (Commit (byteSize, treeHash, parentHashes, authorObj, committerObj, message)) = BSL.toStrict (BSLC.pack ("commit " ++ show byteSize ++ "\0" ++ content))
  where
    (aName, aEmail, aDate, aTimeStamp) = authorObj
    (cName, cEmail, cDate, cTimeStamp) = committerObj
    content = "tree " ++ BS.unpack treeHash ++ "\n" ++ concatMap (\x -> "parent " ++ BS.unpack x ++ "\n") parentHashes ++ gitAuthor ++ gitCommitter ++ message
    -- TODO: ask Jack if using show on Int is fine
    gitAuthor = "author " ++ aName ++ "<" ++ aEmail ++ "> " ++ show aDate ++ " " ++ aTimeStamp ++ "\n"
    gitCommitter = "committer " ++ cName ++ "<" ++ cEmail ++ "> " ++ show cDate ++ " " ++ cTimeStamp ++ "\n\n"

------ Helpers (maybe separate files later) -------
unixToUTCTime :: Integer -> UTCTime
unixToUTCTime unixTime = posixSecondsToUTCTime $ fromInteger unixTime

formatUTCTimeWithTimeZone :: String -> UTCTime -> String
formatUTCTimeWithTimeZone timezoneOffset utcTime = formatTime defaultTimeLocale "%a %b %e %T %Y " utcTimeWithTZ ++ timezoneOffset
  where
    -- Parse the timezone offset from the format "-0500"
    hours = read (take 3 timezoneOffset) :: Int
    minutes = read (drop 3 timezoneOffset) :: Int
    timezoneMinutes = hours * 60 + minutes
    utcTimeWithTZ = utcTime {utctDayTime = utctDayTime utcTime + fromIntegral (timezoneMinutes * 60)}
