module Index
  ( GitIndex (..),
    GitIndexEntry (..),
    gitIndexSerialize,
  )
where

import Data.ByteString (ByteString)

-- Based on the documentation: https://github.com/vaibhavsagar/duffer/blob/master/duffer/src/Duffer/Plumbing.hs
data GitIndexEntry = GitIndexEntry
  { ctimeS :: Int,
    ctimeNs :: Int,
    mtimeS :: Int,
    mtimeNs :: Int,
    dev :: Int,
    ino :: Int,
    modeType :: Int,
    modePerms :: Int,
    uid :: Int,
    gid :: Int,
    fsize :: Int,
    sha :: String,
    flagAssumeValid :: Bool,
    flagStage :: Int,
    name :: String
  }
  deriving (Show)

newtype GitIndex = GitIndex [GitIndexEntry]
  deriving (Show)

-- Header: 12 bytes - "DIRC" + version (4 bytes - always 2 for mvp) + number of entries (4 bytes)
gitIndexSerialize :: GitIndex -> ByteString
gitIndexSerialize (GitIndex entries) = undefined

gitIndexEntrySerialize :: GitIndexEntry -> ByteString
gitIndexEntrySerialize entry = undefined