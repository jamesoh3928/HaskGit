module Index
  ( GitIndex (..),
    GitIndexEntry (..),
  )
where

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