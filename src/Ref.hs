module Ref
  ( GitRef,
    GitRefs,
  )
where

import GitObject

-- | Ref = (name of pointer - (HEAD, or branch pointer), commit object)
type GitRef = (String, GitCommit)

-- | Refs = list of Ref
type GitRefs = [GitRef]
