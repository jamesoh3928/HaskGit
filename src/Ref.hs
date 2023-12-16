module Ref
  ( GitRef,
    GitRefs,
  )
where

import GitObject

-- | Ref = (name of pointer - (HEAD, branch name, etc), commit object)
type GitRef = (String, GitCommit)

-- | Refs = list of Ref
type GitRefs = [GitRef]
