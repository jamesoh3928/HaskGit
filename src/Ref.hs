-- NOTE: this file is not currently used because String is enough to represent ref so far. This file may be used in the future.

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
