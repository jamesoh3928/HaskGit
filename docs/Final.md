# HaskGit

Team members:

- James Oh
- Jack Oh
- Chen Guo

## Summary Description

Reiterate the summary description of the overall goal of the project (updated as
necessary from the Proposal and/or Checkpoint documents).

## Project Execution Summary

Describe the work done for the project and lessons learned.

## Additional Details

- List any additional Haskell libraries required for the project (i.e., what
  `extra-deps` have been added to `stack.yaml` files and `[build-depends]` have
  been added to `package.yaml`/`<package_name>.cabal` files).
- Briefly describe the structure of the code (what are the main components, the
  module dependency structure). Why was the project modularized in this way?
- Choose (at least) one code excerpt that is a particularly good example of
  Haskell features, idioms, and/or style and describe it.
- Were any parts of the code particularly difficult to expres using Haskell?
  What are the challenges in refining and/or refactoring this code to be a
  better example of idiomatic Haskell?
- Describe any approaches attempted and then abandoned and the reasons why. What
  did you learn by undertaking this project?

Review the final project grading rubric and discuss any relevant aspects of the
project.

Note: Be sure that all `.hs` source files and any supporting files (e.g.,
`stack.yaml`, `package.yaml`/`<package_name>.cabal` files, data files, examples,
...) have been committed and pushed.

## Challenges
- When working on the test, there were cases where we needed to read and write to the same file to preserve the original content (such as in gitUpdateRef, etc.). However, due to lazy evaluation in Haskell, errors occurred because the file was being accessed while still in the process of being updated. For this reason, we opted to use strict IO in these cases.
https://stackoverflow.com/questions/5053135/resource-busy-file-is-locked-error-in-haskell


<!-- Proving the IO heavy application can still have benefits by using Haskell as implementation language -->


Note:
git add
- Only take files as command line arguments
- Cross platform functionality