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
### Lazy Evaluation in File I/O
- When working on the test, there were cases where we needed to read and write to the same file to preserve the original content (such as in gitUpdateRef, etc.). However, due to lazy evaluation in Haskell, errors occurred because the file was being accessed while still in the process of being updated. For this reason, we opted to use strict IO in these cases.
https://stackoverflow.com/questions/5053135/resource-busy-file-is-locked-error-in-haskell

Solved by using strict IO: readfile'

### Base 16 Encoding
We experienced multiple bugs related to the representation of the SHA-1 hash for several days. Apparently, the reason was that all Git object files store a base16-encoded hash value, while the index file stores a non-encoded hash value. This led to incorrect hash values in multiple places when staging files to the index. We learned, in a painful way, to have a really good understanding of how binary data is actually stored when interacting with it.

To resolve this issue, we decided to follow the invariant that the `GitHash` type will always contain a ByteString that is encoded in base16. This means that we need to perform encoding/decoding when parsing/writing to the index file. The representation of the hash value in our system can be summarized as follows:

- The SHA function returns a ByteString that is not encoded in base16.
- All Git object files store the hash that is encoded in base16.
- The index stores the hash value that is not encoded in base16.
- Our invariant: `GitHash` stores the ByteString that is encoded in base16.


<!-- Proving the IO heavy application can still have benefits by using Haskell as implementation language -->


Note:
git add
- Only take files as command line arguments
- Cross platform functionality

Useful during develpment:
- Copying the executable: `cp .stack-work/dist/x86_64-linux/ghc-9.4.7/build/HaskGit-exe/HaskGit-exe haskgit`


IMPORTANT
- All of the bytestring in our memory should be encoded into hexadecimal representation. In GitObject, we don't need to encode or decode because we are always storing as hex representation, but when storing in index or generating new hash, we must encoded to hex representation before creating GitHash type.


Notes
- Some of the metadata are not crucial for our mvp (file mode, byte size, etc). While we read in data correctly if these data are found in the existing files, but when we are creating new git objects or new index entry on our own, we are writing default data. If these data become crucial in the future, we may need to modify our codebase little bit.