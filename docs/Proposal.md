# HaskGit

![Git Image](../assets/git-logo.svg)

Team members:

- James Oh
- Jack Oh
- Chen Guo

## Summary Description

HaskGit is a Git implementation using Haskell. The goal of the project is to implement core git commands with Haskell.

### Why Haskell to implement Git?
- Strong type system and functional paradigm help us to ensure the correctness of Git protocol.
- Pure functions are useful to keep data integrity in Git.
- Haskell can be compiled to run on various platforms which is suitable for software like Git.

## Additional Details

### Use Case
- TODO (Chen)
- One or more typical "use cases". These might include "storyboards" explaining
  how a user would interact with the program or some interesting "input/output"
  examples.

### Minimum Viable Product
The MVP will implement most of the git commands that can run locally. The list of commands are following:

Command list
Full list of git commands (not part of MVP): https://git-scm.com/docs 

TODO: (Jack)

Commands that will be implemented:

- Creating projects
  - init: Creates an empty Git repository or reinitialize an existing one 
- Basic Snapshotting.
  - add: Add file contents to the index. The "index" holds a snapshot of the content of the working tree, and it is this snapshot that is taken as the contents of the next commit.  
  - status: Displays paths that have differences between the index file and the current HEAD commit, paths that have differences between the working tree and the index file, and paths in the working tree that are not tracked by Git.
  - commit: Create a new commit containing the current contents of the index and the given log message describing the changes.
  - restore: Restore specified paths in the working tree with some contents from a restore source. If a path is tracked but does not exist in the restore source, it will be removed to match the source.
  - reset: Reset current HEAD to the specified state.
  - rm: Remove files matching pathspec from the index.
- Branching and Merging
  - branch: List, create, or delete branches.
  - checkout: Switch branches or restore working tree files.
    ```console
    git checkout branch-name

    git checkout commit-hash  
    ```

- Inspection and Comparison
  - show: Shows one or more git objects.
     ```console
    git show hash_value

    -- show latest commit
    git show  
    ```
  - log: Show commit logs
  - diff: Show changes between the working tree and the index or a tree, changes between the index and a tree, changes between two trees, changes resulting from a merge, changes between two blob objects, or changes between two files on disk.

- Patching
  - rebase: Reapply commits on top of another base tip
    ```console
    -- upstream can be branch or commit reference
    git rebase <upstream>
    ```
  - revert: Given one or more existing commits, revert the changes that the related patches introduce, and record some new commits that record them.

### Code Structure
- TODO: explain that we will follow "Functional core and imperative shell" design pattern (James)
- A sketch of intended components (key functions, key data structures, separate
  modules).  To satisfy the "multiple Haskell modules" requirement, it may
  suffice to separate an application into a "model-controller" module and a
  "view" module (e.g., a "text view" module that could be replaced by a "GUI
  view" module).

### Key Data Structures

#### Git Objects
We need specific data structures to represent a data unit in the Git database. We will refer to this as a 'Git object' from now on. There are four types of Git objects: BLOB (Binary Large Object), Tree, Commit, and Tag. For the scope of this project, our MVP will not include Tag. For more information about Git objects, please check out: https://git-scm.com/book/en/v2/Git-Internals-Git-Objects.

1. BLOB Object
A blob object is compressed binary data of a content file, representing the file data in a specific state.

```haskell
-- Blob = (file content in binary, hash of (header + new content))
type Blob = (ByteString, ByteString)
```

2. Tree Object
A tree object represents a directory in a file structure. A tree consists of blobs (files in the directory) and subtrees (subdirectories).

```haskell
-- Tree = (list of files and subdirectories, hash of blobs and subtrees)
type Tree = ([Tree | Blob], ByteString)
```

3. Commit Object
A commit object represents each commit made within the repository. It contains the tree object representing the root directory, the parent commit (if it's the first commit, it will be Nothing), author, committer, and commit message.

```haskell
-- Commit = Maybe(tree of root directory, parent commit, author, commiter, commit message)
type Commit = Maybe(Tree, Commit, String, String, String)
```

#### References
In Git, references are labels that point to specific Git objects. For example, when we check out a new branch, there will be a reference pointing to the corresponding Commit object, with the branch name serving as a label. We are going to implement two new types, Ref and Refs, to represent this.

```haskell
-- Ref = list of (name of pointer - (HEAD, branch name, etc), commit object)
type Ref = (String, Commit)
```

```haskell
-- Refs = list of Ref
type Refs = [Ref]
```

#### Index
In Git, the index acts as an intermediate step between the working directory and the Git repository, serving as a staging area. When we read data from the index file, we need a representation of the 'staged changes' to add them to the commit tree when 'git commit' is called. Since the index represents all the changed files, we can alias the `Index` type as `Tree` and use this data to create a Commit object later.

```haskell
type Index = Tree
```

### Key Functions
(TODO: Make sure you make function signatures)
1. Argument Parsing (Chen)

2. TODO: (Jack - git hash-object and git cat-file, read, write, if we need more James will help)

### Libraries
1. SHA1: https://hackage.haskell.org/package/cryptohash-sha1  
We need to hash different things to implement Git.
2. zlib: https://hackage.haskell.org/package/zlib
Git uses zlib to compress the new content and store files efficiently.
3. Other libraries: Data.ByteString module, System.IO, etc.

- TODO: (James, Jack, Chen) - add all the libraries that will be used

### Testing
- TODO: https://hackage.haskell.org/package/HUnit (Chen)
- Thoughts on testing. These might include critical functions or data structures
  that will be given
  [`tasty`](https://hackage.haskell.org/package/tasty) tests.

### Checkpoint
- TODO: Expected functionality to be completed at the Checkpoint (Jack).

### Stretch Goals
- TODO (Chen: I have a summary on google doc so you can just add some explanation to it)

### References
- https://git-scm.com/book/en/v2/Git-Internals-Git-Objects 
- Write yourself a Git: https://wyag.thb.lt/
- TODO: add more (James, Jack, Chen)

Other TODO List (James, Jack, Chen)
- Check if we missed anything from proposal rubric
- Check if we missed anything from final project rubric
- Double check writings

<!-- Proposal Rubric (we also need to check final project rubric as well) -->
<!-- 
One or more typical “use cases”. These might include “storyboards” explaining how a user would interact with the program or some interesting “input/output” examples.
A sketch of intended components (key functions, key data structures, separate modules). To satisfy the “multiple Haskell modules” requirement, it may suffice to separate an application into a “model-controller” module and a “view” module (e.g., a “text view” module that could be replaced by a “GUI view” module).
Thoughts on testing. These might include critical functions or data structures that will be given tasty tests.
Thoughts on a “minimal viable product” and “stretch goals”. Be sure to review the final project grading rubric and consider organizing the project around a core deliverable that will almost certainly be achieved and then a number of extensions and features that could be added to ensure that project is of suitable size/scope/effort.
Expected functionality to be completed at the Checkpoint. -->
