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
   ```
   git init
   ```
- Basic Snapshotting.
  - add: Add file contents to the index (which means move to staging area).
    ```
    git add filename
    ```
  - status: Displays differences between the index file and the current HEAD commit, paths that have differences between the working tree and the index file, and paths in the working tree that are not tracked by Git.
    ```
    git status
    ```
  - commit: Create a new commit containing the current contents of the index.
    ```
    -- this will create commit for current staging area
    git commit
    ```
  - restore: Restore a file from the last commit to the working directory. (you can also unstage by using --staged but that would be stretch goal)
    ```
    git restore <file>
    ```
  - reset: Reset current HEAD to the specified state.
    ```
    -- this will unstage all changes
    git reset

    -- Move the branch pointer to specific commit
    git reset commit-hash
    ```
  - rm: Remove files from the index.
    ```
    git rm file
    ```
- Branching and Merging
  - branch: List, create, or delete branches.
    ```
    -- list the branch
    git branch

    -- create new branch
    git branch branch_name
    ```
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
    ```
    git log
    ```
  - diff: Show changes between the working tree and the index or a tree, changes between the index and a tree, changes between two trees, changes resulting from a merge, changes between two blob objects, or changes between two files on disk (this command might be challenging to implement).
    ```
    git diff
    ```

- Patching
  - rebase: Reapply commits on top of another base tip
    ```console
    -- upstream can be branch or commit reference
    git rebase <upstream>
    ```
  - revert: Revert some existing commits
    ```
    git revert commit-hash
    ```
### Code Structure
HaskGit will follow the 'Functional core and imperative shell' design pattern, meaning that we will separate pure functions from impure functions with side effects (those that interact with the outside world, such as I/O). The basic structure will follow the Model-View-Controller (MVC) pattern, where `HaskGit.hs` will represent the controller, all other files in the Core directory will represent the Model, and `Main.hs` will represent the View. The code structure will look like the following:

```
src/
│   |-- Core/
|   |------ GitObjects.hs -- types and functions related to Git objects
|   |------ Refs.hs -- types and functions related to References
|   |------ Index.hs -- types and functions related to Index
│   │------ HaskGit.hs -- functions for all git commands
│   |-- Shell/
│   │   |-- Main.hs -- main function that does I/O and other impure operations
|
|
|test/
│   |-- Core/
|   |------ GitObjectsSpec.hs
|   |------ RefsSpec.hs
|   |------ IndexSpec.hs
│   │------ HaskGitSpec.hs
│   |-- Shell/
|   |------ Main.hs
```

The above structure may change throughout the project if it appears overly complicated or needs further modularization. This is just a basic idea of how we can divide pure components and impure components. The test files can also be merged into one file if necessary.

The main function in `HaskGit.hs` will receive input from the user and call pure functions. Its structure will be as follows:

```haskell
case get_command of
	init -> runInit
	add->runAdd
  ...
```

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

2. TODO: (Jack - git hash-object and git cat-file, read, write)

### Libraries
1. SHA1: https://hackage.haskell.org/package/cryptohash-sha1  
We need to hash different things to implement Git.
2. zlib: https://hackage.haskell.org/package/zlib
Git uses zlib to compress the new content and store files efficiently.
3. argParser: https://hackage.haskell.org/package/argparser-0.3.4/docs/System-Console-ArgParser.html 
Since we are interacting with command line, we need to parse arguments.
4. Other libraries: Data.ByteString module, System.IO, etc.

- TODO: (Jack, Chen) - add more if needed

### Testing
- TODO: https://hackage.haskell.org/package/HUnit (Chen)
- Thoughts on testing. These might include critical functions or data structures
  that will be given
  [`tasty`](https://hackage.haskell.org/package/tasty) tests.

### Checkpoint
- TODO: Expected functionality to be completed at the Checkpoint (Jack).

### Stretch Goals
- TODO (Chen: I have a summary on google doc so you can just add some explanation to it)

### Areas for Feedback
- We would love to receive feedback on the scope of the project. Do you think this is feasible? Are there any challenging parts that we might be missing?
- Do you have any feedback on our data structures and code structure?

### References
- Pro Git: https://git-scm.com/book/en/v2/Git-Internals-Git-Objects 
- Write yourself a Git: https://wyag.thb.lt/
- TODO: add more if needed (Jack, Chen)

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
