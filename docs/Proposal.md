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
<!-- - TODO (Chen)
- One or more typical "use cases". These might include "storyboards" explaining
  how a user would interact with the program or some interesting "input/output"
  examples. -->
  HaskGit is a CLI application, all I/O interactions with users happen on command line.

-
  ```bash
  git help
  ```
  Output: All available HaskGit commands, with a brief description of each command
  ```
  Example:
  init    Create an empty Git repository
  add     Add file contents to the index
  commit  Record changes to the repository
  ```

-
  ```bash
  git init <path>
  ```
  Output: "Initialized empty Git repository in <path>/.git/"

-
  ```bash
  git add <path>
  ```
  Output:
  - no path provided: "Nothing specified, nothing added."
  - the path exists: "add <path>"
  - the path does not exist: "error: path <path> did not match any files"

-
  ```bash
  git commit <msg>
  ```
  Output:
  - `msg` is provided:
    ```
     [<branch> <hash>] <msg>
     <#> file(s) changed, <#> insertions(+), <#> deletions(-)
     <create/delete> mode <file permission> <path>
     -- repeat the above message if more path is added
    ```
  - `msg` is not provied:
    call an editor with comment on the header
    ```
    # On branch <branch>
    # Changes to be committed:
    #   new file: <path>
    #   modified: <path>
    #
    <msg>
    ```
    when the user saves it and exit, 
    display the same output as the above case.
  - others:
    - "nothing to commit, working tree clean"
    ```
    On branch <branch>
    Changes not staged for commit: OR  Untracked files:
    modified: OR  untracked: <path>
    ```


### Minimum Viable Product
The MVP will implement most of the git commands that can run locally. For feasibllity, we will implement the git commands without the flags options. The list of commands that will be implmented are following:

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
-- Blob = (file content in binary, hash of (header + new content), filename)
type Blob = (ByteString, ByteString, ByteString)
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

   Basically it is a Command-Line-Interface Application, 
2. Hashing Git Objects
   
   In Git, the path of git objects are determined by using hash function. This will make tracking the git object easy and fast. 
   
   We will need to implement a function that takes a git object and return a hash value. We would also need a flag that indicates whether or not the git object we are hashing will be stored in the path. If the 
   Following the design patter of git, HaskGit will also store git objects in .git/objects. 

    To store the git object, we take first two characters of hash value, then a directory delimiter /, then the remaining part. For example,

    ```
    -- hash value: e673d1b7eaa0aa01b5bc2442d570a765bdaae751
    -- it will be stored at:
    -- .git/objects/e6/73d1b7eaa0aa01b5bc2442d570a765bdaae751
    ```
    To implment hashObject function, we create a new data with all of our git objects which are Tree, Commit, and Blob. The function will then use the data in gitObject to get the hash value. The function will also take gitObject and a boolean value to indicate if git objects should be stored in .git/objects. 
    ```haskell
    Data GitObject = Tree | Commit | Blob
    -- (Git object, flag to indicate store object in repository)
    hashObject :: GitObject -> Bool -> ByteString
    ```

    For hash, the plan is to use Crypto.Hash.SHA1.hash function. This function takes ByteString as input and return hash value as ByteString. We would also use Data.ByteString.Char8 module to convert string to ByteString.

    ```
    import qualified Crypto.Hash.SHA1 as SHA1

    -- apply hash function
    sha1Hash = SHA1.hash SomeByteString
    ```

    The current plan for getting hash value for each object type:
    
    - Blob: Just run hash function on the content
    - Tree: Concatenate hash value of blobs and subtrees and hash the concatenated value
    - Commit: Concatenate hash value of tree of root directory, parent commit, author, commiter, and commit message. Return the hash value of concatenated value.

3. Git cat file
  
  We would need to implement a function that gets the content of git objects.

  ```
  getCatFile :: GitObject -> [String]
  ```

  Based on type of GitObject, the data will be extracted, uncompressed using Codec.Compression.Zlib and converted to string. Based on type of GitObject, it will return the following String:

  - For Blob, it will return the actual content. (There will be only one String in a list)
  - For Tree, it will return the list of String representing each elements by "Git Object type, hash value, and file name" (actual git includes the file mode but we will not include this for project scope)

  ```Console
  ["blob a906cb2a4a904a152e80877d4088654daad0c8599 file1.txt", 
   "blob 8f94139338f9404f26296befa88755fc2598c2893 file2.txt",
   "tree 23ebdb3b47d0f41f0c9b07b6286e103b971a51c1  subdirectory"]
  ```
  - For Commit, it will return the list of String representing "Tree , parent (if not initial commit), author, committer, and commit message". For example:

    ```
    tree a906cb2a4a904a152e80877d4088654daad0c8599
    parent 8f94139338f9404f26296befa88755fc2598c2893
    author Someone1 
    committer Someone2

    Initial commit
    ```

### Libraries
1. SHA1: https://hackage.haskell.org/package/cryptohash-sha1  
We need to hash different things to implement Git.
2. zlib: https://hackage.haskell.org/package/zlib
Git uses zlib to compress the new content and store files efficiently.
  ```Haskell
   import qualified Data.ByteString.Char8 as C8

  -- convert string to ByteString
  dataToHash :: ByteString
  dataToHash = C8.pack "Hello, world!"
  ```
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
  By the first checkpoint, we hope to complete following tasks:
  
  - Defining data types for Git Objects, Ref, and Index
  - Argument parser function
  - Git Object hash function
  - Git Object cat function
  - Unit test for above functions

  Defining data types would be straight forward as this was already discussed in this proposal. We will first focus on establishing main functionality such as argument parser, hash, cat function while creating unit test along the way. After the first checkpoint, the git commands in the MVP scope will be split for each member to implment.

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
