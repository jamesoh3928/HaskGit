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
The MVP will implement most of the Git commands that can run locally. For feasibility, we will implement the Git commands without any flag options, except for '-m' in the case of commit. Most commands will follow the default options. The list of commands to be implemented is as follows:

Command list
Full list of git commands (not part of MVP): https://git-scm.com/docs 

Commands that will be implemented:

- Creating projects
  - init: Creates an empty Git repository or reinitialize an existing one.
   ```
   git init
   ```
- Basic Snapshotting.
  - add: Adds file contents to the index (which means move to staging area).
    ```
    git add filename
    ```
  - status: Displays differences between the index file and the current HEAD commit, paths that have differences between the working tree and the index file, and paths in the working tree that are not tracked by Git.
    ```
    git status
    ```
  - commit: Creates a new commit containing the current contents of the index.
    ```
    -- this will create commit for current staging area
    git commit

    git commit -m "Some commit message"
    ```
  - restore: Restores a file from the last commit to the working directory. (You can also unstage by using --staged, but that would be a stretch goal).
    ```
    git restore <file>
    ```
  - reset: Resets the current HEAD to the specified state.
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
  - branch: Lists, creates, or deletes branches.
    ```
    -- list the branch
    git branch

    -- create new branch
    git branch branch_name
    ```
  - checkout: Switches branches or restores working tree files.
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
  - diff: Shows changes between the working tree and the index or a tree, changes between the index and a tree, changes between two trees, changes resulting from a merge, changes between two blob objects, or changes between two files on disk (this command might be challenging to implement).
    ```
    git diff
    ```

- Patching
  - rebase: Reapplies commits on top of another base tip. 
    ```console
    -- upstream can be branch or commit reference
    git rebase <upstream>
    ```
  - revert: Reverts some existing commits
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
We need specific data structures to represent a data unit in the Git database. We will refer to this as a 'Git object' from now on. There are four types of Git objects: Blob (Binary Large Object), Tree, Commit, and Tag. For the scope of this project, our MVP will not include Tag. For more information about Git objects, please check out: https://git-scm.com/book/en/v2/Git-Internals-Git-Objects.

1. Blob Object

    A blob object is compressed binary data of a content file, representing the file data in a specific state.

```haskell
-- Blob = (file content in binary, filename)
type Blob = (ByteString, String)
```

2. Tree Object

    A tree object represents a directory in a file structure. A tree consists of Blobs (files in the directory) and subtrees (subdirectories).

```haskell
-- Tree = list of files and subdirectories
type Tree = [Tree | Blob]
```

3. Commit Object

    A commit object represents each commit made within the repository. It contains the tree object representing the root directory, the parent commit (if it's the first commit, it will be Nothing), author, committer, and commit message.

```haskell
-- Commit = Maybe(tree of root directory, parent commit, author, commiter, commit message, creationg time)
type Commit = Maybe((Tree, Commit, String, String, String, UTCTime))
```

It will be useful to have some sort of enum that will represent all Git objects, and we would like to store with hash value as well. Therefore, we will also create following types:

```haskell
Data GitObject = Tree | Commit | Blob

Data GitObjectHash = (GitObject, ByteString)
```

#### References
In Git, references are labels that point to specific Git objects. For example, when we check out a new branch, there will be a reference pointing to the corresponding Commit object, with the branch name serving as a label. We are going to implement two new types, Ref and Refs, to represent this.

```haskell
-- Ref = (name of pointer - (HEAD, branch name, etc), commit object)
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

    The command-line interface consists of command and infomation such as path. To reduce the complexity, there is no option for each command.

    We use `<library>` ...

    FIXEME: sample code of `ArgParser` doesn't work. \
      Report error: "fail to parse 'pos1' : missing arg".
    
    I also looked [cmdargs: Command line argument processing](https://hackage.haskell.org/package/cmdargs)
      which is the most downloaded one in hackage.
    It also has some problems on type class "Data" resolving.

    TODO: try to find workable approach and give a demo with "git help"

2. Hashing Git Objects
   
   In Git, the paths of Git objects are determined by using a hash function. This makes tracking Git objects easy and fast.
   
   To accomplish this, we need to implement a function that takes a Git object and returns a hash value. We will also require a flag that indicates whether or not the Git object being hashed will be stored in the path. Following the design pattern of Git, HaskGit will also store Git objects in the .git/objects directory.

    To store a Git object, we take the first two characters of the hash value, then a directory delimiter '/', followed by the remaining part. For example:

    ```
    -- hash value: e673d1b7eaa0aa01b5bc2442d570a765bdaae751
    -- it will be stored at:
    -- .git/objects/e6/73d1b7eaa0aa01b5bc2442d570a765bdaae751
    ```

    The function will use this data in GitObject to compute the hash value. The function will also take GitObject and a boolean value to indicate whether Git objects should be stored in .git/objects.

    ```haskell
    -- Git object > flag to indicate store object in the repository
    hashObject :: GitObject -> Bool -> ByteString
    ```

   For the hash computation, the plan is to use Crypto.Hash.SHA1.hash function. This function takes ByteString as input and returns the hash value as ByteString. We will also use the Data.ByteString.Char8 module to convert strings to ByteString. Here's how you can use it:

    ```
    import qualified Crypto.Hash.SHA1 as SHA1

    -- apply hash function
    sha1Hash = SHA1.hash SomeByteString
    ```

    The current plan for obtaining the hash value for each object type is as follows:

    - For Blob: Hash of (header + new content). Note that header = "Blob #{content.bytesize}\0"
    - For Tree: Hash of the concatenation of Blobs and subtrees within Tree
    - For Commit: Hash of concatenated Tree Object hash + committer name + author name + commit message + creation time + Parent commit hash

3. Implementing cat function
  
  We need to implement a function that retrieves the content of Git objects. 

  ```
  getCatFile :: GitObject -> [String]
  ```

  Based on the type of GitObject, the data will be extracted, uncompressed using Codec.Compression.Zlib, and converted to a string. Depending on the type of GitObject, it will return the following string:

  - For Blob, it will return the actual content. (There will be only one string in the list)
  - For Tree, it will return a list of strings representing each element as "Git Object type, hash value, and file name" (the actual Git format includes the file mode, but we will not include this for the project's scope). For example:

  ```Console
  ["blob a906cb2a4a904a152e80877d4088654daad0c8599 file1.txt", 
   "blob 8f94139338f9404f26296befa88755fc2598c2893 file2.txt",
   "tree 23ebdb3b47d0f41f0c9b07b6286e103b971a51c1  subdirectory"]
  ```
  - For Commit, it will return a list of strings representing "Tree, parent (if not an initial commit), author, committer, and commit message". For example:

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
FIXME: the demo code doesn't work.
4. Other libraries: Data.ByteString module, System.IO, Data.Time, etc.

- TODO: (Jack, Chen) - add more if needed

### Testing
- TODO: https://hackage.haskell.org/package/HUnit (Chen)
- Thoughts on testing. These might include critical functions or data structures
  that will be given
  - [`tasty`](https://hackage.haskell.org/package/tasty) tests.
  - [HUnit: A unit testing framework for Haskell](https://hackage.haskell.org/package/HUnit)

  1. Command-Line Interface
    - use HUnit to assert equal by string (actual and expected output)
  2. GitObject (Hash for tree and blob)
    - generate files and directories
    - assert equal by hash value
    - Git utility tools of "hash-object", "cat-file", and "read/write/commit-tree"
        maybe helpful as a guide.
  3. CommitObject
    - generate commits and store info used for generatation
    - assert equal by hash value
    - FIXME: if there is no feature to make a commit with a custom datetime,
      it may be compared by info decoded by the hash
  4. Git Core functionality (git init, git add, git commit, any command that changes direcotry `.git`.
  5. Test Management (as there are different aspects of the database, i.e., CLI, Data structure, Core functionality, and possible extendable features )
    - [Nicolas Mattia – Automatically generated directories for individual tasty tests](https://www.nmattia.com/posts/2018-04-30-tasty-test-names/)
  6. Format of test: [Writing tests with Hspec - Hspec: A Testing Framework for Haskell](https://hspec.github.io/writing-specs.html)


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

Git is a system for distributed version control. 
  Besides the core features for basic Git usage, it offers a

### Areas for Feedback
- We would love to receive feedback on the scope of the project. Do you think this is feasible? Are there any challenging parts that we might be missing?
- Do you have any feedback on our data structures and code structure?

### References
- Pro Git: https://git-scm.com/book/en/v2/Git-Internals-Git-Objects 
- Write yourself a Git: https://wyag.thb.lt/
- Git Community Book: https://shafiul.github.io/gitbook/index.html
- TODO: add more if needed (Jack, Chen)

Other TODO List (James, Jack, Chen)
- Check if we missed anything from proposal rubric
- Check if we missed anything from final project rubric
- Double check writings

Missing
- separate modules
  - Hunit

<!-- Proposal Rubric (we also need to check final project rubric as well) -->
<!-- 
One or more typical “use cases”. These might include “storyboards” explaining how a user would interact with the program or some interesting “input/output” examples.
A sketch of intended components (key functions, key data structures, separate modules). To satisfy the “multiple Haskell modules” requirement, it may suffice to separate an application into a “model-controller” module and a “view” module (e.g., a “text view” module that could be replaced by a “GUI view” module).
Thoughts on testing. These might include critical functions or data structures that will be given tasty tests.
Thoughts on a “minimal viable product” and “stretch goals”. Be sure to review the final project grading rubric and consider organizing the project around a core deliverable that will almost certainly be achieved and then a number of extensions and features that could be added to ensure that project is of suitable size/scope/effort.
Expected functionality to be completed at the Checkpoint. -->
