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
  HaskGit will be a CLI application, all I/O interactions with users happen on command line. Following are some example commands:

-
  ```bash
  haskgit help
  ```
  Output: All available HaskGit commands, with a brief description of each command
  ```
  init    Create an empty Git repository
  add     Add file contents to the index
  commit  Record changes to the repository
  ...
  ```

-
  ```bash
  haskgit init <path>
  ```
  Output: "Initialized empty Git repository in <path>/.git/"

-
  ```bash
  haskgit add <path>
  ```
  Output:
  - no path provided: "Nothing specified, nothing added."
  - the path exists: "add <path>"
  - the path does not exist: "error: path <path> did not match any files"

-
  ```bash
  haskgit commit <msg>
  ```
  Output:
  - `msg` is provided:
    ```
     [<branch> <hash>] <msg>
     <#> file(s) changed
    ```
  - `msg` is not provied:
    For our MVP, we will require users to provide messages on command line. 
    - "Commit message not provided. Use '-m' flag."
  - no files are in staging area:
    - "nothing to commit, working tree clean"
    ```
    On branch <branch>
    Changes not staged for commit: OR  Untracked files:
    modified: OR  untracked: <path>
    ```


### Minimum Viable Product
The MVP will implement most of the Git commands that can run locally. For feasibility, we will implement the Git commands without any flag options, except for some cases, such as '-m' flag for commit command. Most commands will follow the default options. The list of commands to be implemented is as follows:

Command list

Full list of git commands (not part of MVP): https://git-scm.com/docs 

Commands that will be implemented:

- Creating projects
  - init: Creates an empty Git repository or reinitialize an existing one.
   ```
   haskgit init
   ```
- Basic Snapshotting.
  - add: Adds file contents to the index (which means move to staging area).
    ```
    haskgit add filename
    ```
  - status: Displays differences between the index file and the current HEAD commit, paths that have differences between the working tree and the index file, and paths in the working tree that are not tracked by Git.
    ```
    haskgit status
    ```
  - commit: Creates a new commit containing the current contents of the index.
    ```
    -- this will create commit for current staging area
    haskgit commit

    haskgit commit -m "Some commit message"
    ```
  - restore: Restores a file from the last commit to the working directory. (You can also unstage by using --staged, but that would be a stretch goal).
    ```
    haskgit restore <file>
    ```
  - reset: Resets the current HEAD to the specified state.
    ```
    -- this will unstage all changes
    haskgit reset

    -- Move the branch pointer to specific commit
    haskgit reset commit-hash
    ```
  - rm: Remove files from the index.
    ```
    haskgit rm file
    ```
- Branching and Merging
  - branch: Lists, creates, or deletes branches.
    ```
    -- list the branch
    haskgit branch

    -- create new branch
    haskgit branch branch_name
    ```
  - checkout: Switches branches or restores working tree files.
    ```
    haskgit checkout branch-name

    haskgit checkout commit-hash  
    ```

- Inspection and Comparison
  - log: Show commit logs
    ```
    haskgit log
    ```

- Patching
  - rebase: Reapplies commits on top of another base tip. 
    ```console
    -- upstream can be branch or commit reference
    haskgit rebase <upstream>
    ```
  - revert: Reverts some existing commits
    ```
    haskgit revert commit-hash
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
-- GitBlob = (file content in binary, filename)
type GitBlob = (ByteString, String)
```

2. Tree Object

    A tree object represents a directory in a file structure. A tree consists of Blobs (files in the directory) and subtrees (subdirectories).

```haskell
-- GitTree = list of files and subdirectories
type GitTree = [Tree | Blob]
```

3. Commit Object

    A commit object represents each commit made within the repository. It contains the tree object representing the root directory, the parent commit (if it's the first commit, it will be Nothing), author, committer, and commit message.

```haskell
-- Commit = Maybe(tree of root directory, parent commit, author, commiter, commit message, creation time)
type GitCommit = Maybe((GitTree, GitCommit, String, String, String, UTCTime))
```

It will be useful to have some sort of enum that will represent all Git objects, and we would like to store with hash value as well. Therefore, we will also create following types:

```haskell
Data GitObject = GitTree | GitCommit | GitBlob

Data GitObjectHash = (GitObject, ByteString)
```

#### References
In Git, references are labels that point to specific Git objects. For example, when we check out a new branch, there will be a reference pointing to the corresponding Commit object, with the branch name serving as a label. We are going to implement two new types, GitRef and GitRefs, to represent this.

```haskell
-- Ref = (name of pointer - (HEAD, branch name, etc), commit object)
type GitRef = (String, GitCommit)
```

```haskell
-- Refs = list of Ref
type GitRefs = [GitRef]
```

#### Index
In Git, the index acts as an intermediate step between the working directory and the Git repository, serving as a staging area. When we read data from the index file, we need a representation of the 'staged changes' to add them to the commit tree when 'git commit' is called. Since the index represents all the changed files, we can alias the `GitIndex` type as `GitTree` and use this data to create a Commit object later.

```haskell
type GitIndex = GitTree
```

### Key Functions

1. Hashing Git Objects
   
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

    ```haskell
    import qualified Crypto.Hash.SHA1 as SHA1

    -- apply hash function
    sha1Hash = SHA1.hash SomeByteString
    ```

    The current plan for obtaining the hash value for each object type is as follows:

    - For Blob: Hash of (header + new content).
    - For Tree: Hash of (header + concatenation of Blobs and subtrees within Tree)
    - For Commit: Hash of (header + concatenated Tree Object hash + committer name + author name + commit message + creation time + Parent commit hash)
    - Note that header = "{type} #{content.bytesize}\0", e.g. "Blob 139\0"

2. Reading/Writing to and from Object Storage

    We will also implement functions for converting data from the stored data to GitObjects and vice versa. When storing data, we will concatenate the header and data, which will be compressed and stored in the file. For each GitObject, the header and data will follow this format:

      ```
      <!-- Each header will have type, byte length, and null character -->
      -- Blob
      header = "blob #{data.bytesize}\0"
      data =  file content

      -- Tree
      header = "tree #{data.bytesize}\0"
      data = concatenated hashes of subtrees and blobs (have space between)

      -- Commit
      header = "commit #{data.bytesize}\0"
      data = hash of tree of root directory + parent commit hash + author + commiter + commit message + creation time (Spaced out)
      ```

      Again, the header and data will be concatenated and compressed using the `zlib` library and stored in the content. The function for serializing and deserializing data will have the following signature:

      ```haskell
      serializeGitObject ::  GitObject -> ByteString

      deserializeGitObject :: ByteString -> GitObject
      ```

      Both functions will use zlib to compress and decompress the files.

      For reading and writing the files, we will use the `System.IO` library.

3. Instantiating git objects as Show

    We would need a feature to convert git objects to String so we can print on stdout. This feature needs in commands like show and log. To do this, we will make GitObject as instance of type class Show and use show function.

      ``` Haskell
      instance Show GitObject where
        -- Implement the show function
        show Blob = ...  
        show Tree = ...
        show Commit = ...
      ```

      The implementation of show will be based on the type of GitObject. Depending on the type of GitObject, it will return the following string:

      - For Blob, it will return the actual content. (There will be only one string in the list)
      - For Tree, it will return a string of following format:
      
        ```Console
        object_type1 hash_value1 file_name1
        object_type2 hash_value2 file_name2
        object_type3 hash_value3 file_name3
        ...
        (the actual `git cat-file` command includes the file mode, but we will not include this for the project's scope). For example:

        ```Console
        -- sample example
        blob a906cb2a4a904a152e80877d4088654daad0c8599 file1.txt
        blob 8f94139338f9404f26296befa88755fc2598c2893 file2.txt
        tree 23ebdb3b47d0f41f0c9b07b6286e103b971a51c1  subdirectory
        ```
      - For Commit, it will return a string of following format:

        ```Console
        tree hash_value
        parent hash_value
        author author_name
        committer committer_name
        commit_message
        ```

        ```Console
        -- sample example
        tree a906cb2a4a904a152e80877d4088654daad0c8599
        parent 8f94139338f9404f26296befa88755fc2598c2893
        author Someone1
        committer Someone2
        Initial commit
        ```

### Libraries
1. SHA1: https://hackage.haskell.org/package/cryptohash-sha1  
We need to hash various things to implement Git.
2. zlib: https://hackage.haskell.org/package/zlib
Git uses zlib to compress the new content and store files efficiently.
  ```Haskell
   import qualified Data.ByteString.Char8 as C8

  -- convert string to ByteString
  dataToHash :: ByteString
  dataToHash = C8.pack "Hello, world!"
  ```
3. cmdargs

    [cmdargs: Command line argument processing](https://hackage.haskell.org/package/cmdargs-0.10.22)
    cmdargs can be used  to handle the parsing of command-line arguments and the formating of outputs.

    cmdargs will mainly be used for parsing and handling command-line arguments.

    As there is a feature of help built in CmdArgs, we can also employ "haskgit [command] --help" to
      output the usage of each command. Here is a simple and draft example of using "--help".

    ```bash
    cabal build
    cabal v2-exec <your project name> -- --help
    ```

    ```haskell
      -- here resolves problem of "Can't make a derived instance of `Data HaskGit`"
      {-# LANGUAGE DeriveDataTypeable #-}
      import System.Console.CmdArgs

      -- define CLI data type called "HaskGit"
      -- only command of "help", will add more modes on it
      data HaskGit = Help
        deriving (Data, Typeable, Show)

      helpMode :: HaskGit
      -- &= attach attribute to this mode
      helpMode = Help &= help "usage: haskgit [command]"

      main :: IO ()
      -- print available mode (command)
      main = print =<< cmdArgs helpMode
    ```

    ```output
    The haskgit program

    haskgit [OPTIONS]
      usage: haskgit [command]

    Common flags:
      -? --help     Display help message
      -V --version  Print version information
    ```


4. Other libraries: Data.ByteString module, System.IO, Data.Time, etc.

### Testing
  Testing will involve using the [`tasty`](https://hackage.haskell.org/package/tasty) testing framework, including [Tasty-Unit](https://hackage.haskell.org/package/tasty-hunit) which provides HUnit features.


  The following is the plan for testing each component:

  1. Testing Hash Functions
      - Create test cases for Blob, Tree, and Commit.
      - Verify that hash values remain consistent when called multiple times with the same objects (for testing purpouse, we will need to pass in the same time to make hash consistent).
      - When a flag is set to True, confirm that Git objects are stored in the expected directory.

  2. Testing Serialize/Deserialize Functions
      - Test cases for serializing/deserializing Blob, Tree, and Commit.
      - Verify that output of serailization and deserialization are correct

  3. Testing Commands
      - Test cases for each command will be created to check if all Git commands are executed as expected.
      - For example:
        - `haskgit init`: Confirm the creation of a Git directory.
        - `haskgit add`: Verify that files are added to the staging area.
        - `haskgit commit`: Check for the expected creation of commit objects.
      - More specific testing strategies for each command will be discussed further as concrete plans are established for each command.

  4. More test may be added depending on the design changes in the future.


### Checkpoint
  By the checkpoint, we plan to complete following tasks:
  
  - Defining data types for Git Objects, Ref, and Index
  - Argument parsing (set up the structure in `Main.hs`)
  - GitObject hash function
  - Serialize/Deserialize Functions
  - Implementing Show for GitObject
  - Unit test for above functions
  - `git init`, `git add`, and `git status` commands

  We will first focus on establishing main functionality such as argument parsing, hash, serialize/deserialize Functions, and show while creating unit test along the way. After the checkpoint, the git commands in the MVP scope will be split for each member to implment.

### Stretch Goals

In addition to the MVP, the following stretch goals will be accomplished based on available time
* Implement other challenging git commands:
  - `git diff`: Shows changes between commits, commit and working tree, etc.
  - `git fsck` : Verifies the connectivity and validity of the objects in the database.
  - `haskgit merge`: Joins two or more development histories together.
  - `haskgit cherry-pick`: Applies the changes introduced by some existing commits.
* Implement remote repository feature
  - `haskgit push`
  - `haskgit pull`
* Implment some flags for git commands
  - -a for `commit`: Automatically stage and commit all changes in one step.
  - -b for `checkout`: Create and switch to a new branch.
  - -d for `branch`: Delete a branch.
  - Other common flags.

### Areas for Feedback
- We would love to receive feedback on the scope of the project. Do you think this is feasible? Are there any challenging parts that we might be missing?
- Do you have any feedback on our data structures and code structure?
- Is there any library you recommend to use?

### References
- Pro Git: https://git-scm.com/book/en/v2/Git-Internals-Git-Objects 
- Write yourself a Git: https://wyag.thb.lt/
- Git Community Book: https://shafiul.github.io/gitbook/index.html
