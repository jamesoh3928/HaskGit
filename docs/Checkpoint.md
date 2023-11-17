# HaskGit

Team members:

- James Oh
- Jack Oh
- Chen Guo

## Summary Description

HaskGit is a Git implementation using Haskell. The goal of the project is to implement core git commands with Haskell.

## Modified MVP
We realized that the scope of the project might have been a little large; hence, we decided to remove some commands from the MVP in the proposal. The removed commands are `git init`, `git rebase`, and `git restore`. We aimed to select commands that are more interesting and do not require handling merge conflicts. Even after removing these commands, we believe the project is still very interesting to explore with Haskell, and the scope of the project remains large. Of course, if time permits, we will implement other interesting commands.

Command list

Commands that will be implemented for MVP:

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
    -- '-m' flag is required for MVP
    haskgit commit -m "Some commit message"
    ```
  - reset: Resets the current HEAD to the specified state. This command will be interesting to implement.
    ```
    -- this will unstage all changes
    haskgit reset

    -- Move the branch pointer to specific commit
    haskgit reset commit-hash
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
  - show: Shows a git object. In the standard 'git show' behavior, it includes the specified commit and the differences. However, for the project's scope, the diff part will be not displayed.

    ```Console
    haskgit show hash_value

    -- show latest commit
    haskgit show  
    ```

  - log: Show commit logs
      ```
      haskgit log
      ```

- Patching
  - revert: Reverts exiting commit. 
    ```
    haskgit revert commit-hash
    ```


## Checkpoint Progress Summary

fter adjusting our approach to explore with small commands and then building up some core functions based on the patterns we found, the parts we implemented are slightly different from the ones proposed in the initial proposal. However, we still believe that we have made significant progress in this project by developing core functions that can be utilized in many other command functions. Of course, there is still a lot of work to be done until we reach the MVP. Here is a brief summary of the progress we have made up to this checkpoint.

1. Modifying design
A lot of things are happening inside Git, so we had to spend a lot of time researching the concept. There were many details we did not realize during the proposal, and there were too many I/Os happening inside Git to follow the "Functional core/Imperative shell" design pattern. If we were to follow this design pattern, we would have to read the entire Git directory into memory for every single Git command. This was not ideal, so we decided to reject such a design pattern and instead follow the simple MVC pattern.

2. Load/Save GitObjects
We implemented a parser that reads various types of Git objects (blob, tree, and commit objects) and created a function to save Git objects from memory to disk. These functions serve as foundational elements, as they can be utilized in any Git command functions that interact with Git objects. The names of these functions are `GitParser.parseGitObject` and `HaskGit.saveGitObject`.

3. `haskgit show` command
The `haskgit show` command is equivalent to the `git show` command with some restrictions (it takes only a hash value as an argument and does not show any diff). This command was the first function we implemented to explore how we could implement other Git commands. The example outputs of the `haskgit show` command are as follows:"

An example of tree show
```
PS C:\Users\james\Documents\CS541\HaskGit> .stack-work/dist/22605e11/build/HaskGit-exe/HaskGit-exe show f6e1af0b636897ed62c8c6dad0828f1172b9b82a
tree f6e1af0b636897ed62c8c6dad0828f1172b9b82a

.gitattributes
.gitignore
README.md
assets
docs
```

An example of commit show
```
PS C:\Users\james\Documents\CS541\HaskGit> .stack-work/dist/22605e11/build/HaskGit-exe/HaskGit-exe show 562c9c7b09226b6b54c28416d0ac02e0f0336bf6
commit 562c9c7b09226b6b54c28416d0ac02e0f0336bf6
Author: James Oh <jo9347@cs.rit.edu>
Date:   Thu Nov  9 14:33:38 2023 -0500

    Change content to bytestring

PS C:\Users\james\Documents\CS541\HaskGit> 
```

4. Read/write index file
This was one of the most challenging parts of the project so far because it was difficult to debug what went wrong when dealing with the binary file. If we misinterpret one byte, the parser will simply fail, and the output binary file will be completely different from the expected output (at least when you look at the encoded version of the file). However, we were able to successfully parse the git version 2 index file (we are not considering any other version for MVP) and reproduce the same index file by parsing the index file and saving it. These functions will be useful for any command that interacts with the index file (e.g., git add, git status, etc).

If you use the `testSaveIndex` file in `Experiment.hs`, it will read the `.git/index` file and load it into memory, and then save it in the `testIndex` file. We were able to reproduce the same index file (if you are testing this locally, make sure your index file is not corrupted or in use).

The Diff command on original index file and reproduced index file:
```
james@DESKTOP-531QK4A MINGW64 ~/Documents/CS541/HaskGit (main)
$ diff .git/index testIndex 

james@DESKTOP-531QK4A MINGW64 ~/Documents/CS541/HaskGit (main)
```


5. `hashObject` function
TODO: Review Jack
The `hashObject` function takes the gitObject as an argument and returns the hash value of the content. This function will be useful when we are implementing the commit command.

6. Locate `.git` directory
TODO: Review Jack
In order to run Git commands, we need to be able to locate the `.git` directory. Git clients climb up the directory hierarchy until they find the `.git` directory. Our `getGitDirectory` function achieves this and will be one of the functions repeatedly used in Git command functions.

7. Implementing `Ref`
TODO: Jack

8. Command Line Parsing
TODO: Review Chen
Our main function can parse the argument and call the function in HaskGit to perform the task. An example command that can be run in the current state of the project is (although you would need to create a `.haskgit` directory in order to test this):"

```
.stack-work/dist/22605e11/build/HaskGit-exe/HaskGit-exe show faf353d2a79d5103cea4090c9d66db3124629d38
```

9. Testing Infrastructure
TODO: Review Chen
We haven't added too many tests yet, but we have set up the testing infrastructure using the Tasty framework. We plan to add more unit tests in the future for each porcelain command we implement. For example, to test `haskgit show`, we will include test git object files and an expected output file to compare if the command produces the expected output.


**Note**: We are aware that the current state of the project may contain some non-clean code. This is because we were focused on exploring different options and implemented the show command first to identify some patterns. In the future, we plan to clean up the code.

## Additional Details

1. Additional Libraries
We have been interacting with the ".cabal" file directly. Dependencies we have added to all library, executable, and test are as follows:

- base16-bytestring: Used for base16 encoding for SHA-1 hash.
- bytestring: Used for interacting with bytestring data.
- cmdargs: Parsing command line arguments.
- cryptohash-sha1: SHA-1 hash.
- directory: Used for finding the current Git directory.
- filepath: Used for file paths.
- parsec: Used for parsing Git object files and Git index files.
- time: Needed for UTC time.
- Tasty: Testing framework.
- zlib: Compression library that Git uses.

2. Code Structure
As mentioned above, we have decided to move away from the "Functional Core and Imperative Shell" design pattern because there are simply too many I/O operations happening in Git. We are still following the MVC pattern, where Main.hs serves as the command-line interface that the user interacts with, HaskGit is the controller file, and all other files act as models that include business logic.

```
src/
|   |-- GitObject.hs -- types and functions related to Git objects
|   |-- GitParser.hs -- parser for git object files and index file
|   |-- Refs.hs -- types and functions related to References
|   |-- Index.hs -- types and functions related to Index
│   │-- HaskGit.hs -- functions for all git porcleain and plumbing commands
|   |-- Experiment.hs -- just a playground for an experiment (planning to delete it in the future)
app/
|
│   |-- Main.hs -- main executable
|
|test/
│   |-- Spec.hs -- Unit tests (may get separeted into multiple files in the future)
```

3. Questions/Feedback
- While we were implementing the parser, we realized it was challenging to pinpoint which step of the parsing failed. Do you have any tips on this?

- Are there specific commands you think will be interesting or challenging? What challenges do you foresee?

- What are your thoughts on the new scope of the project?

- Feel free to provide any other feedback you have on our project! We aim to create an awesome and interesting project, so criticism is welcomed!
