# HaskGit

Team members:

- James Oh
- Jack Oh
- Chen Guo

## Summary Description

HaskGit is a Git implementation using Haskell. The goal of the project is to implement core git commands with Haskell.

### Why Haskell to implement Git?

- Strong type system and functional paradigm help us to ensure the correctness of Git protocol
- There are multiple advantages of implementing CLI application with Haskell. Check out: https://github.com/Gabriella439/post-rfc/blob/main/sotu.md#scripting--command-line-applications
- TODO: add more

## Additional Details

### Why Haskell to implement Git?
- TODO (probably just explain how we will interact with CLI)
- One or more typical "use cases". These might include "storyboards" explaining
  how a user would interact with the program or some interesting "input/output"
  examples.

### Minimum Viable Product
The MVP will implement most of the git commands that can run locally. The list of commands are following:

Command list
Full list of git commands (not part of MVP): https://git-scm.com/docs 

TODO: add short explanation of each command so we know what we are doing

- Creating projects:
  - init
- Basic Snapshotting:
  - add
  - status
  - commit
  - restore
  - reset
  - rm
- Inspection and Comparison
  - show
  - log
  - diff
- Patching
  - rebase
  - revert

### Code Structure
- TODO: explain that we will follow "Functional core and imperative shell" design pattern

### Key Data Structures
- TODO

### Key Functions
- TODO

### Libraries
- TODO: add all the libraries we are planning to use

### Testing
- TODO: https://hackage.haskell.org/package/HUnit
- Thoughts on testing. These might include critical functions or data structures
  that will be given
  [`tasty`](https://hackage.haskell.org/package/tasty) tests.

### Checkpoint
- TODO: Expected functionality to be completed at the Checkpoint.

### Stretch Goals
- A sketch of intended components (key functions, key data structures, separate
  modules).  To satisfy the "multiple Haskell modules" requirement, it may
  suffice to separate an application into a "model-controller" module and a
  "view" module (e.g., a "text view" module that could be replaced by a "GUI
  view" module).

### References
- Write yourself a Git: https://wyag.thb.lt/
- TODO: add more


Other TODO List
- Check if we missed anything from proposal rubric and final project rubric
- Double check writings

<!-- Proposal Rubric (we also need to check final project rubric as well) -->
<!-- 
One or more typical “use cases”. These might include “storyboards” explaining how a user would interact with the program or some interesting “input/output” examples.
A sketch of intended components (key functions, key data structures, separate modules). To satisfy the “multiple Haskell modules” requirement, it may suffice to separate an application into a “model-controller” module and a “view” module (e.g., a “text view” module that could be replaced by a “GUI view” module).
Thoughts on testing. These might include critical functions or data structures that will be given tasty tests.
Thoughts on a “minimal viable product” and “stretch goals”. Be sure to review the final project grading rubric and consider organizing the project around a core deliverable that will almost certainly be achieved and then a number of extensions and features that could be added to ensure that project is of suitable size/scope/effort.
Expected functionality to be completed at the Checkpoint. -->
