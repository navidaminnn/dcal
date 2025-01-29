# Developer Guide

Follow these steps to setup and test the project:

1. **Clone the Repository**  
   Clone this repository to your local machine:

```bash
git clone <https://github.com/DistCompiler/dcal.git>
```

2. **Install Scala CLI**
   Install Scala CLI using the using this [link](https://scala-cli.virtuslab.org/install/)
3. **Install Scala(Metals)**
   Install the Scala(Metals) plugin by Scalameta in VSCode.

4. **Verify Installation**
   After installation, close and reopen your terminal, then verify Scala CLI is installed by running:

```bash
scala-cli version
```

5. **Run Tests**
   After veryifying installation, build and run the tests using:

```bash
scala-cli test .
```

6. **Debugging**
   Use the debug adapter for troubleshooting and debugging. Refer to the documentation for setup and usage instructions.

# DCal Compiler Project

This is a combination of a compiler and a compiler construction toolkit - mostly the toolkit as of writing.
The goal of DCal is to produce a combined compilation and metaprogramming system that can help developers go from formal specifications in TLA+ (-adjacent language) to optimized, customized distributed system implementations.

This repository is public for collaboration purposes, and makes no promises of completion.
This message will be replaced when the compiler prototype has reached some degree of completion.

For now, repository contains:
- `Node`: a generic AST class inspired by the Trieste project. AST nodes are tagged with Token objects, whose pointer identity makes for easy pattern matching. The structure is mutable, and is designed to have well defined behavior despite this fact (should be hard to create corrupt trees).
- `Manip`: an _applicative functor_ that is used for tree traversal and modification. Manip supports context-passing, backtracking search with commit points, and flatMap for occasions when monadic behavior is needed.
- `SeqPattern`: a wrapper around Manip that handles pattern matching sequences of sibling nodes in an AST. Used with `dsl.on(...)` and `dsl.pass`, patterns let you express rewrite rulesets.
- `Source` and `SourceRange`: byte-level views over files, wrapping `java.nio` buffers. Contains added Scala sequence syntax, no-copy slicing, line/column number calculation, and source location pretty-printing.
- `Wellformed`: AST specification and validation. You write what your AST should look like in an EBNF-like syntax and it:
  1. will check that syntax for you if you ask, wrapping bad cases in Error nodes. Node has facilities for efficiently detecting Error subtrees and iterating over them.
  2. can serialize your AST to and from s-expressions, either to look at it, or for interchange.

Outside of those notable classes and some utilities, there are 2 important subfolders:
- `sexpr`: facilities for reading and writing S-Expressions. Mostly for use by Wellformed, but tested on a necessary-seeming subset of the original proposed RFC.
- `tla`: under-construction parser for the TLA+ modeling language, which has been invaluable in stress-testing all the generic machinery. The test suite involves trying to parse the full TLA+ Examples folder.
