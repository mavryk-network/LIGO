# Architecture of the LIGO front-end

## Passes and Stages

The architecture of the LIGO compiler is standard in the sense that it
is *a pipeline of passes*. The product of a pass is called a
*stage*. Each pass can use external libraries and vendored libraries,
the difference being that the former are managed by `opam` whereas the
latter by `dune` and are distributed along the LIGO compiler with the
same MIT license.

Here is the directory structure that contains the front-end:

```
.
├──vendors
│  ├── LexerLib
│  ├── ligo-utils
│  │   ├── simple-utils
│  │   ...
│  ├── ParserLib
│  ├── Preprocessor
│  ...
├── src
... ├── passes
    │   ├── 00-preprocessing
    │   ├── 01-lexing
    │   ├── 02-parsing
    │   ├── 03-self-cst
    │   ├── 04-tree_abstraction
    │   ...
    ├── stages
    │   ├── 1-cst
    │   ├── 2-ast_imperative
    │   ...
    ...
```

Unfortunately, there is no consistency between the numbering of the
passes and that of the stages, so `1-cst` is the stage after
("produced by") pass `02-parsing`, and `2-ast_imperative` is after
`04-tree_abstraction`. (In passing, there is no consistency on the use
of underscores versus dashes.) Notice the pass `03-self-cst` whose
name suggests that its output is the same stage as its input. Other
passes make use of this structure too, that is, a proper pass followed
by a self-pass. The purpose of a self-pass could be to filter-out,
normalise or optimise some constructs, without cluttering the
associated pass.

As the architecture of the compiler is a pipeline, each pass depends
on the previous ones, but sometimes a previous pass can be disabled
for debugging purposes, for example preprocessing.

Passes usually depend on one or more vendored libraries:

  * pass `00-preprocessing` depends on the vendored library
    `Preprocessor`;

  * pass `01-lexing` depends on `Preprocessor` and `LexerLib`;

  * pass `02-parsing` depends on `Preprocessor`, `LexerLib` and
    `ParserLib`.

Historically, those vendored libraries were part of some passes and
were redesigned to become independent. Each one of them, let us say
`MyLib`, follows the simple convention of having

  * a file `MyLib.opam`;

  * files `dune` and `dune-project`

Optionally, we may find

  * a `LICENSE` file (ought to be MIT);

  * a `README.md` file;

  * a `*Main.ml` OCaml if the library can be used to build a
    standalone executable for testing purposes.
