# The Preprocessor Library and Standalone Executable

A preprocessor is a tool that reads a text file and copies it. If some
special intructions, called _preprocessing directives_, are found in
the input, the output may not be an identical copy, for example some
parts can be skipped. We document here the preprocessor shipped with
the LIGO compiler.

## Contents

This directory is located in the `vendors` directory of the `git`
repository of LIGO `ligo`. As such, it is distributed with the LIGO
compiler which uses it as a library. Nevertheless, a standalone
preprocessor can also be built and used either as an interactive way
to test the library, or for uses independent of LIGO. The directory
`ligo/vendors/Preprocessor` should list the following files:

```
Preprocessor
├── API.mli
├── API.mll
├── CLI.ml
├── CLI.mli
├── dune
├── dune-project
├── E_AST.ml
├── E_LexerMain.ml
├── E_Lexer.mli
├── E_Lexer.mll
├── E_ParserMain.ml
├── E_Parser.mly
├── LICENSE
├── Makefile.cfg
├── Preprocessor.opam
├── PreprocMainGen.ml
├── PreprocMainGen.mli
├── PreprocMain.ml
├── README.md
├── Stubs
│   └── Preprocessor.ml
└── Tests
     ├── ...
     ...
```

Here is a short description of those files:

  * The OCaml module `API` is the heart of the preprocessor.

  * The modules `CLI` and `PreprocMainGen` deal with command-line
    options for the standalone preprocessor, and also export data
    structures about the configuration meant for the library client,
    for example, the LIGO compiler.

  * The modules whose names start with `E_` are used by `API` to parse
    the boolean expression of conditional directives like `#if`.

  * The module `PreprocMain` is the standalone preprocessor.

  * The file `README.md` is the present file.

  * The `LICENSE` file must contain the MIT license.

  * The directory `Tests` is used for unit tests of the standalone
    preprocessor.

The following files and directories are meant to be used only by a
special Makefile to build the standalone preprocessor. See
[how to build with the Makefile](#makefile).

  * Hidden files prefixed by `.`.

  * The directory `Stubs` is for the build of the standalone
    preprocessor using a Makefile.

  * The file `Makefile.cfg` configure the Makefile.

## Builds

### The Preprocessor as a Library

The relevant files are `dune` and `dune-project`. The shell command

        $ dune build Preprocessor.a

is to be run in the directory `ligo/vendors/Preprocessor`.

### The Standalone Preprocessor with dune

The relevant files are `dune` and `dune-project`. The shell command

        $ dune build PreprocMain.exe

is to be run in the directory `ligo/vendors/Preprocessor`. As usual
with `dune`, the executable is found in the mirror directory tree
`ligo/_build/default/vendors/Preprocessor`.

### The Standalone Preprocessor with Make {#makefile}

It is possible to build the standalone preprocessor with a Makefile
instead of `dune`, in which case you need to clone the following `git`
repositories first, outside the LIGO working directory:

> $ git clone https://github.com/rinderknecht/Scripts.git
> $ git clone https://github.com/rinderknecht/OCaml-build.git

Make sure the directory to the scripts is in your `PATH` shell
variable. Then, back in `ligo/Preprocessor`:

> $ setup.sh

should create symbolic links or report some errors. Running

> $ make conf

should at least find `ocamlc`, `ocamldep`, `menhir`, `grep`, `sed` and
`arch`. If not, install them.

Then run

> $ make sync
> $ make

This should build `PreprocMain.byte` in the directory `./_x86_64/`,
whose name is made by prefixing with `_` the result of the output of
the shell command

> $ uname -a

Note: the Makefile relies on the `opam` database installed by running

> $ dune build

at the root of the LIGO working directory `ligo`.

## The Preprocessing Directives

This preprocessor features different kinds of directives.

  * directives found in the standard preprocessor for the language C#,
    except about strings.

  * a directive from `cpp`, the `C` preprocessor, enabling the textual
    inclusion of files.

  * a directive specific to LIGO to support a minimal module system.

In the following subsections, we shall briefly present those
directives. Here, we state some properties which hold for all of
them.

  * They must start with a `#` symbol at the beginning of a line.

  * They can have arguments in the form of free text or strings.

  * Strings arguments must be enclosed between double quotes and
    cannot span over two or more lines.

  * The valid preprocessing of a directive leaves in its place an
    empty line (that is, a newline character) or another directive, to
    be picked up by other tools, like lexers.

### Conditional Directives and Symbol Definition

The preprocessor enables the conditional copying of its input. At the
start, its default mode is _copy_, meaning that characters in the
input are copied to the output. Conditional directives enable another
mode: _skip_, by means of which the following characters are
discarded, and only newline characters are copied.

The conditional directives are `#if`, taking a boolean expression as
argument, and `#endif`, which closes the conditional. We can also make
use of an `#else` directive. The minimal example would be as follows:

```
#if false
This is NOT copied to the output, except the newline character
#endif
```

where `false` is a predefined symbol acting like a boolean value. The
output is

```
# 1 "Tests/test.txt"




```

Note what looks like an anonymous preprocessing directive. We will
explain its meaning when presenting
[The Inclusion Directive](#include). The blank lines differ from the
output of `cpp`. Their use is clearer if we add text before and after
the conditional, like so:

```
---
#if false
A
#endif
---
```

whose preprocessed output is then

```
# 1 "Tests/test.txt"
---



---
```

Here is how to use the `#else` directive:

```
#if false
This is NOT copied to the output, except the newline character.
#else
This IS copied to the output.
#endif
```

The booleans `false` and `true` are predefined. The way to define
symbols (that is the traditional name of those identifiers) consists
in using the `#define` directive, followed by the symbol, like so:

```
#define MY_FALSE

#if MY_FALSE
This is NOT copied to the output, except the newline character.
#else
This IS copied to the output.
#endif
```

This opens the possibiliy to boolean expression made of
  * `true` and `false` already mentioned;
  * `||` for the disjunction ("or");
  * `&&` for the conjunction ("and");
  * `==` for equality;
  * `!=` for inequality;
  * `!` for negation;
  * `(` and `)` around expressions to specify priorities.

When we want to write a cascade of if-then-else, the preprocessor a
shortcut by means of the `#elif` directive, like so:

```
#if ...
...
#elif ...
...
#elif ...
...
#endif
```

Basically, a `#elif` directive is equivalent to `#else` followed by
`#if`, but we only need to close with only one `#endif`.

### The Error Directive

### The Inclusion Directive {#include}

### The Import Directive


## Preprocessing Strings and Comments


## Documenting the Modules

### CLI

### API

### PreprocMainGen

### PreprocMain


## Maintaining the Preprocessor {#maintenance}

### Adding a Command-Line Option

### Adding a Preprocessing Directive

### Adding a Comment or String
