# The Preprocessor Library and Standalone Executable

A preprocessor is a tool that reads a text file and, if some special
intructions, called _preprocessing directives_, are found in the
input, the output may not be an identical copy, for example some parts
can be skipped. We document here the preprocessor shipped with the
LIGO compiler.

## Contents

This directory is located in the `vendors` directory of the repository
of LIGO, here `ligo/vendors`. As such, it is distributed with the LIGO
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
└── Tests
     ├── ...
     ...
```

Here is a short description of those files:

  * The OCaml module `API` is the heart of the preprocessor.

  * The modules whose names start with `E_` are used by `API` to parse
    the boolean expression of conditional directives `#if` and
    `#elif`.

  * The modules `CLI` and `PreprocMainGen` deal with command-line
    options for the standalone preprocessor, and also export data
    structures about the configuration meant for the library client,
    for example, the LIGO compiler.

  * The module `PreprocMain` is the standalone preprocessor.

  * The directory `Tests` is used for unit tests of the standalone
    preprocessor.

  * The file `README.md` is the present file.

  * The `LICENSE` file must contain the MIT license.

The following files are meant to be used only by a special Makefile to
build the standalone preprocessor. See
[how to build with the Makefile](#makefile).

  * Hidden files prefixed by `.`.

  * The file `Makefile.cfg` configures the Makefile.

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
variable. Then, back in `ligo/vendors/Preprocessor`, run

> $ setup.sh

which should create symbolic links or report some errors. Running

> $ make conf

should at least find `ocamlc`, `ocamldep`, `menhir`, `grep`, `sed` and
`arch`. If not, install them.

Then run

> $ make sync

> $ make

which is expected to build `PreprocMain.byte` in the directory
`./_x86_64/`, whose name is made by prefixing with `_` the result of
the output of the shell command

> $ uname --machine

Note: the Makefile relies on the `opam` database installed by
previously running

> $ make

at the root of the LIGO working directory `ligo`, or, at least,

> $ install_vendors_deps.sh

in `ligo/scripts`.


## The Preprocessing Directives

This preprocessor features different kinds of directives:

  * directives found in the standard preprocessor for the language C#;

  * a directive from `cpp`, the `C` preprocessor, enabling the textual
    inclusion of files;

  * a directive specific to LIGO to support a minimal module system.

Importantly, [strings](#strings) are handled the way `cpp` does, not
`C#`.

In the following subsections, we shall briefly present those
directives. Here, we state some properties which hold for all of
them.

  * They must start with a `#` symbol at the beginning of a line.

  * Wrongly spelled directives or unsupported ones are ignored without
    warning, and therefore will appear in the output.

  * They can have arguments in the form of free text or
    strings. (Anything after the directive name is considered a
    potential argument.)

  * Strings arguments must be enclosed between double quotes and
    cannot span over two or more lines.

  * The valid preprocessing of a directive leaves in its place an
    empty line (that is, a newline character) or another directive, to
    be picked up by other tools, like lexers.

  * Newline characters are never discarded, to preserve the line
    numbers of copied text.

### Conditional Directives and Symbol Definition

The preprocessor enables the conditional copying of its input. At the
start, its default mode is _copy_, meaning that characters in the
input are copied to the output. Conditional directives enable another
mode: _skip_, by means of which the following characters are
discarded, and only newline characters are copied.

Conditional directives follow the familiar syntax of some of their
cousins in programming languages. At the very least,

  1. they start with the `#if` directive, followed by a boolean
     expression as argument,

  2. and they are closed by `#endif`.

It is also possible to use

  * one `#else` directive before `#endif`;

  * a series of `#elif` directive after `#if` (as a short-hand for a
    `#else` immediately followed by an `#if`, except that only one
    `#endif` will close the conditional).

A trivial example would be:


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

Note what looks like an anonymous preprocessing directive `# 1
"Tests/test.txt"`. We will explain its meaning when presenting
[The Inclusion Directive](#include). (Remark: `cpp` would not output
blank lines followed by the end of the file.) Their use is clearer if
we add text before and after the conditional, like so:

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
_symbols_ (that is the traditional name of those identifiers) consists
in using the `#define` directive, followed by the symbol, like so:

```
#define SYM

#if SYM
This IS copied to the output.
#else
This is NOT copied to the output, except the newline character.
#endif
```

This opens the possibiliy to use boolean expressions made of
  * `true` and `false` already mentioned;
  * `||` for the disjunction ("or");
  * `&&` for the conjunction ("and");
  * `==` for equality;
  * `!=` for inequality;
  * `!` for negation;
  * `(` and `)` around expressions to specify priorities.

Directives are processed in sequence in the input file. This
preprocessor, like that of `C#`, allows us to _undefine_ a symbol,
that is, giving it the boolean value `false`, like so:

```
#define SYM
#undef SYM

#if SYM
This is NOT copied to the output, except the newline character.
#else
This IS copied to the output.
#endif
```

When we want to write a cascade of conditionals, the preprocessor
enables a shortcut by means of the `#elif` directive, like so:

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

When debugging or putting in place directives in an already existing
input, it is sometimes useful to force the preprocessor to stop and
emit an error. This is possible thanks to the `#error` directive,
which is followed by an error message as free text until the end of
the line, like so:

```
#error Not implemented/tested yet
```

### The Inclusion Directive {#include}




### The Import Directive {#import}


## Preprocessing Strings and Comments {#strings}


## Documenting the Modules

### CLI

### API

### PreprocMainGen

### PreprocMain


## Maintaining the Preprocessor {#maintenance}

### Adding a Command-Line Option

### Adding a Preprocessing Directive

### Adding a Comment or String
