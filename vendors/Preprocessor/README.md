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
[how to build with the Makefile](#the-standalone-preprocessor-with-make).

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

### The Standalone Preprocessor with Make

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

## The Standalone Preprocessor

Let us assume here that the standalone preprocessor has been [built
with `dune`](#the-standalone-preprocessor-with-dune), therefore the
executable is `PreprocMain.exe`. We can obtain the available
command-line options like so:

    $ PreprocMain.exe --help

resulting in the output

```
Usage: PreprocMain.exe [<option> ...] -- [<input>]
where <input> is the source file (default: stdin),
and each <option> (if any) is one of the following:
  -I <paths>       Inclusion paths (colon-separated)
  -h, --help       This help
  -v, --version    Commit hash on stdout
      --cli        Print given options (debug)
      --columns    Columns for source locations
      --show-pp    Print result of preprocessing
```

First, we see that the preprocessor follows the
[GNU getopt](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)
conventions on short and long option names. The input is an anonymous
argument that must be preceeded by `--`. The options are

  * `-I <paths>` for finding files given to the directive `#include`
    which are not found by appending their path to the current
    directory.

  * `--columns` for using column numbers instead of horizontal
    offsets when reporting errors in the input (the first character
    on a line is on the column 1, but has offset 0).

  * `--show-pp` for actually printing on `stdin` the result of
    preprocessing the input. If not given this option, the
    preprocessor will only print on `stderr` errors, if any.

## The Preprocessing Directives

This preprocessor features different kinds of directives:

  * directives found in the standard preprocessor for the language
    `C#`;

  * a directive from `cpp`, the `C` preprocessor, enabling the textual
    inclusion of files;

  * a directive specific to LIGO to support a minimal module system.

Importantly, [strings](#preprocessing-strings-and-comments) are
handled the way `cpp` does, not `C#`.

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

### The Error Directive

When debugging or putting in place directives in an already existing
input, it is sometimes useful to force the preprocessor to stop and
emit an error. This is possible thanks to the `#error` directive,
which is followed by an error message as free text until the end of
the line, like so:

```
#error Not implemented/tested yet
```

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
[The Inclusion Directive](#the-inclusion-directive). (Remark: `cpp`
would not output blank lines followed by the end of the file.) Their
use is clearer if we add text before and after the conditional, like
so:

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

The rationale for using conditional directives in LIGO is to enable in
a single smart contract several versions of a standard.

```
#if STANDARD_1
...
#elif STANDARD_2
...
#else
#error Standard not implemented
#endif
```

A real life example could be
[Dexter](https://gitlab.com/dexter2tz/dexter2tz/-/blob/febd360cf6df6e090dedbf21b27538681246f980/dexter.mligo#L52). It
provides another interesting use of a conditional directive, where
[a record type depends on the version of the standard](https://gitlab.com/dexter2tz/dexter2tz/-/blob/febd360cf6df6e090dedbf21b27538681246f980/dexter.mligo#L84).

### The Inclusion Directive

The solution provided by the conditional directives with symbol
definition to manage several standards is improved upon by physically
separating the input into different files. This is where the
`#include` directive comes handy. Basically, it takes an argument made
of a string containing a path to the file to be textually included,
like so:

```
#include "path/to/standard_1.ligo`
```

and the preprocessor replaces the directive with the contents of the
file `path/to/standard_1.ligo`, whose contents is then preprocessed as
well. This can in theory create a loop, for example, if two files try
to include each other.

In fact, the preprocessor does more than simply include the given
file. To enable the consumer of the output to keep track of
inclusions, in particular, to maintain the line numbers of the input
that has been copied, the preprocessor inserts two special directives
in the output, called
[linemarkers](https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html),
one in the stead of the `#include` directive and one after the
inclusion. Let us consider the following example where `a.txt`
includes `b.txt`, which, in turn, includes `c.txt`. Here is the
contents of `a.txt`:

```
Start of "a.txt"
#include "b.txt"
End of "a.txt"
```

Then `b.txt`:

```
Start of "b.txt"
#include "c.txt"
End of "b.txt"
```

and, finally, `c.txt`:

```
Start of "c.txt"
End of "c.txt"
```

If we gather the files in a `Tests` directory and run the
[standalone preprocessor built with `dune`](#the-standalone-preprocessor-with-dune)
like so

> $ PreprocMain.exe --show-pp -- Tests/a.txt

we obtain on `stdout`:

```
# 1 "Tests/a.txt"
Start of "a.txt"

# 1 "Tests/b.txt" 1
Start of "b.txt"

# 1 "Tests/c.txt" 1
Start of "c.txt"
End of "c.txt"
# 3 "Tests/b.txt" 2
End of "b.txt"
# 3 "Tests/a.txt" 2
End of "a.txt"
```

There are three forms of linemarkers:

  1. `# <line number> "path/to/file"`
  2. `# <line number> "path/to/file" 1`
  2. `# <line number> "path/to/file" 2`

The first kind is used only at the start of the output file and states
that the line after the linemarker has number `<line number>` and
belongs to the file `path/to/file`. Therefore `Start of "a.txt"` has
line number `1` in file `Tests/a.txt`.

The second kind is used when including a file. The `#include`
directive is discarded in the output, except the newline character,
which explains the empty line after `Start of "a.txt"`. Then a
linemarker ending in `1` is printed, which means that we went to the
file `path/to/file` when processing the input.

The third kind is inserted in the output upon returning from an
included file. For example, `# 3 "Tests/b.txt" 2` means that the next
line has number `3` and we return to file `Tests/b.txt`.

Linemarkers need to be handled by the consumer of the output. In the
context of the LIGO compiler, the lexer reads the output of the
preprocessor, therefore scans for linemarkers.

When using the preprocessor with the LIGO compiler, the `#include`
directive can only occur at the top level according to the grammar,
that is, either at the beginning of the smart contract, in between
file-level declarations or at the end. (This property is checked by
the parser.) The rationale for this restriction is to avoid fragments
of smart contracts that are syntactically incorrect, and yet assembled
into a correct one.

### The Import Directive

The `#import` directive is specific to the LIGO compiler. It provides
the support for a
[minimal module system](https://ligolang.org/docs/language-basics/modules#modules-and-imports-build-system).


## Preprocessing Strings and Comments

Strings and comments are recognised by the preprocessor, even in
pieces of the input that are not copied. (This last point is a
difference between `cpp` and the `C#` preprocessor.) The rationale for
doing so is twofold:

  1. We do not want the preprocessor to interpret a directive that is
     actually in a comment. This can happen when commenting out a
     piece of the source code that contains a preprocessing directive:
     we do not want that directive to be interpreted.

  2. We do not want the preprocessor to interpret a directive that is
     actually in a string. This can happen if the source code is that
     of a bootstrapped compiler, that is, a compiler for its own
     language. Another scenario is that of a test: the source code is
     actually printing what is happening.

## Documenting the Modules

### CLI

As we explained in section [Contents](#contents), the module `CLI` has
a twofold purpose: to deal with command-line options for the
standalone preprocessor, and also export data structures about the
configuration meant for the library client, for example, the LIGO
compiler.

The module signature
[COMMENTS](https://gitlab.com/ligolang/ligo/-/blob/998107d5f0098c8acc86f8950f2a0f9fc5836f5d/vendors/Preprocessor/CLI.mli#L10)
exports optional values denoting two kinds of comments: one-line
comments and block comments (that is, multi-line comments). Due to the
use of metaprogramming by means of `ocamllex` to implement the module
`API`, where comments are defined as regular expressions, the client
of `CLI` must make sure to choose line and block comments that are
actually recognised by `API`.

The module signature
[CLI.S](https://gitlab.com/ligolang/ligo/-/blob/998107d5f0098c8acc86f8950f2a0f9fc5836f5d/vendors/Preprocessor/CLI.mli#L23)
gathers data structures parameterising the behaviour of the
preprocessor. If building the standalone preprocessor, those data will
come from the command-line. They are

```
    val input     : string option (* input file             *)
    val extension : string option (* file extension         *)
    val dirs      : string list   (* -I                     *)
    val show_pp   : bool          (* --show-pp              *)
    val offsets   : bool          (* negation of --columns  *)
```

So, for example, if `offsets` is `true`, error messages will report
regions of the input with horizontal offsets (a la Emacs) instead of
column numbers (a la Vim).

Information about how the parsing of command-line options went is
given by the type and value `status`:

```
    type status = [
      `Done
    | `Version      of string
    | `Help         of Buffer.t
    | `CLI          of Buffer.t
    | `SyntaxError  of string
    | `FileNotFound of string
    ]

    val status : status
```

The constructor `` `Version`` carries the commit hash of the source
code, as shown about
[the standalone preprocessor](#the-standalone-preprocessor). The ``
`Help`` constructor carries the help displayed by `--help`.

### API

### PreprocMainGen

### PreprocMain


## Error messages




## Maintaining the Preprocessor

### Adding a Command-Line Option

### Adding a Preprocessing Directive

### Adding a Comment or String
