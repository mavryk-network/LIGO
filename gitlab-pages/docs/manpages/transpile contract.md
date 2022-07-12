
### SYNOPSIS
ligo transpile contract SOURCE_FILE SYNTAX

### DESCRIPTION
This sub-command transpiles a source file to another syntax. It does not use the build system, but the source file is preprocessed. Comments are currently not transpiled. Please use at your own risk.

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--output-file FILENAME**
if used, prints the output into the specified file instead of stdout (alias: -o)

**--pascaligo-dialect DIALECT**
the pascaligo dialect that will be used. Currently supported dialects are "terse" and "verbose". By default the dialect is "terse". (aliases: -d, -dialect)

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively). (alias: -s)

**-help**
print this help text and exit (alias: -?)


