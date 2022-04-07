
### SYNOPSIS
ligo print pretty SOURCE_FILE

### DESCRIPTION
This sub-command pretty-prints a source file in LIGO. The width of the pretty-printed text is adjusted to the number of columns in the terminal (or 60 if it cannot be determined).

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively). (alias: -s)

**--werror**
treat warnings as errors

**-help**
print this help text and exit (alias: -?)


