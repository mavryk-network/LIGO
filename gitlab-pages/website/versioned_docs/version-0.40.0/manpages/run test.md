
### SYNOPSIS
ligo run test SOURCE_FILE

### DESCRIPTION
This sub-command tests a LIGO contract using a LIGO interpreter. Still under development, there are features that are work in progress and are subject to change. No real test procedure should rely on this sub-command alone.

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--project-root PATH**
The path to root of the project.

**--steps INT**
a bound in the number of steps to be done by the interpreter. (alias: -n)

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively). (alias: -s)

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (hangzhou , ithaca). By default, the current protocol (hangzhou) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


