
### SYNOPSIS
ligo run evaluate-expr SOURCE_FILE

### DESCRIPTION
This sub-command evaluates a LIGO definition. The context is initialized from a source file where the definition is written. The interpretation is done using a Michelson interpreter.

### FLAGS
**--amount INT**
the tezos amount the Michelson interpreter will use for the transaction.

**--balance INT**
the balance the Michelson interpreter will use for the contract balance.

**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--no-warn**
disable warning messages

**--now TIMESTAMP**
the NOW value the Michelson interpreter will use (e.g. '2000-01-01T10:10:10Z')

**--project-root PATH**
The path to root of the project.

**--sender ADDRESS**
the sender the Michelson interpreter transaction will use.

**--source ADDRESS**
the source the Michelson interpreter transaction will use.

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively). (alias: -s)

**--werror**
treat warnings as errors

**-e ENTRY-POINT**
the entry-point that will be compiled. (alias: --entry-point)

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (hangzhou , ithaca). By default, the current protocol (hangzhou) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


