open Cli_expect

let%expect_test _ =
  run_ligo_good [ "-help" ] ;
  [%expect {|
    The LigoLANG compiler

      ligo SUBCOMMAND

    === subcommands ===

      compile    compile a ligo program to michelson
      transpile  transpile ligo code from a syntax to another (BETA)
      run        compile and interpret ligo code
      info       tools to get information from contracts
      mutate     create mutants of a ligo file
      repl       interactive ligo interpreter
      changelog  print the ligo changelog
      print      print intermediary program representation.
                 Warning: Intended for development of LIGO and can break at any time
      install    install ligo packages declared in package.json
      version    print version information
      help       explain a given subcommand (perhaps recursively)

    (src/command.ml.Exit_called (status 0)) |} ]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; "-help" ] ;
  [%expect {|
    compile a contract.

      ligo compile contract SOURCE_FILE

    This sub-command compiles a contract to Michelson code. It expects a source file and an entrypoint function that has the type of a contract: "parameter * storage -> operations list * storage".

    === flags ===

      [--constants CONSTANTS]                  A list of global constants that will
                                               be assumed in the context, separated
                                               by ','
                                               (alias: -c)
      [--disable-michelson-typechecking]       Disable Michelson typecking, this
                                               might produce ill-typed Michelson
                                               code.
      [--display-format FORMAT]                the format that will be used by the
                                               CLI. Available formats are 'dev',
                                               'json', and 'human-readable'
                                               (default). When human-readable lacks
                                               details (we are still tweaking it),
                                               please contact us and use another
                                               format in the meanwhile.
                                               (alias: --format)
      [--enable-michelson-typed-opt]           Enable Michelson optimizations that
                                               work using typecking.
      [--file-constants FILE_CONSTANTS]        A file with a JSON list of strings
                                               with Michelson code. Those Michelson
                                               values will be registered as global
                                               constants in the context.
      [--michelson-comments COMMENT_TYPE] ...  Selects kinds of comments to be added
                                               to the Michelson output. Currently
                                               'location' and 'env' are supported.
                                               'location' propagates original source
                                               locations. 'env' inserts additional
                                               empty Seq nodes with comments
                                               relating the Michelson stack to the
                                               source LIGO environment.
      [--michelson-format CODE_FORMAT]         format that will be used by
                                               compile-contract for the resulting
                                               Michelson. Available formats are
                                               'text' (default), 'json' and 'hex'.
      [--no-warn]                              disable warning messages
      [--output-file FILENAME]                 if used, prints the output into the
                                               specified file instead of stdout
                                               (alias: -o)
      [--project-root PATH]                    The path to root of the project.
      [--syntax SYNTAX]                        the syntax that will be used.
                                               Currently supported syntaxes are
                                               "pascaligo", "cameligo", "reasonligo"
                                               and "jsligo". By default, the syntax
                                               is guessed from the extension (.ligo,
                                               .mligo, .religo, and .jsligo
                                               respectively).
                                               (alias: -s)
      [--views VIEWS]                          A list of declaration name that will
                                               be compiled as on-chain views,
                                               separated by ','
                                               (alias: -v)
      [--warn-unused-rec]                      warn about unused recursion in a
                                               recursive function
      [--werror]                               treat warnings as errors
      [-e ENTRY-POINT]                         the entry-point that will be
                                               compiled.
                                               (alias: --entry-point)
      [-p PROTOCOL]                            choose protocol's types/values
                                               pre-loaded into the LIGO environment
                                               (jakarta ,
                                               ithaca). By default, the current
                                               protocol (ithaca) will be used
                                               (alias: --protocol)
      [-help]                                  print this help text and exit
                                               (alias: -?)

    (src/command.ml.Exit_called (status 0)) |} ]

let%expect_test _ =
  run_ligo_good [ "compile" ; "parameter" ; "-help" ] ;
  [%expect {|
    compile parameters to a Michelson expression.

      ligo compile parameter SOURCE_FILE PARAMETER_EXPRESSION

    This sub-command compiles a parameter for a given contract to a Michelson expression. The resulting Michelson expression can be passed as an argument in a transaction which calls a contract.

    === flags ===

      [--amount INT]                     the tezos amount the Michelson interpreter
                                         will use for the transaction.
      [--balance INT]                    the balance the Michelson interpreter will
                                         use for the contract balance.
      [--constants CONSTANTS]            A list of global constants that will be
                                         assumed in the context, separated by ','
                                         (alias: -c)
      [--display-format FORMAT]          the format that will be used by the CLI.
                                         Available formats are 'dev', 'json', and
                                         'human-readable' (default). When
                                         human-readable lacks details (we are still
                                         tweaking it), please contact us and use
                                         another format in the meanwhile.
                                         (alias: --format)
      [--file-constants FILE_CONSTANTS]  A file with a JSON list of strings with
                                         Michelson code. Those Michelson values will
                                         be registered as global constants in the
                                         context.
      [--michelson-format CODE_FORMAT]   format that will be used by
                                         compile-contract for the resulting
                                         Michelson. Available formats are 'text'
                                         (default), 'json' and 'hex'.
      [--no-warn]                        disable warning messages
      [--now TIMESTAMP]                  the NOW value the Michelson interpreter
                                         will use (e.g. '2000-01-01T10:10:10Z')
      [--output-file FILENAME]           if used, prints the output into the
                                         specified file instead of stdout
                                         (alias: -o)
      [--project-root PATH]              The path to root of the project.
      [--sender ADDRESS]                 the sender the Michelson interpreter
                                         transaction will use.
      [--source ADDRESS]                 the source the Michelson interpreter
                                         transaction will use.
      [--syntax SYNTAX]                  the syntax that will be used. Currently
                                         supported syntaxes are "pascaligo",
                                         "cameligo", "reasonligo" and "jsligo". By
                                         default, the syntax is guessed from the
                                         extension (.ligo, .mligo, .religo, and
                                         .jsligo respectively).
                                         (alias: -s)
      [--warn-unused-rec]                warn about unused recursion in a recursive
                                         function
      [--werror]                         treat warnings as errors
      [-e ENTRY-POINT]                   the entry-point that will be compiled.
                                         (alias: --entry-point)
      [-p PROTOCOL]                      choose protocol's types/values pre-loaded
                                         into the LIGO environment (jakarta ,
                                         ithaca). By default, the current protocol
                                         (ithaca) will be used
                                         (alias: --protocol)
      [-help]                            print this help text and exit
                                         (alias: -?)

    (src/command.ml.Exit_called (status 0)) |} ]

let%expect_test _ =
  run_ligo_good [ "compile"; "storage" ; "-help" ] ;
  [%expect {|
    compile an initial storage in LIGO syntax to a Michelson expression.

      ligo compile storage SOURCE_FILE STORAGE_EXPRESSION

    This sub-command compiles an initial storage for a given contract to a Michelson expression. The resulting Michelson expression can be passed as an argument in a transaction which originates a contract.

    === flags ===

      [--amount INT]                     the tezos amount the Michelson interpreter
                                         will use for the transaction.
      [--balance INT]                    the balance the Michelson interpreter will
                                         use for the contract balance.
      [--constants CONSTANTS]            A list of global constants that will be
                                         assumed in the context, separated by ','
                                         (alias: -c)
      [--display-format FORMAT]          the format that will be used by the CLI.
                                         Available formats are 'dev', 'json', and
                                         'human-readable' (default). When
                                         human-readable lacks details (we are still
                                         tweaking it), please contact us and use
                                         another format in the meanwhile.
                                         (alias: --format)
      [--file-constants FILE_CONSTANTS]  A file with a JSON list of strings with
                                         Michelson code. Those Michelson values will
                                         be registered as global constants in the
                                         context.
      [--michelson-format CODE_FORMAT]   format that will be used by
                                         compile-contract for the resulting
                                         Michelson. Available formats are 'text'
                                         (default), 'json' and 'hex'.
      [--no-warn]                        disable warning messages
      [--now TIMESTAMP]                  the NOW value the Michelson interpreter
                                         will use (e.g. '2000-01-01T10:10:10Z')
      [--output-file FILENAME]           if used, prints the output into the
                                         specified file instead of stdout
                                         (alias: -o)
      [--project-root PATH]              The path to root of the project.
      [--sender ADDRESS]                 the sender the Michelson interpreter
                                         transaction will use.
      [--source ADDRESS]                 the source the Michelson interpreter
                                         transaction will use.
      [--syntax SYNTAX]                  the syntax that will be used. Currently
                                         supported syntaxes are "pascaligo",
                                         "cameligo", "reasonligo" and "jsligo". By
                                         default, the syntax is guessed from the
                                         extension (.ligo, .mligo, .religo, and
                                         .jsligo respectively).
                                         (alias: -s)
      [--warn-unused-rec]                warn about unused recursion in a recursive
                                         function
      [--werror]                         treat warnings as errors
      [-e ENTRY-POINT]                   the entry-point that will be compiled.
                                         (alias: --entry-point)
      [-p PROTOCOL]                      choose protocol's types/values pre-loaded
                                         into the LIGO environment (jakarta ,
                                         ithaca). By default, the current protocol
                                         (ithaca) will be used
                                         (alias: --protocol)
      [-help]                            print this help text and exit
                                         (alias: -?)

    (src/command.ml.Exit_called (status 0)) |} ]

let%expect_test _ =
  run_ligo_good [ "compile" ; "constant" ; "-help" ] ;
  [%expect {|
    compile constant to a Michelson value and its hash.

      ligo compile constant SYNTAX _EXPRESSION

    This sub-command compiles a LIGO expression to a Michelson value and its hash as a global constant. It works by compiling the LIGO expression to a Michelson expression and then interpreting it using Michelson's interpreter.

    === flags ===

      [--display-format FORMAT]  the format that will be used by the CLI. Available
                                 formats are 'dev', 'json', and 'human-readable'
                                 (default). When human-readable lacks details (we
                                 are still tweaking it), please contact us and use
                                 another format in the meanwhile.
                                 (alias: --format)
      [--init-file FILENAME]     the path to the smart contract file to be used for
                                 context initialization.
      [--no-warn]                disable warning messages
      [--project-root PATH]      The path to root of the project.
      [--warn-unused-rec]        warn about unused recursion in a recursive function
      [--werror]                 treat warnings as errors
      [--without-run]            disable running of compiled expression.
      [-p PROTOCOL]              choose protocol's types/values pre-loaded into the
                                 LIGO environment (jakarta ,
                                 ithaca). By default, the current protocol (ithaca)
                                 will be used
                                 (alias: --protocol)
      [-help]                    print this help text and exit
                                 (alias: -?)

    (src/command.ml.Exit_called (status 0)) |} ]

let%expect_test _ =
  run_ligo_good [ "run" ; "dry-run" ; "-help" ] ;
  [%expect {|
    run a smart-contract with the given storage and input.

      ligo run dry-run SOURCE_FILE PARAMETER_EXPRESSION STORAGE_EXPRESSION

    This sub-command runs a LIGO contract on a given storage and parameter. The context is initialized from a source file where the contract is implemented. The interpretation is done using Michelson's interpreter.

    === flags ===

      [--amount INT]             the tezos amount the Michelson interpreter will use
                                 for the transaction.
      [--balance INT]            the balance the Michelson interpreter will use for
                                 the contract balance.
      [--display-format FORMAT]  the format that will be used by the CLI. Available
                                 formats are 'dev', 'json', and 'human-readable'
                                 (default). When human-readable lacks details (we
                                 are still tweaking it), please contact us and use
                                 another format in the meanwhile.
                                 (alias: --format)
      [--no-warn]                disable warning messages
      [--now TIMESTAMP]          the NOW value the Michelson interpreter will use
                                 (e.g. '2000-01-01T10:10:10Z')
      [--project-root PATH]      The path to root of the project.
      [--sender ADDRESS]         the sender the Michelson interpreter transaction
                                 will use.
      [--source ADDRESS]         the source the Michelson interpreter transaction
                                 will use.
      [--syntax SYNTAX]          the syntax that will be used. Currently supported
                                 syntaxes are "pascaligo", "cameligo", "reasonligo"
                                 and "jsligo". By default, the syntax is guessed
                                 from the extension (.ligo, .mligo, .religo, and
                                 .jsligo respectively).
                                 (alias: -s)
      [--warn-unused-rec]        warn about unused recursion in a recursive function
      [--werror]                 treat warnings as errors
      [-e ENTRY-POINT]           the entry-point that will be compiled.
                                 (alias: --entry-point)
      [-p PROTOCOL]              choose protocol's types/values pre-loaded into the
                                 LIGO environment (jakarta ,
                                 ithaca). By default, the current protocol (ithaca)
                                 will be used
                                 (alias: --protocol)
      [-help]                    print this help text and exit
                                 (alias: -?)

    (src/command.ml.Exit_called (status 0)) |} ]

let%expect_test _ =
  run_ligo_good [ "run" ; "evaluate-call" ; "-help" ] ;
  [%expect {|
    run a function with the given parameter.

      ligo run evaluate-call SOURCE_FILE PARAMETER_EXPRESSION

    This sub-command runs a LIGO function on a given argument. The context is initialized from a source file where the function is implemented. The interpretation is done using Michelson's interpreter.

    === flags ===

      [--amount INT]             the tezos amount the Michelson interpreter will use
                                 for the transaction.
      [--balance INT]            the balance the Michelson interpreter will use for
                                 the contract balance.
      [--display-format FORMAT]  the format that will be used by the CLI. Available
                                 formats are 'dev', 'json', and 'human-readable'
                                 (default). When human-readable lacks details (we
                                 are still tweaking it), please contact us and use
                                 another format in the meanwhile.
                                 (alias: --format)
      [--no-warn]                disable warning messages
      [--now TIMESTAMP]          the NOW value the Michelson interpreter will use
                                 (e.g. '2000-01-01T10:10:10Z')
      [--project-root PATH]      The path to root of the project.
      [--sender ADDRESS]         the sender the Michelson interpreter transaction
                                 will use.
      [--source ADDRESS]         the source the Michelson interpreter transaction
                                 will use.
      [--syntax SYNTAX]          the syntax that will be used. Currently supported
                                 syntaxes are "pascaligo", "cameligo", "reasonligo"
                                 and "jsligo". By default, the syntax is guessed
                                 from the extension (.ligo, .mligo, .religo, and
                                 .jsligo respectively).
                                 (alias: -s)
      [--warn-unused-rec]        warn about unused recursion in a recursive function
      [--werror]                 treat warnings as errors
      [-e ENTRY-POINT]           the entry-point that will be compiled.
                                 (alias: --entry-point)
      [-p PROTOCOL]              choose protocol's types/values pre-loaded into the
                                 LIGO environment (jakarta ,
                                 ithaca). By default, the current protocol (ithaca)
                                 will be used
                                 (alias: --protocol)
      [-help]                    print this help text and exit
                                 (alias: -?)

    (src/command.ml.Exit_called (status 0)) |} ]


let%expect_test _ =
  run_ligo_good [ "run" ; "evaluate-expr" ; "-help" ] ;
  [%expect {|
    evaluate a given definition.

      ligo run evaluate-expr SOURCE_FILE

    This sub-command evaluates a LIGO definition. The context is initialized from a source file where the definition is written. The interpretation is done using a Michelson interpreter.

    === flags ===

      [--amount INT]             the tezos amount the Michelson interpreter will use
                                 for the transaction.
      [--balance INT]            the balance the Michelson interpreter will use for
                                 the contract balance.
      [--display-format FORMAT]  the format that will be used by the CLI. Available
                                 formats are 'dev', 'json', and 'human-readable'
                                 (default). When human-readable lacks details (we
                                 are still tweaking it), please contact us and use
                                 another format in the meanwhile.
                                 (alias: --format)
      [--no-warn]                disable warning messages
      [--now TIMESTAMP]          the NOW value the Michelson interpreter will use
                                 (e.g. '2000-01-01T10:10:10Z')
      [--project-root PATH]      The path to root of the project.
      [--sender ADDRESS]         the sender the Michelson interpreter transaction
                                 will use.
      [--source ADDRESS]         the source the Michelson interpreter transaction
                                 will use.
      [--syntax SYNTAX]          the syntax that will be used. Currently supported
                                 syntaxes are "pascaligo", "cameligo", "reasonligo"
                                 and "jsligo". By default, the syntax is guessed
                                 from the extension (.ligo, .mligo, .religo, and
                                 .jsligo respectively).
                                 (alias: -s)
      [--warn-unused-rec]        warn about unused recursion in a recursive function
      [--werror]                 treat warnings as errors
      [-e ENTRY-POINT]           the entry-point that will be compiled.
                                 (alias: --entry-point)
      [-p PROTOCOL]              choose protocol's types/values pre-loaded into the
                                 LIGO environment (jakarta ,
                                 ithaca). By default, the current protocol (ithaca)
                                 will be used
                                 (alias: --protocol)
      [-help]                    print this help text and exit
                                 (alias: -?)

    (src/command.ml.Exit_called (status 0)) |} ]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "-help" ] ;
  [%expect {|
    compile to a Michelson value.

      ligo compile expression SYNTAX _EXPRESSION

    This sub-command compiles a LIGO expression to a Michelson value. It works by compiling the LIGO expression to a Michelson expression and then interpreting it using Michelson's interpreter.

    === flags ===

      [--constants CONSTANTS]            A list of global constants that will be
                                         assumed in the context, separated by ','
                                         (alias: -c)
      [--display-format FORMAT]          the format that will be used by the CLI.
                                         Available formats are 'dev', 'json', and
                                         'human-readable' (default). When
                                         human-readable lacks details (we are still
                                         tweaking it), please contact us and use
                                         another format in the meanwhile.
                                         (alias: --format)
      [--file-constants FILE_CONSTANTS]  A file with a JSON list of strings with
                                         Michelson code. Those Michelson values will
                                         be registered as global constants in the
                                         context.
      [--init-file FILENAME]             the path to the smart contract file to be
                                         used for context initialization.
      [--michelson-format CODE_FORMAT]   format that will be used by
                                         compile-contract for the resulting
                                         Michelson. Available formats are 'text'
                                         (default), 'json' and 'hex'.
      [--no-warn]                        disable warning messages
      [--project-root PATH]              The path to root of the project.
      [--warn-unused-rec]                warn about unused recursion in a recursive
                                         function
      [--werror]                         treat warnings as errors
      [--without-run]                    disable running of compiled expression.
      [-p PROTOCOL]                      choose protocol's types/values pre-loaded
                                         into the LIGO environment (jakarta ,
                                         ithaca). By default, the current protocol
                                         (ithaca) will be used
                                         (alias: --protocol)
      [-help]                            print this help text and exit
                                         (alias: -?)

    (src/command.ml.Exit_called (status 0)) |} ] ;
