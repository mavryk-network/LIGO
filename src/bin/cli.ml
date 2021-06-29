open Core_kernel
open Command
open Cli_helpers

let version = Version.version

let source_file =
  let docv = "SOURCE_FILE" in
  let _doc = "$(docv) is the path to the smart contract file." in
  Param.(anon (docv %: string))

(*  let info =
    info ~docv ~doc [] in
  required @@ pos n (some non_dir_file) None info
*)
let entry_point =
  let docv = "ENTRY_POINT" in
  let _doc = "$(docv) is entry-point that will be compiled." in
  let open Param in
  maybe_with_default "main" (docv %: string)

let expression purpose =
  let open Param in
  let docv = purpose ^ "_EXPRESSION" in
  let _doc = "$(docv) is the expression that will be compiled." in
  anon (docv %: string)

let libraries =
  let open Param in
  let docv = "LIBRARY" in
  let doc = docv ^ " is a path to a directory containing included files" in
  flag ~aliases:["lib"] "l" (listed string) ~doc

let syntax =
  let open Param in
  let docv = "SYNTAX" in
  let doc = docv ^ " is the syntax that will be used. Currently supported syntaxes are \"pascaligo\", \"cameligo\", \"reasonligo\" and \"jsligo\". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively)." in
  flag ~aliases:["syntax"] "s" (optional_with_default "auto" string) ~doc


let protocol_version =
  let open Param in
  let open Environment.Protocols in
  let plist = Format.asprintf "%a" (Simple_utils.PP_helpers.list_sep_d_par Format.pp_print_string) protocols_str in
  let docv = "PROTOCOL_VERSION" in
  let doc = Format.asprintf "%s will decide protocol's types/values pre-loaded into the LIGO environment %s. \
                            By default, the current protocol (%s) will be used" docv plist (variant_to_string current) in
  flag ~aliases:["protocol"] "p"  (optional_with_default "current" string) ~doc

let dialect =
  let open Param in
  let docv = "PASCALIGO_DIALECT" in
  let doc = docv ^ " is the pascaligo dialect that will be used. Currently supported dialects are \"terse\" and \"verbose\". By default the dialect is \"terse\"." in
  flag ~aliases:["dialect"] "d" (optional_with_default "terse" string) ~doc

let req_syntax =
  let open Param in
  let docv = "SYNTAX" in
  let _doc = docv ^ " is the syntax that will be used. Currently supported syntaxes are \"pascaligo\", \"cameligo\" and \"reasonligo\". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, .jsligo respectively)." in
  anon (docv %: string)

let init_file =
  let open Param in
  let docv = "INIT_FILE" in
  let doc = docv ^ " is the path to smart contract file to be used for context initialization." in
  flag "init-file" (optional string) ~doc

let amount =
  let open Param in
  let docv = "AMOUNT" in
  let doc = docv ^ " is the amount the Michelson interpreter will use for the transaction." in
  flag "amount" (optional_with_default "0" string) ~doc

let balance =
  let open Param in
  let docv = "BALANCE" in
  let doc = docv ^ " is the balance the Michelson interpreter will use for the contract balance." in
  flag "balance" (optional_with_default "0" string) ~doc

let sender =
  let open Param in
  let docv = "SENDER" in
  let doc = docv ^ " is the sender the Michelson interpreter transaction will use." in
  flag "sender" (optional string) ~doc

let source =
  let open Param in
  let docv = "SOURCE" in
  let doc = docv ^ " is the source the Michelson interpreter transaction will use." in
  flag "source" (optional string) ~doc

let disable_michelson_typechecking =
  let open Param in
  let doc = "disable Michelson typecking, this might produce ill-typed Michelson code." in
  flag "disable-michelson-typechecking" no_arg ~doc


let with_types =
  let open Param in
  let doc = "tries to infer types for all named expressions" in
  flag "with-types" no_arg ~doc

let now =
  let open Param in
  let docv = "NOW" in
  let doc = docv ^ " is the NOW value the Michelson interpreter will use (e.g. '2000-01-01T10:10:10Z')" in
  flag "now" (optional string) ~doc

let display_format =
  let open Param in
  let open Display in
  let enum = Arg_type.of_map @@
    String.Map.of_alist_exn [("human-readable", human_readable); ("dev", dev); ("json", json)] in
  let docv = "DISPLAY_FORMAT" in
  let doc = docv ^ " is the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile." in
  flag ~aliases:["format"] "display-format" (optional_with_default human_readable enum) ~doc

let output_file =
  let open Param in
  let docv = "OUTPUT_FILE" in
  let doc = docv ^ " if used, prints the output into the specified file instead of stdout" in
  flag ~aliases:["output"] "output-file" (optional string) ~doc

let michelson_code_format =
  let open Param in
  let enum = Arg_type.of_map @@
    String.Map.of_alist_exn  [("text", `Text); ("json", `Json); ("hex", `Hex)] in
  let docv = "MICHELSON_FORMAT" in
  let doc = docv ^ " is the format that will be used by compile-contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'." in
  flag "michelson-format" (optional_with_default `Text enum ) ~doc

let optimize =
  let open Param in
  let _docv = "ENTRY_POINT" in
  let doc = "Apply Mini-C optimizations as if compiling $(docv)" in
  flag "optimize" (optional string) ~doc

let infer =
  let open Param in
  let doc = "enable type inference" in
  flag "infer" no_arg ~doc

let warn =
  let open Param in
  let docv = "BOOL" in
  let doc = docv ^ " indicates whether warning messages should be printed in stderr or not" in
  flag "warn" (optional_with_default true bool) ~doc

let werror =
  let open Param in
  let docv = "BOOL" in
  let doc = docv ^ " indicates whether warning messages should be treated as errors or not" in
  flag "werror" (optional_with_default false bool) ~doc


let compile_file =
  let f source_file entry_point syntax infer protocol_version display_format disable_typecheck michelson_format output_file warn werror =
    return_result ~warn ?output_file @@ 
    Api.Compile.contract ~werror source_file entry_point syntax infer protocol_version display_format disable_typecheck michelson_format in
  basic 
    ~summary:"Subcommand: Compile a contract."
    ~readme:(fun () -> 
             "This sub-command compiles a contract to Michelson \
                 code. It expects a source file and an entrypoint \
                 function that has the type of a contract: \"parameter \
                 * storage -> operations list * storage\"."
      )
    Let_syntax.(
      let%map_open source_file = source_file in
      fun () -> f source_file entry_point syntax infer protocol_version display_format disable_typecheck michelson_format output_file warn werror
    )
let preprocess =
  let f source_file syntax display_format =
    return_result @@
      Api.Print.preprocess source_file syntax display_format in
  let term = Term.(const f $ source_file 0 $ syntax $ display_format) in
  let cmdname = "preprocess" in
  let doc = "Subcommand: Preprocess the source file.\nWarning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command runs the pre-processor on a LIGO \
                 source file and outputs the result. The directive \
                 `#include` directly inlines the included file and \
                 therefore its content appears in the output. In \
                 contrast, the directive `#import` includes the file \
                 as a module and therefore the content of the imported \
                 file is not printed by this sub-command."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let pretty_print =
  let f source_file syntax display_format =
    return_result @@ 
    Api.Print.pretty_print source_file syntax display_format in
  let term = Term.(const f $ source_file 0 $ syntax $ display_format) in
  let cmdname = "pretty-print" in
  let doc = "Subcommand: Pretty-print the source file." in
  let man = [`S Manpage.s_description;
             `P "This sub-command pretty-prints a source file in \
                 LIGO. The width of the pretty-printed text is \
                 adjusted to the number of columns in the terminal (or \
                 60 if it cannot be determined)."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_graph =
  let f source_file syntax display_format =
    return_result @@
    Api.Print.dependency_graph source_file syntax display_format
  in
  let term = Term.(const f $ source_file 0  $ syntax $ display_format) in
  let cmdname = "print-graph" in
  let doc = "Subcommand: Print the dependency graph.\nWarning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the dependency graph created \
                 by the module system. It explores all imported source \
                 files (recursively) following a DFS strategy."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_cst =
  let f source_file syntax display_format =
    return_result @@
    Api.Print.cst source_file syntax display_format
  in
  let term = Term.(const f $ source_file 0  $ syntax $ display_format) in
  let cmdname = "print-cst" in
  let doc = "Subcommand: Print the CST.\nWarning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the CST \
                 stage, obtained after preprocessing and parsing."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_ast =
  let f source_file syntax display_format =
    return_result@@
    Api.Print.ast source_file syntax display_format
  in
  let term = Term.(const f $ source_file 0 $ syntax $ display_format) in
  let cmdname = "print-ast" in
  let doc = "Subcommand: Print the AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 imperative stage, before sugaring step is applied."]
  in (Term.ret term, Term.info ~man ~doc cmdname)


let print_ast_sugar =
  let f source_file syntax display_format =
    return_result @@
    Api.Print.ast_sugar source_file syntax display_format
  in
  let term = Term.(const f $ source_file 0  $ syntax $ display_format) in
  let cmdname = "print-ast-sugar" in
  let doc = "Subcommand: Print the AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 stage, after sugaring step is applied."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_ast_core =
  let f source_file syntax infer protocol_version display_format =
    return_result @@
    Api.Print.ast_core source_file syntax infer protocol_version display_format
  in
  let term = Term.(const f $ source_file 0  $ syntax $ infer $ protocol_version $ display_format) in
  let cmdname = "print-ast-core" in
  let doc = "Subcommand: Print the AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 core stage."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_ast_typed =
  let f source_file syntax infer protocol_version display_format =
    return_result @@
    Api.Print.ast_typed source_file syntax infer protocol_version display_format
  in
  let term = Term.(const f $ source_file 0  $ syntax $ infer $ protocol_version $ display_format) in
  let cmdname = "print-ast-typed" in
  let doc = "Subcommand: Print the typed AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 typed stage. Internally, it uses the build system to \
                 type the contract, but the contract is not combined \
                 with imported modules."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_ast_combined =
  let f source_file syntax infer protocol_version display_format =
    return_result @@
    Api.Print.ast_combined source_file syntax infer protocol_version display_format
  in
  let term = Term.(const f $ source_file 0  $ syntax $ infer $ protocol_version $ display_format) in
  let cmdname = "print-ast-combined" in
  let doc = "Subcommand: Print the contract after combination with the build system.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 typed stage. Internally, it uses the build system to \
                 type the contract, and the contract is combined with \
                 the imported modules."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_mini_c =
  let f source_file syntax infer protocol_version display_format optimize =
    return_result @@
    Api.Print.mini_c source_file syntax infer protocol_version display_format optimize
  in
  let term = Term.(const f $ source_file 0 $ syntax $ infer $ protocol_version $ display_format $ optimize) in
  let cmdname = "print-mini-c" in
  let doc = "Subcommand: Print Mini-C. Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the Mini-C \
                 stage. Internally, it uses the build system to type \
                 and compile the contract. Compilation is applied \
                 after combination in the AST typed stage."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let measure_contract =
  let f source_file entry_point syntax infer protocol_version display_format warn werror =
    return_result ~warn @@
    Api.Info.measure_contract source_file entry_point syntax infer protocol_version display_format werror
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1  $ syntax $ infer $ protocol_version $ display_format $ warn $ werror) in
  let cmdname = "measure-contract" in
  let doc = "Subcommand: Measure a contract's compiled size in bytes." in
  let man = [`S Manpage.s_description;
             `P "This sub-command compiles a source file and measures \
                 the contract's compiled size in bytes."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let compile_parameter =
  let f source_file entry_point expression syntax infer protocol_version amount balance sender source now display_format michelson_format output_file warn werror =
    return_result ~warn ?output_file @@
    Api.Compile.parameter source_file entry_point expression syntax infer protocol_version amount balance sender source now display_format michelson_format werror
    in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2  $ syntax $ infer $ protocol_version $ amount $ balance $ sender $ source $ now $ display_format $ michelson_code_format $ output_file $ warn $ werror) in
  let cmdname = "compile-parameter" in
  let doc = "Subcommand: Compile parameters to a Michelson expression." in
  let man = [`S Manpage.s_description;
             `P "This sub-command compiles a parameter for a given \
                 contract to a Michelson expression. The resulting \
                 Michelson expression can be passed as an argument in \
                 a transaction which calls a contract."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let interpret =
  let f expression init_file syntax infer protocol_version amount balance sender source now display_format =
    return_result @@
    Api.Run.interpret expression init_file syntax infer protocol_version amount balance sender source now display_format
  in
  let term =
    Term.(const f $ expression "EXPRESSION" 0 $ init_file $ syntax $ infer $ protocol_version $ amount $ balance $ sender $ source $ now $ display_format) in
  let cmdname = "interpret" in
  let doc = "Subcommand: Interpret the expression in the context initialized by the provided source file." in
  let man = [`S Manpage.s_description;
             `P "This sub-command interprets a LIGO expression. The \
                 context can be initialized by providing a source \
                 file. The interpretation is done using Michelson's \
                 interpreter."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let compile_storage =
  let f source_file entry_point expression syntax infer protocol_version amount balance sender source now display_format michelson_format output_file warn werror =
    return_result ~warn ?output_file @@
    Api.Compile.storage source_file entry_point expression syntax infer protocol_version amount balance sender source now display_format michelson_format werror
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "STORAGE" 2  $ syntax $ infer $ protocol_version $ amount $ balance $ sender $ source $ now $ display_format $ michelson_code_format $ output_file $ warn $ werror) in
  let cmdname = "compile-storage" in
  let doc = "Subcommand: Compile an initial storage in LIGO syntax to \
             a Michelson expression." in
  let man = [`S Manpage.s_description;
             `P "This sub-command compiles an initial storage for a \
                 given contract to a Michelson expression. The \
                 resulting Michelson expression can be passed as an \
                 argument in a transaction which originates a contract."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let dry_run =
  let f source_file entry_point input storage amount balance sender source now syntax infer protocol_version display_format warn werror =
    return_result ~warn @@
    Api.Run.dry_run source_file entry_point input storage amount balance sender source now syntax infer protocol_version display_format werror
    in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ expression "STORAGE" 3 $ amount $ balance $ sender $ source $ now  $ syntax $ infer $ protocol_version $ display_format $ warn $ werror) in
  let cmdname = "dry-run" in
  let doc = "Subcommand: Run a smart-contract with the given storage and input." in
  let man = [`S Manpage.s_description;
             `P "This sub-command runs a LIGO contract on a given \
                 storage and parameter. The context is initialized \
                 from a source file where the contract is \
                 implemented. The interpretation is done using \
                 Michelson's interpreter."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let evaluate_call ~cmdname_deprecation =
  let f source_file entry_point parameter amount balance sender source now syntax infer protocol_version display_format warn werror =
    return_result ~warn @@
    Api.Run.evaluate_call source_file entry_point parameter amount balance sender source now syntax infer protocol_version display_format werror
    in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ amount $ balance $ sender $ source $ now  $ syntax $ infer $ protocol_version $ display_format $ warn $ werror) in
  (* "run-function" was renamed to "evaluate-call", keeping both for a few versions for backward-compatibility. *)
  let cmdname = match cmdname_deprecation with
  | `deprecated_run_function -> "run-function"
  | `evaluate_call -> "evaluate-call" in
  let deprecation = match cmdname_deprecation with
  | `deprecated_run_function -> "Deprecated, renamed to evaluate-call. Use evaluate-call instead. "
  | `evaluate_call -> "" in
  let doc = deprecation ^ "Subcommand: Run a function with the given parameter." in
  let man = [`S Manpage.s_description;
             `P (deprecation ^
                 "This sub-command runs a LIGO function on a given \
                  argument. The context is initialized from a source \
                  file where the function is implemented. The \
                  interpretation is done using Michelson's interpreter.")]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let evaluate_expr ~cmdname_deprecation =
  let f source_file entry_point amount balance sender source now syntax infer protocol_version display_format warn werror =
    return_result ~warn @@
    Api.Run.evaluate_expr source_file entry_point amount balance sender source now syntax infer protocol_version display_format werror
    in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ amount $ balance $ sender $ source $ now  $ syntax $ infer $ protocol_version $ display_format $ warn $ werror) in
  (* "run-function" was renamed to "evaluate-call", keeping both for a few versions for backward-compatibility. *)
  let cmdname = match cmdname_deprecation with
  | `deprecated_evaluate_value -> "evaluate-value"
  | `evaluate_expr -> "evaluate-expr" in
  let deprecation = match cmdname_deprecation with
  | `deprecated_evaluate_value -> "Deprecated, renamed to evaluate-expr. Use evaluate-expr instead. "
  | `evaluate_expr -> "" in
  let doc = deprecation ^ "Subcommand: Evaluate a given definition." in
  let man = [`S Manpage.s_description;
             `P (deprecation ^
                 "This sub-command evaluates a LIGO definition. The \
                  context is initialized from a source file where the \
                  definition is written. The interpretation is done \
                  using a Michelson interpreter.")]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let compile_expression =
  let f expression syntax infer protocol_version init_file display_format michelson_format warn werror =
    return_result ~warn @@
    Api.Compile.expression expression syntax infer protocol_version init_file display_format michelson_format werror
    in
  let term =
    Term.(const f $ expression "" 1 $ req_syntax 0 $ infer $ protocol_version $ init_file $ display_format $ michelson_code_format $ warn $ werror) in
  let cmdname = "compile-expression" in
  let doc = "Subcommand: Compile to a Michelson value." in
  let man = [`S Manpage.s_description;
             `P "This sub-command compiles a LIGO expression to a \
                 Michelson value. It works by compiling the LIGO \
                 expression to a Michelson expression and then \
                 interpreting it using Michelson's interpreter."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let dump_changelog =
  let f display_format =
    return_result @@ Api.dump_changelog display_format in
  let term =
    Term.(const f $ display_format) in
  let cmdname = "changelog" in
  let doc = "Dump the LIGO changelog to stdout." in
  let man = [`S Manpage.s_description;
             `P "This sub-command dumps the changelog to the stdout."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let list_declarations =
  let f source_file syntax display_format =
    return_result @@
    Api.Info.list_declarations source_file syntax display_format
  in
  let term =
    Term.(const f $ source_file 0  $ syntax $ display_format) in
  let cmdname = "list-declarations" in
  let doc = "Subcommand: List all the top-level declarations." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints a list of all top-level \
                 declarations (not including types and modules)."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let transpile_contract =
  let f source_file new_syntax syntax new_dialect display_format output_file =
    return_result ?output_file @@
    Api.Transpile.contract source_file new_syntax syntax new_dialect display_format
  in
  let term =
    Term.(const f $ source_file 0 $ req_syntax 1  $ syntax $ dialect $ display_format $ output_file) in
  let cmdname = "transpile-contract" in
  let doc = "Subcommand: Transpile a contract to another syntax (BETA)." in
  let man = [`S Manpage.s_description;
             `P "This sub-command transpiles a source file to another \
                 syntax. It does not use the build system, but the \
                 source file is preprocessed. Comments are currently \
                 not transpiled. Please use at your own risk."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let transpile_expression =
  let f expression new_syntax syntax new_dialect display_format =
    return_result @@
    Api.Transpile.expression expression new_syntax syntax new_dialect display_format
  in
  let term =
    Term.(const f $ expression "" 1  $ req_syntax 2 $ req_syntax 0 $ dialect $ display_format) in
  let cmdname = "transpile-expression" in
  let doc = "Subcommand: Transpile an expression to another syntax (BETA)." in
  let man = [`S Manpage.s_description;
             `P "This sub-command transpiles a LIGO expression to \
                 another syntax. Comments are currently not \
                 transpiled. Please use at your own risk."]
  in (Term.ret term, Term.info ~man ~doc cmdname)


let get_scope =
  let f source_file syntax infer protocol_version libs display_format with_types =
    return_result @@
    Api.Info.get_scope source_file syntax infer protocol_version libs display_format with_types
  in
  let term =
    Term.(const f $ source_file 0 $ syntax $ infer $ protocol_version $ libraries $ display_format $ with_types) in
  let cmdname = "get-scope" in
  let doc = "Subcommand: Return the JSON encoded environment for a given file." in
  let man = [`S Manpage.s_description;
             `P "This sub-command returns the environment for a given \
                 file in JSON format. It does not use the build system."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let test =
  let f source_file syntax infer protocol_version display_format =
    return_result @@
    Api.Run.test source_file syntax infer protocol_version display_format
  in
  let term =
    Term.(const f $ source_file 0 $ syntax $ infer $ protocol_version $ display_format) in
  let cmdname = "test" in
  let doc = "Subcommand: Test a contract with the LIGO test framework (BETA)." in
  let man = [`S Manpage.s_description;
             `P "This sub-command tests a LIGO contract using a LIGO \
                 interpreter, no Michelson code is evaluated. Still \
                 under development, there are features that are work \
                 in progress and are subject to change. No real test \
                 procedure should rely on this sub-command alone.";
             (* 
             TODO: correct text below
             
             `S "EXTRA PRIMITIVES FOR TESTING";
             `P "Test.originate c st : binds contract c with the \
                 address addr which is returned, st as the initial \
                 storage.";
             `P "Test.set_now t : sets the current time to t.";
             `P "Test.set_balance addr b : sets the balance of \
                 contract bound to address addr (returns unit).";
             `P "Test.external_call addr p amt : performs a call to \
                 contract bound to addr with parameter p and amount \
                 amt (returns unit).";
             `P "Test.get_storage addr : returns current storage bound \
                 to address addr.";
             `P "Test.get_balance : returns current balance bound to \
                 address addr.";
             `P "Test.assert_failure (f : unit -> _) : returns true if \
                 f () fails.";
             `P "Test.log x : prints x into the console." *)
            ]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let repl =
  let f syntax_name protocol_version infer
    amount balance sender source now display_format init_file : unit Term.ret =
    (let protocol = Environment.Protocols.protocols_to_variant protocol_version in
    let syntax = Ligo_compile.Helpers.syntax_to_variant (Syntax_name syntax_name) None in
    let dry_run_opts = Ligo_run.Of_michelson.make_dry_run_options {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
    match protocol, Trace.to_option syntax, Trace.to_option dry_run_opts with
    | _, None, _ -> `Error (false, "Please check syntax name.")
    | None, _, _ -> `Error (false, "Please check protocol name.")
    | _, _, None -> `Error (false, "Please check run options.")
    | Some protocol, Some syntax, Some dry_run_opts ->
       `Ok (Repl.main syntax display_format protocol infer dry_run_opts init_file)) in
  let term =
    Term.(const f $ req_syntax 0 $ protocol_version $ infer $ amount $ balance $ sender $ source $ now $ display_format $ init_file) in
  let cmdname = "repl" in
  let doc = "Subcommand: REPL" in
  (Term.ret term , Term.info ~doc cmdname)


let buffer = Buffer.create 100


let run () = 
  run ~version main

let run ?argv () =
  let err = Format.formatter_of_buffer buffer in
  Term.eval_choice ~err ?argv main [
    test ;
    compile_file ;
    measure_contract ;
    compile_parameter ;
    compile_storage ;
    compile_expression ;
    transpile_contract ;
    transpile_expression ;
    interpret ;
    dry_run ;
    evaluate_call ~cmdname_deprecation:`deprecated_run_function ;
    evaluate_call ~cmdname_deprecation:`evaluate_call ;
    evaluate_expr ~cmdname_deprecation:`deprecated_evaluate_value ;
    evaluate_expr ~cmdname_deprecation:`evaluate_expr ;
    dump_changelog ;
    print_graph ;
    print_cst ;
    print_ast ;
    print_ast_sugar ;
    print_ast_core ;
    print_ast_typed ;
    print_ast_combined ;
    print_mini_c ;
    list_declarations ;
    preprocess;
    pretty_print;
    get_scope;
    repl;
  ]
