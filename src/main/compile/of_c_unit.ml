module Location = Simple_utils.Location
open Helpers

type c_unit = Buffer.t

let compile ~raise ~meta c_unit (source_filename : string) : Ast_unified.program =
  parse_and_abstract ~raise ~meta c_unit source_filename


let compile_temp
    ~(raise : (Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise)
    ~options
    ~meta
    c_unit
    (source_filename : string)
    : Ast_core.program
  =
  let open Simple_utils.Trace in
  let open Main_errors in
  let unified =
    match meta.syntax with
    | PascaLIGO ->
      let raw =
        trace ~raise parser_tracer (Parsing.Pascaligo.parse_file c_unit source_filename)
      in
      Unification.Pascaligo.compile_program raw
    | CameLIGO ->
      let raw =
        trace ~raise parser_tracer (Parsing.Cameligo.parse_file c_unit source_filename)
      in
      Unification.Cameligo.compile_program raw
    | JsLIGO ->
      let raw =
        trace ~raise parser_tracer (Parsing.Jsligo.parse_file c_unit source_filename)
      in
      Unification.Jsligo.compile_program raw
  in
  trace ~raise nanopasses_tracer (Nanopasses.compile_program ~options unified)


let compile_expression ~raise = parse_and_abstract_expression ~raise

let compile_string ~raise : meta:meta -> c_unit -> Ast_unified.program =
 fun ~meta c_unit -> parse_and_abstract_string ~raise meta.syntax c_unit


let compile_contract_input ~raise : meta:meta -> c_unit -> c_unit -> Ast_unified.expr =
 fun ~meta storage parameter ->
  let storage, parameter =
    Simple_utils.Pair.map ~f:(compile_expression ~raise ~meta) (storage, parameter)
  in
  Ast_unified.e_tuple
    ~loc:Location.dummy
    (Simple_utils.List.Ne.of_list [ storage; parameter ])


let pretty_print_cst = pretty_print_cst
let pretty_print = pretty_print
