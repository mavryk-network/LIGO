open Main_errors
open Simple_utils.Trace
open Ast_unified
open Nanopasses

let compile ~raise ~options (p : program) : Ast_core.program =
  trace ~raise nanopasses_tracer @@ compile_program ~options p


let compile_expression
    ~(raise : (Main_errors.all, Main_warnings.all) raise)
    ~options
    (e : expr)
    : Ast_core.expression
  =
  trace ~raise nanopasses_tracer @@ compile_expression ~options e
