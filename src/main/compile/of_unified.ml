open Main_errors
open Simple_utils.Trace
open Ast_unified
open Nanopasses

let compile ~raise ~(meta : Helpers.meta) (p : program) : Ast_core.program =
  trace ~raise nanopasses_tracer @@ compile_program ~syntax:meta.syntax p


let compile_expression
    ~(raise : (Main_errors.all, Main_warnings.all) raise)
    ~(meta : Helpers.meta)
    (e : expr)
    : Ast_core.expression
  =
  trace ~raise nanopasses_tracer @@ compile_expression ~syntax:meta.syntax e
