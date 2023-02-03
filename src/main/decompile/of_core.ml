open Ast_core
open Nanopasses

let decompile (m : program) : Ast_unified.program = decompile_program m

let decompile_expression (e : expression) : Ast_unified.expr =
  decompile_expression e
