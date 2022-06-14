module Errors = Errors

let compile_expression ~raise : Ast_typed.expression -> Ast_aggregated.expression =
  fun x -> Compiler.compile_expression ~raise Compiler.Data.empty [] x

(* compile_expression_in_context [filler] [context] : let .. = .. in let .. = .. in [filler'] *)
let compile_expression_in_context : Ast_typed.expression -> Ast_typed.expression Ast_aggregated.program -> Ast_aggregated.expression =
  fun i_exp prg -> prg i_exp

let compile_program ~raise : Ast_typed.program -> Ast_typed.expression Ast_aggregated.program =
  fun prg ->
    (fun hole -> Compiler.compile ~raise Compiler.Data.empty [] hole prg)

let decompile = Decompiler.decompile
