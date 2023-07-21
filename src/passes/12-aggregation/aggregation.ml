module Errors = Errors
module Data = Compiler.Data

let compile_expression ~raise : Ast_typed.expression -> Ast_aggregated.expression =
 fun x ->
  ignore raise;
  Compiler.compile_expression Compiler.Data.empty_env x


let compile_program ~raise
    : Ast_typed.expression -> Ast_typed.program -> Ast_aggregated.program
  =
 fun hole program ->
  ignore raise;
  Compiler.compile Compiler.Data.empty hole program


let compile_context ~raise
   : Ast_typed.program -> Data.t
   =
   fun program ->
   ignore raise;
   Compiler.compile_declarations Compiler.Data.empty [] program


let compile_expressions ~raise : Data.t -> Ast_typed.expression list -> Ast_aggregated.expression list =
 fun data exprs ->
 ignore raise;
  let f expr = Compiler.compile_expression data.env expr in
  List.map ~f exprs


let decompile : Ast_aggregated.expression -> Ast_typed.expression = Decompiler.decompile
