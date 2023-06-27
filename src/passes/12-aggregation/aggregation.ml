module Errors = Errors

let compile_expression ~raise : Ast_typed.expression -> Ast_aggregated.expression =
 fun x ->
  ignore raise;
  Compiler.compile_expression Compiler.Data.empty [] x


let compile_program ~raise
    : Ast_typed.expression -> Ast_typed.program -> Ast_aggregated.program
  =
 fun hole program ->
  ignore raise;
  Compiler.compile Compiler.Data.empty [] hole program


let compile_type : Ast_typed.type_expression -> Ast_aggregated.type_expression =
 fun type_ -> Compiler.compile_type type_


let decompile : Ast_aggregated.expression -> Ast_typed.expression = Decompiler.decompile
