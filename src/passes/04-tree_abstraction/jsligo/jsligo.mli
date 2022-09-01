[@@@warning "-45"]


module CST = Cst.Jsligo
module AST = Ast_imperative
module Errors = Errors

val compile_expression : raise:(Errors.abs_error,Main_warnings.all) Simple_utils.Trace.raise -> CST.expr -> AST.expr
val compile_program    : raise:(Errors.abs_error list,Main_warnings.all) Simple_utils.Trace.raise -> CST.ast -> AST.program

val decompile_expression: AST.expr -> CST.expr list
val decompile_program: AST.program -> CST.ast

val decompile_pattern_to_string : AST.type_expression option Ligo_prim.Pattern.t -> string
