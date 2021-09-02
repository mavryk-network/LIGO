[@@@warning "-45"]

open Trace

module CST = Cst.Cameligo
module AST = Ast_imperative
module Errors = Errors
module Warnings = Warnings


val compile_expression : raise:Errors.abs_error raise -> add_warning:(Warnings.abs_warning -> unit)-> CST.expr -> AST.expr
val compile_module : raise:Errors.abs_error raise -> add_warning:(Warnings.abs_warning -> unit) -> CST.ast -> AST.module_

val decompile_expression : AST.expr -> CST.expr
val decompile_module     : AST.module_ -> CST.ast
