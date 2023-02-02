open Simple_utils.Trace
open Unification_shared
module CST = Cst.Jsligo
module AST = Ast_unified

val compile_expression : CST.expr -> AST.expr
val compile_program : CST.t -> AST.program

(* val decompile_expression : AST.expr -> CST.expr *)
(* val decompile_module     : AST.module_ -> CST.t *)
