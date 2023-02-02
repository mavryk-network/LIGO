open Simple_utils.Trace
open Unification_shared
module CST = Cst.Cameligo
module AST = Ast_unified

val compile_expression : CST.expr -> AST.expr
val compile_program : CST.ast -> AST.program

(* val decompile_expression : AST.expr -> CST.expr *)
(* val decompile_module     : AST.module_ -> CST.ast *)
