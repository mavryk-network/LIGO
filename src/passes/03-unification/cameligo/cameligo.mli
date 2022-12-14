open Simple_utils.Trace
open Unification_shared
module CST = Cst.Cameligo
module AST = Ast_unified

val compile_expression
  :  raise:(Errors.t, Main_warnings.all) raise
  -> CST.expr
  -> AST.expr

val compile_program
  :  raise:(Errors.t list, Main_warnings.all) raise
  -> CST.ast
  -> AST.program

(* val decompile_expression : AST.expr -> CST.expr *)
(* val decompile_module     : AST.module_ -> CST.ast *)
