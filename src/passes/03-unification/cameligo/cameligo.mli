module CST = Cst.Cameligo
module AST = Ast_unified

val compile_expression : CST.expr -> AST.expr
val compile_program : CST.ast -> AST.program

