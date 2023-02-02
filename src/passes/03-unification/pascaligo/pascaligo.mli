module CST = Cst.Pascaligo
module AST = Ast_unified

val compile_expression : CST.expr -> AST.expr
val compile_program : CST.t -> AST.program
