module CST    = Cst.Jsligo

let peephole_type ~raise ~comments : CST.type_expr -> CST.type_expr = fun t -> 
  ignore raise; ignore comments;
  t

let peephole_expression: CST.expr -> CST.expr = fun e ->
  e

let peephole_statement ~raise ~comments : CST.statement -> CST.statement = fun s ->
  ignore raise; ignore comments;
  s 

let peephole ~raise ~comments : ('err) Helpers.mapper = {
  t = peephole_type ~raise ~comments;
  e = peephole_expression;
  d = peephole_statement ~raise ~comments;
}