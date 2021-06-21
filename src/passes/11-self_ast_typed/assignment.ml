open Helpers
open Ast_typed
open Trace

let replace_assign : _ mapper = fun expr ->
  match expr.expression_content with
  | E_assign {lvalue; value} ->
     ok @@ { expr with expression_content = e_let_in lvalue value e_a_unit true }
  | _ ->
     ok @@ expr
