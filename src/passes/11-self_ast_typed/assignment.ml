open Helpers
open Ast_typed
open Trace

let replace_assign : _ mapper = fun expr ->
  match expr.expression_content with
  | E_assign {lvalue; value; next} ->
     ok @@ { expr with expression_content = e_let_in lvalue value next true }
  | _ ->
     ok @@ expr
