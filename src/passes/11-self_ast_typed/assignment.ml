open Helpers
open Ast_typed
open Trace

let replace_assign : _ mapper = fun expr ->
  match expr.expression_content with
  | E_let_in {let_binder=_;
              rhs={expression_content = E_assign {lvalue;value};_};
              let_result;
              inline=_} ->
     ok @@ { expr with expression_content = e_let_in lvalue value let_result false }
  (* | E_assign {lvalue; value} -> *)
  | _ ->
     ok @@ expr
