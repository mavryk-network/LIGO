(* lift functions outside *)
open Mini_c.Types

(* let collect_variables: ... -> ... = fun e ->
  ... *)

let lift: expression -> expression list * expression = fun e ->
  match e.content with 
    E_closure 
  | E_let_in ({content = E_closure},) -> 
    (* MOVE UPWARDS *)
  | E_let_in ({content = something_else}) ->
    (* collect variable as defined from here *)
  | E_variable _ ->
    (* is it available here? if not -> should be an argument *)
  [], e

let rec toplevel: expression -> expression = fun e ->
  match e.content with
    E_let_in ({content = E_closure _; _} as e1, inline, ((var_name, type_expression), e2)) -> 
      { e with content = E_let_in (snd @@ lift e1, inline, ((var_name, type_expression), toplevel e2)) }
  | content -> { e with content }

  (* 1. entry function *)
  (* 2. toplevel functions *)
  (* 3. too deep *)
  (* what variables does the function need, besides the arguments *)
