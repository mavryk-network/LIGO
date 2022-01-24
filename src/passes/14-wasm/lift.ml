(* lift functions outside *)
[@@@warning "-27-26"]
open Mini_c.Types

type env = {
  variables      : var_name list;
  missing        : var_name list;
  exported_funcs : expression list;
}

let env = {
  variables      = [];
  missing        = [];
  exported_funcs = [];
}

(* 
type available_variables = var_name list
type missing_variables   = var_name list

type lifted_functions    = (missing_variables * expression) list *)

let rec lift: env -> expression -> env * expression = fun env e ->
  match e.content with 
  | E_literal _ -> env, e
  | E_closure {binder; body} -> 
    let env, body = lift env body in
    env, {e with content = E_closure {binder; body}}
  | E_constant { cons_name; arguments } ->
    let arguments = List.map ~f:(fun a -> snd @@ lift env a) arguments in
    env, {e with content = E_constant {cons_name; arguments}}
  | E_application (e1, e2) ->
    let env, e1 = lift env e1 in 
    let env, e2 = lift env e2 in 
    env, {e with content = E_application (e1, e2)}
  | E_variable v -> 
    if List.mem env.variables v ~equal:Caml.(=) then (
      print_endline "found it";
      env, e)
    else
      {env with missing = v :: env.missing}, e

  | E_iterator (cc, ((var_name, type_expression), e1), e2) ->
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    env, {e with content = E_iterator (cc, ((var_name, type_expression), e1), e2)}
  | E_fold (((var_name, type_expression), e1), e2, e3) ->
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    let env, e3 = lift env e3 in
    env, {e with content = E_fold (((var_name, type_expression), e1), e2, e3)}
  | E_fold_right (((var_name, type_expression1), e1), (e2, type_expression2), e3) ->
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    let env, e3 = lift env e3 in
    env, {e with content = E_fold_right (((var_name, type_expression1), e1), (e2, type_expression2), e3)}
  | E_if_bool (e1, e2, e3) ->
    let _, e1 = lift env e1 in
    let _, e2 = lift env e2 in
    let _, e3 = lift env e3 in
    env, {e with content = E_if_bool (e1, e2, e3) }
  | E_if_none (e1, e2, ((var_name, type_expression), e3)) -> 
    let _, e1 = lift env e1 in
    let _, e2 = lift env e2 in
    let _, e3 = lift env e3 in
    env, {e with content = E_if_none (e1, e2, ((var_name, type_expression), e3)) }
  | E_if_cons (e1, e2, (((var_name1, type_expression1), (var_name2, type_expression2)), e3)) ->
    let _, e1 = lift env e1 in
    let _, e2 = lift env e2 in
    let _, e3 = lift env e3 in
    env, {e with content = E_if_cons (e1, e2, (((var_name1, type_expression1), (var_name2, type_expression2)), e3)) }
  | E_if_left (e1, ((var_name1, type_expression1), e2), ((var_name2, type_expression2), e3)) ->
    let _, e1 = lift env e1 in
    let _, e2 = lift env e2 in
    let _, e3 = lift env e3 in
    env, {e with content = E_if_left (e1, ((var_name1, type_expression1), e2), ((var_name2, type_expression2), e3))}
  | E_let_in ({content = E_closure _} as c, inline, ((var_name, type_expression), e2)) -> 
    
    print_endline ("Move upwards:" ^ (Var.debug (Location.unwrap var_name)));

    let _, e2 = lift env e2 in
    let env = {env with exported_funcs = e2 :: env.exported_funcs} in
    env, {}

    (* TODO: add needed variables as arguments to function *)
    (* TODO: return call to function with env. *)
    (* let lifted_functions, e2 = lift available_variables missing_variables e2 in
    let lifted_functions, c = lift [] [] c in
    
    

    [], {e with content = E_let_in (c, inline, ((var_name, type_expression), e2))} *)
    failwith "todo x"
    (* MOVE UPWARDS *)
  | E_let_in (e1, inline, ((var_name, type_expression), e2)) ->
    let env, e2 = lift env e2 in
    let env, e1 = lift env e1 in
    let env = {env with variables = var_name :: env.variables} in
    
    env, {e with content = E_let_in (e1, inline, ((var_name, type_expression), e2))}
  | E_tuple l -> 
    let l = List.map ~f:(fun e -> snd @@ lift env e) l in
    env, {e with content = E_tuple l}
  | E_let_tuple (e1, (lst, e2)) -> 
    let _, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    env, {e with content = E_let_tuple (e1, (lst, e2))}
  | E_proj (e, a, b) ->
    let env, e = lift env e in
    env, {e with content = E_proj (e, a, b)}
  | E_update (e1, a, e2, b) ->
    let _, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    env, {e with content = E_update (e1, a, e2, b)}
  | E_raw_michelson _ -> env, e

let rec toplevel: expression -> expression = fun e ->
  match e.content with
    E_let_in ({content = E_closure {binder; body}; _} as e1, inline, ((var_name, type_expression), e2)) -> 
      print_endline ("toplevel:" ^ (Var.debug (Location.unwrap var_name)));
      let _, body = lift env body in
      {e with content = E_let_in ({ e1 with content =  E_closure {binder; body}}, inline, ((var_name, type_expression), toplevel e2))}
  | content -> { e with content }

  (* 1. entry function *)
  (* 2. toplevel functions *)
  (* 3. too deep *)
  (* what variables does the function need, besides the arguments *)
