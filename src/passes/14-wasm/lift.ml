(* lift functions outside *)
[@@@warning "-27-26"]
open Mini_c.Types

type env = {
  variables      : var_name list;
  missing        : var_name list;
  exported_funcs : (expression -> expression) list;
  replacements   : (var_name * (expression -> expression)) list;
}

let empty_env = {
  variables      = [];
  missing        = [];
  exported_funcs = [];
  replacements   = [];
}

(* 
type available_variables = var_name list
type missing_variables   = var_name list

type lifted_functions    = (missing_variables * expression) list *)

let rec lift: env -> expression -> env * expression = fun env e ->
  match e.content with 
  | E_literal _ -> env, e
  | E_closure {binder; body} -> 
    let env = {env with variables = binder :: env.variables} in
    let env, body = lift env body in
    env, {e with content = E_closure {binder; body}}
  | E_constant { cons_name; arguments } ->
    let arguments = List.map ~f:(fun a -> snd @@ lift env a) arguments in
    env, {e with content = E_constant {cons_name; arguments}}
  | E_application (e1, e2) ->
    (match e1.content with 
      E_variable v -> 
        print_endline (Var.debug (Location.unwrap v));
        List.iter ~f:(fun (e, _) -> if (Location.equal_content ~equal:Var.equal e v) then print_endline "yay" else print_endline "nay") env.replacements
(*         
        print_endline "evar yes" *)
    | _ -> ()
    );
    let env, e1 = lift env e1 in 
    let env, e2 = lift env e2 in 
    env, {e with content = E_application (e1, e2)}
  | E_variable v -> 
    if List.mem env.variables v ~equal:(Location.equal_content ~equal:Var.equal) then 
      env, e
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
    let env, e2 = lift env e2 in
    let env2, c = lift empty_env c in
    let v = Location.wrap (Var.fresh_like (Location.unwrap var_name)) in
    (* let replacement = {e with content = E_let_in ({content = E_variable v; type_expression = c.type_expression; location = c.location}, inline, ((var_name, type_expression), e2))} in *)
    let rec aux remaining = 
      match remaining with 
        item :: remaining -> 
          {content = E_closure {binder = item; body = aux remaining};  type_expression = c.type_expression; location = c.location}
      | [] -> c
    in
    let rec aux2 remaining = 
      match remaining with 
        item :: remaining -> 
          fun e -> 
            let item = {content = E_variable item; type_expression = c.type_expression; location = c.location} in
            {content = E_application (item, aux2 remaining e);  type_expression = c.type_expression; location = c.location}
      | [] -> fun e -> e
    in
    let replacement = (var_name, aux2 env2.missing) in
    let export = aux env2.missing in
    let export = fun e -> {content = E_let_in (export, inline, ((v, type_expression), e)); type_expression = c.type_expression; location = c.location} in
    let env = {env with exported_funcs = export :: env.exported_funcs; variables = v :: env.variables; replacements = replacement :: env.replacements } in
    env, e2
  | E_let_in (e1, inline, ((var_name, type_expression), e2)) ->
    let env = {env with variables = var_name :: env.variables} in
    let env, e2 = lift env e2 in
    let env, e1 = lift env e1 in
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
      let env, body = lift empty_env body in
      print_endline ("exported functions:" ^ string_of_int (List.length env.exported_funcs));
      (* func1 -> func2 -> this_func *)
      List.fold_right ~f:(
        fun prev el ->
          prev el
      ) ~init:{e with content = E_let_in ({ e1 with content =  E_closure {binder; body}}, inline, ((var_name, type_expression), toplevel e2))}
      env.exported_funcs 
      (* in  *)

  | content -> { e with content }

  (* 1. entry function *)
  (* 2. toplevel functions *)
  (* 3. too deep *)
  (* what variables does the function need, besides the arguments *)
