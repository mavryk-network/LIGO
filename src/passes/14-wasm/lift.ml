(* 
  Lift functions outside, as WebAssembly has no support for nested functions.
   
  Creation of types in this file is flawed, but doesn't seem to have an effect so far?
*)
open Mini_c.Types

type env = {
  variables      : (var_name * type_expression) list;
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

let rec lift: env -> expression -> env * expression = fun env e ->
  match e.content with 
  | E_literal _ -> env, e
  | E_closure {binder; body} -> 
    let env = {env with variables = (binder, e.type_expression) :: env.variables} in
    let env, body = lift env body in
    env, {e with content = E_closure {binder; body}}
  | E_constant { cons_name; arguments } ->
    let arguments = List.map ~f:(fun a -> snd @@ lift env a) arguments in
    env, {e with content = E_constant {cons_name; arguments}}
  | E_application ({content = E_variable v; _ } as e1, e2) ->
    let env, e1 = lift env e1 in 
    let env, e2 = lift env e2 in 
    let e_app = {e with content = E_application (e1, e2)} in
    print_endline ("Trying to find a replacement for:" ^ Var.debug (Location.unwrap v));
    let r = (match List.find env.replacements ~f:(fun r -> Location.equal_content ~equal:Var.equal (fst r) v) with 
        Some r -> print_endline "-success"; env, snd r e2
      | None -> print_endline "-failure"; env, e_app
    );
  in 
  r
  | E_application (e1, e2) ->
    let env, e1 = lift env e1 in 
    let env, e2 = lift env e2 in 
    env, {e with content = E_application (e1, e2)}
  | E_variable v -> 
    (match (List.find ~f:(fun var -> Location.equal_content ~equal:Var.equal v (fst var)) env.variables) with 
      Some _ -> env, e 
    | None -> 
      (print_endline ("Could not find:" ^ Var.debug (Location.unwrap v));
      {env with missing = v :: env.missing}, e))
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
  | E_let_in ({content = E_closure _} as c, inline, ((var_name, _type_expression), e2)) -> 
    print_endline ("Extract function:" ^ Var.debug (Location.unwrap var_name));
    let env2, c = lift {empty_env with replacements = env.replacements} c in
    let v = Location.wrap (Var.fresh_like (Location.unwrap var_name)) in
    let rec aux remaining = 
      match remaining with 
        item :: remaining ->
          let body, type_expression = aux remaining in
          let type_expression_item = (match (List.find ~f:(fun var -> Location.equal_content ~equal:Var.equal item (fst var)) env.variables) with
              Some (_, s) -> s
            | None -> type_expression
          ) in
          let type_expression = {type_content = T_function (type_expression_item, type_expression); location = e.type_expression.location} in
          {content = E_closure {binder = item; body};  type_expression; location = c.location}, type_expression
      | [] -> c, c.type_expression
    in
    let rec aux2 remaining r = 
      match remaining with 
        item :: remaining -> 
          let item = {content = E_variable item; type_expression = c.type_expression; location = c.location} in
          {content = E_application (item, aux2 remaining r);  type_expression = c.type_expression; location = c.location}
      | [] -> r
    in
    (* env2.missing should not be in env.replacements *)
    let in_replacement i = (match List.find env.replacements ~f:(fun r -> Location.equal_content ~equal:Var.equal (fst r) i) with 
        Some _ -> true
      | None -> false
    ) in
    let missing = List.filter ~f:(fun f -> not(in_replacement f)) env2.missing in
    print_endline ("Missing variables:");
    List.iter ~f:(fun f -> print_endline (" - " ^ Var.debug (Location.unwrap f))) missing;
    let replacement = (var_name, aux2 (v :: missing)) in
    let export, type_expression = aux missing in
    let export = fun e -> {content = E_let_in (export, inline, ((v, type_expression), e)); type_expression; location = c.location} in
    print_endline ("Replacement available for:" ^ Var.debug (Location.unwrap var_name));
    let env = {env with exported_funcs = export :: env.exported_funcs; replacements = replacement :: env2.replacements @ env.replacements; missing = env2.missing @ env.missing } in
    let env, e2 = lift env e2 in
    env, e2
  | E_let_in (e1, inline, ((var_name, type_expression), e2)) ->
    let env = {env with variables = (var_name, type_expression) :: env.variables} in
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    env, {e with content = E_let_in (e1, inline, ((var_name, type_expression), e2))}
  | E_tuple l -> 
    let l = List.map ~f:(fun e -> snd @@ lift env e) l in
    env, {e with content = E_tuple l}
  | E_let_tuple (e1, (lst, e2)) -> 
    let _, e1 = lift env e1 in
    let env, e2 = lift {env with variables = lst @ env.variables} e2 in
    (* let variables = List.map ~f:fst lst in *)
    env, {e with content = E_let_tuple (e1, (lst, e2))}
  | E_proj (e, a, b) ->
    let env, e = lift env e in
    env, {e with content = E_proj (e, a, b)}
  | E_update (e1, a, e2, b) ->
    let _, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    env, {e with content = E_update (e1, a, e2, b)}
  | E_raw_michelson _ -> env, e

let rec toplevel_inner: env -> expression -> expression = fun env e ->
  match e.content with
    E_let_in ({content = E_closure {binder; body}; _} as e1, inline, ((var_name, type_expression), e2)) -> 
      print_endline ("Handling function: " ^ Var.debug (Location.unwrap var_name));
      let env, body = lift {empty_env with replacements = env.replacements} body in
      List.fold_left ~f:(
        fun prev el ->
          el prev
      ) ~init:{e with content = E_let_in ({ e1 with content =  E_closure {binder; body}}, inline, ((var_name, type_expression), toplevel_inner env e2))}
      env.exported_funcs 
  | E_let_in (e1, inline, ((var_name, type_expression), e2)) -> 
      {e with content = E_let_in (e1, inline, ((var_name, type_expression), toplevel_inner env e2))}
  | _ -> e

let toplevel = toplevel_inner empty_env 