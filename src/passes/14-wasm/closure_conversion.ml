(*
   Lift functions outside, as WebAssembly has no support for nested functions.
*)
open Mini_c.Types
module ValueVar = Ligo_prim.ValueVar

type env = {
  variables: (var_name * type_expression) list;
  missing: var_name list;
  exported_funcs: (expression -> expression) list;
  replacements: (var_name * expression) list;
  functions: var_name list;
}

let empty_env =
  {
    variables = [];
    missing = [];
    exported_funcs = [];
    replacements = [];
    functions = [];
  }

let variable_exists env v =
  match List.find ~f:(fun var -> ValueVar.equal v (fst var)) env.variables with
  | Some _ -> true
  | None -> false

let var_to_string name =
  let name, hash = ValueVar.internal_get_name_and_counter name in
  name ^ "#" ^ string_of_int hash

let var_type env v =
  match List.find ~f:(fun var -> ValueVar.equal v (fst var)) env.variables with
  | Some (_, type_) -> type_
  | None -> failwith ("should not happen:" ^ var_to_string v)

let rec lift : env -> expression -> env * expression =
 fun env e ->
  let variable_exists = variable_exists env in
  match e.content with
  | E_literal _ -> (env, e)
  | E_closure {binder; body} ->
    let env2 = 
      {empty_env with 
          variables    = (binder, e.type_expression) :: []; 
          replacements = env.replacements; 
          functions    = env.functions;
      }
    in
    let env2, body = lift env2 body in
    let v = ValueVar.fresh () in
    let env = { 
      env2 with 
        variables = (binder, e.type_expression) :: (v, e.type_expression) :: env.variables
    } 
    in
    let export_func remaining =
      List.fold_left
        ~f:(fun all binder ->
          let type_expression_item = body.type_expression  in
          let type_expression =
            {
              type_content =
                T_function (type_expression_item, all.type_expression);
              location = e.type_expression.location;
              source_type = None;
            }
          in
          {
            content = E_closure {binder; body = all};
            type_expression;
            location = e.location;
          })
        ~init:{e with content = E_closure {binder; body} } remaining
    in
    let in_function i =
      match List.find env.functions ~f:(fun r -> ValueVar.equal r i) with
      | Some _ -> true
      | None -> false
    in
    let missing = env.missing in    
    let missing = List.filter ~f:(fun f -> not (in_function f)) missing in
    let export = export_func missing in
    let type_expression = export.type_expression in
    let export e =
      {
        content = E_let_in (export, false, ((v, type_expression), e));
        type_expression;
        location = e.location;
      }
    in
    let env =
      {
        variables = env.variables;
        exported_funcs = export :: env.exported_funcs;
        functions = env.functions;
        replacements = env.replacements;
        missing = env.missing;
      }
    in
    let e = { e with content = E_variable v} in
    (env, e)

  | E_constant {cons_name; arguments} ->
    let env, arguments =
      List.fold_left
        ~f:(fun (env, args) a ->
          let env, arg = lift env a in
          (env, arg :: args))
        ~init:(env, []) (List.rev arguments)
    in
    (env, {e with content = E_constant {cons_name; arguments}})
  | E_application (({content = E_variable v; _} as e1), e2) ->
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    let env, e1 =
      match
        List.find env.replacements ~f:(fun (r, _) -> ValueVar.equal r v)
      with
      | Some (_, x) -> lift env x
      | None -> (env, e1)
    in
    ( env,
      {
        e with
        content = E_application (e1, e2);
        type_expression =
          {
            e.type_expression with
            type_content = T_function (e1.type_expression, e2.type_expression);
          };
      } )
  | E_application (e1, e2) ->
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    ( env,
      {
        
        e with
        content = E_application (e1, e2);
        type_expression =
          {
            e.type_expression with
            type_content = T_function (e1.type_expression, e2.type_expression);
          };
      } )
  | E_variable v when variable_exists v -> 
    (env, e)
  | E_variable v -> ({env with missing = v :: env.missing}, e)
  | E_iterator (cc, ((var_name, type_expression), e1), e2) ->
    let variables = (var_name, type_expression) :: env.variables in
    let env = {env with variables} in
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    ( env,
      {e with content = E_iterator (cc, ((var_name, type_expression), e1), e2)}
    )
  | E_fold (((var_name, type_expression), e1), e2, e3) ->
    let env =
      {env with variables = (var_name, type_expression) :: env.variables}
    in
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    let env, e3 = lift env e3 in
    (env, {e with content = E_fold (((var_name, type_expression), e1), e2, e3)})
  | E_fold_right (((var_name, type_expression1), e1), (e2, type_expression2), e3)
    ->
    let env =
      {env with variables = (var_name, type_expression1) :: env.variables}
    in
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    let env, e3 = lift env e3 in
    ( env,
      {
        e with
        content =
          E_fold_right
            (((var_name, type_expression1), e1), (e2, type_expression2), e3);
      } )
  | E_if_bool (e1, e2, e3) ->
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    let env, e3 = lift env e3 in
    (env, {e with content = E_if_bool (e1, e2, e3)})
  | E_if_none (e1, e2, ((var_name, type_expression), e3)) ->
    let variables = (var_name, type_expression) :: env.variables in
    let env = {env with variables} in
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    let env, e3 = lift env e3 in
    ( env,
      {e with content = E_if_none (e1, e2, ((var_name, type_expression), e3))}
    )
  | E_if_cons
      ( e1,
        e2,
        (((var_name1, type_expression1), (var_name2, type_expression2)), e3) )
    ->
    let variables =
      (var_name1, type_expression1)
      :: (var_name2, type_expression2)
      :: env.variables
    in
    let env = {env with variables} in
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    let env, e3 = lift env e3 in
    ( env,
      {
        e with
        content =
          E_if_cons
            ( e1,
              e2,
              ( ((var_name1, type_expression1), (var_name2, type_expression2)),
                e3 ) );
      } )
  | E_if_left
      ( e1,
        ((var_name1, type_expression1), e2),
        ((var_name2, type_expression2), e3) ) ->
    let variables =
      (var_name1, type_expression1)
      :: (var_name2, type_expression2)
      :: env.variables
    in
    let env = {env with variables} in
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    let env, e3 = lift env e3 in
    ( env,
      {
        e with
        content =
          E_if_left
            ( e1,
              ((var_name1, type_expression1), e2),
              ((var_name2, type_expression2), e3) );
      } )
  | E_let_in (e1, inline, ((var_name, type_expression), e2)) ->
    let env =
      {env with variables = (var_name, type_expression) :: env.variables}
    in
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    ( env,
      {
        e with
        content = E_let_in (e1, inline, ((var_name, type_expression), e2));
      } )
  | E_tuple l ->
    let env, l =
      List.fold_left
        ~f:(fun (env, args) a ->
          let env, arg = lift env a in
          (env, arg :: args))
        ~init:(env, []) (List.rev l)
    in
    (env, {e with content = E_tuple (List.rev l)})
  | E_let_tuple (e1, (lst, e2)) ->
    let env, e1 = lift env e1 in
    let env, e2 = lift {env with variables = lst @ env.variables} e2 in
    (env, {e with content = E_let_tuple (e1, (lst, e2))})
  | E_proj (e, a, b) ->
    let env, e = lift env e in
    (env, {e with content = E_proj (e, a, b)})
  | E_update (e1, a, e2, b) ->
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    (env, {e with content = E_update (e1, a, e2, b)})
  | E_create_contract _ -> (env, e)
  | E_raw_michelson _ -> (env, e)
  | E_raw_wasm _ -> (env, e)
  | E_global_constant (s, lst) ->
    let env, lst =
      List.fold_left
        ~f:(fun (env, args) a ->
          let env, arg = lift env a in
          (env, arg :: args))
        ~init:(env, []) (List.rev lst)
    in
    (env, {e with content = E_global_constant (s, lst)})

let rec toplevel_inner : env -> expression -> expression =
 fun env e ->
  match e.content with
  | E_let_in
      ( ({content = E_closure {binder; body}; _} as e1),
        inline,
        ((var_name, type_expression), e2) ) ->  
    
    let rec aux b =
      match b.content with
      | E_closure {binder; body} ->
        let env, body = aux body in
        env, {b with content = E_closure {binder; body} }
      | _  -> lift {empty_env with functions = var_name :: env.functions; variables = (binder, type_expression) :: env.variables} b
    in
    let env, body = aux body in 
    List.fold_left
      ~f:(fun prev el -> el prev)
      ~init:
        {
          e with
          content =
            E_let_in
              ( {e1 with content = E_closure {binder; body}},
                inline,
                ((var_name, type_expression), toplevel_inner env e2) );
        }
      env.exported_funcs
  | E_let_in (e1, inline, ((var_name, type_expression), e2)) ->
    {
      e with
      content =
        E_let_in
          ( e1,
            inline,
            ((var_name, type_expression), toplevel_inner env e2) );
    }
  | _ -> e

let toplevel = toplevel_inner empty_env
