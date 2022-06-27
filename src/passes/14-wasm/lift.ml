(*
   Lift functions outside, as WebAssembly has no support for nested functions.
*)
open Mini_c.Types
module ValueVar = Stage_common.Types.ValueVar

type env = {
  variables: (var_name * type_expression) list;
  missing: var_name list;
  exported_funcs: (expression -> expression) list;
  replacements: (var_name * var_name * expression) list;
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

let var_type env v =
  match List.find ~f:(fun var -> ValueVar.equal v (fst var)) env.variables with
  | Some (_, type_) -> type_
  | None -> failwith "should not happen"

let rec lift : env -> expression -> env * expression =
 fun env e ->
  let variable_exists = variable_exists env in
  match e.content with
  | E_literal _ -> (env, e)
  | E_closure {binder; body} ->
    let env =
      {env with variables = (binder, e.type_expression) :: env.variables}
    in
    let env, body = lift env body in
    (env, {e with content = E_closure {binder; body}})
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
        List.find env.replacements ~f:(fun (r, _, _) -> ValueVar.equal r v)
      with
      | Some (_, _, x) -> lift env x
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
  | E_application (e1, ({content = E_variable v; _} as e2)) ->
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    let e2 =
      match
        List.find env.replacements ~f:(fun (r, _, _) -> ValueVar.equal r v)
      with
      | Some (_, x, _) -> {e2 with content = E_variable x}
      | None -> e2
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
  | E_variable v when variable_exists v -> (env, e)
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
  | E_let_in
      ( ({content = E_closure _} as c),
        inline,
        thunk,
        ((var_name, type_expression), e2) ) ->
    let env2, c =
      lift
        {
          empty_env with
          replacements = env.replacements;
          functions = env.functions;
        }
        c
    in
    let v = ValueVar.fresh_like var_name in
    let env = {env with variables = (v, type_expression) :: env.variables} in
    let export_func remaining =
      List.fold_left
        ~f:(fun all binder ->
          let type_expression_item = var_type env binder in
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
            location = c.location;
          })
        ~init:c remaining
    in
    let var_replacement remaining init =
      List.fold_left
        ~f:(fun result argument ->
          let type_expression_item = var_type env argument in
          let e2 =
            {
              content = E_variable argument;
              type_expression = type_expression_item;
              location = c.location;
            }
          in
          let t_type_expression =
            {
              type_content =
                T_function (type_expression_item, result.type_expression);
              location = e.type_expression.location;
              source_type = None;
            }
          in
          {
            content = E_application (result, e2);
            type_expression = t_type_expression;
            location = c.location;
          })
        ~init remaining
    in
    let in_replacement i =
      match
        List.find env.replacements ~f:(fun (_, r, _) -> ValueVar.equal r i)
      with
      | Some _ -> true
      | None -> false
    in
    let in_function i =
      match List.find env.functions ~f:(fun r -> ValueVar.equal r i) with
      | Some _ -> true
      | None -> false
    in
    let missing =
      List.filter ~f:(fun f -> not (in_replacement f)) env2.missing
    in
    let missing = List.filter ~f:(fun f -> not (in_function f)) missing in
    let replacement =
      ( var_name,
        v,
        var_replacement missing
          {
            content = E_variable v;
            type_expression = e.type_expression;
            location = c.location;
          } )
    in
    let export = export_func missing in
    let type_expression = export.type_expression in
    let export e =
      {
        content = E_let_in (export, inline, thunk, ((v, type_expression), e));
        type_expression;
        location = c.location;
      }
    in
    let env =
      {
        variables = (v, type_expression) :: env.variables;
        exported_funcs = export :: env.exported_funcs;
        functions = var_name :: env.functions;
        replacements = (replacement :: env2.replacements) @ env.replacements;
        missing = env2.missing @ env.missing;
      }
    in
    let env, e2 = lift env e2 in
    (env, e2)
  | E_let_in (e1, inline, thunk, ((var_name, type_expression), e2)) ->
    let env =
      {env with variables = (var_name, type_expression) :: env.variables}
    in
    let env, e1 = lift env e1 in
    let env, e2 = lift env e2 in
    ( env,
      {
        e with
        content = E_let_in (e1, inline, thunk, ((var_name, type_expression), e2));
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
        thunk,
        ((var_name, type_expression), e2) ) ->
    let var_to_string name =
      let name, hash = ValueVar.internal_get_name_and_counter name in
      name ^ "#" ^ string_of_int hash
    in
    print_endline ("e_let_in x:" ^ var_to_string var_name);
    let env, body =
      lift {empty_env with functions = var_name :: env.functions} body
    in
    List.fold_left
      ~f:(fun prev el -> el prev)
      ~init:
        {
          e with
          content =
            E_let_in
              ( {e1 with content = E_closure {binder; body}},
                inline,
                thunk,
                ((var_name, type_expression), toplevel_inner env e2) );
        }
      env.exported_funcs
  | E_let_in (e1, inline, thunk, ((var_name, type_expression), e2)) ->
    let var_to_string name =
      let name, hash = ValueVar.internal_get_name_and_counter name in
      name ^ "#" ^ string_of_int hash
    in
    print_endline ("e_let_in:" ^ var_to_string var_name);
    {
      e with
      content =
        E_let_in
          ( e1,
            inline,
            thunk,
            ((var_name, type_expression), toplevel_inner env e2) );
    }
  | _ -> e

let toplevel = toplevel_inner empty_env
