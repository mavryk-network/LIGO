(*
   Lift functions outside, as WebAssembly has no support for nested functions.
*)
open Mini_c.Types
module Value_var = Ligo_prim.Value_var

type replacements = (var_name * var_name) list

type env = {
  variables: (var_name * type_expression) list;
  missing: var_name list;
  exported_funcs: (replacements -> var_name list -> expression -> var_name list * expression) list;
  replacements: replacements;
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
  match List.find ~f:(fun var -> Value_var.equal v (fst var)) env.variables with
  | Some _ -> true
  | None -> false

let var_to_string name =
  let name, hash = Value_var.internal_get_name_and_counter name in
  name ^ "#" ^ string_of_int hash

let var_type env v =
  match List.find ~f:(fun var -> Value_var.equal v (fst var)) env.variables with
  | Some (_, type_) -> type_
  | None -> failwith ("should not happen:" ^ var_to_string v)

let rec lift : env -> expression -> env * expression =
 fun env e ->
  match e.content with
  | E_literal _ -> (env, e)
  | E_closure {binder; body} ->
    let env2 = 
      {empty_env with 
          variables    = (binder, e.type_expression) :: []; 
          replacements = env.replacements; 
          functions    = env.functions;
          exported_funcs = env.exported_funcs
      }
    in
    
    let rec find_body env2 a = 
      match a.content with 
        E_closure {binder; body} -> 
          let env2 = {env2 with variables = (binder, a.type_expression) :: env2.variables } in 
          let env2, body = find_body env2 body in
          env2, {a with content = E_closure { binder; body }}
      | _ -> 
        lift env2 a
    in
    let env, body = find_body env2 body in
    
    let v = Value_var.fresh () ~name: "moved_function" in

    let env = { 
      env2 with 
        variables = 
            (v, e.type_expression) :: env.variables;
        exported_funcs = env.exported_funcs;
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
      match List.find env.functions ~f:(fun r -> Value_var.equal r i) with
      | Some _ -> true
      | None -> false
    in
    let missing = env.missing in    
    let missing = List.filter ~f:(fun f -> not (in_function f)) missing in
    let export = export_func missing in
    let type_expression = export.type_expression in
    let export replacements (funcs: var_name list) e =
      
      List.iter ~f:(fun (a, b) -> print_endline ("Check: " ^ var_to_string a ^ " with " ^ var_to_string  b ^ ".")) replacements;
      let v = match (List.find replacements ~f:(fun (_, r) ->  Value_var.equal r v)) with
      | Some (x, _) -> 
        print_endline "YES HERE";
        x
      | None -> v
      in
      (match (List.find funcs ~f:(fun a -> Value_var.equal a v)) with
      | Some s -> 
        print_endline ("in funcs here:" ^ var_to_string s);
        funcs, e
        
      | None -> 
        print_endline ("not in funcs here:" ^ var_to_string v);
        v :: funcs, {        
        content = E_let_in (export, false, ((v, type_expression), e));
        type_expression;
        location = e.location;
      })
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
  | E_application (({content = E_variable _v; _} as e1), e2) ->
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
  | E_variable v when variable_exists env v -> 
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
  | E_let_mut_in (e1, (b, e2)) ->
    let env, e1 = lift env e1 in 
    let env, e2 = lift env e2 in 
    (env, {e with content = E_let_mut_in (e1, (b, e2))})
  | E_let_in     (e1, inline, ((var_name, type_expression), e2)) ->
    let env, e1 = lift env e1 in
    let env = 
      (match e1.content with 
        E_variable v ->
          {env with replacements = (var_name, v) :: env.replacements}
      | _ ->
        env
      )
    in
    let env, e2 = lift env e2 in
    env, (match e1.content with 
      E_variable _ ->
        e2
    | _ -> 
      {
        e with
        content = E_let_in (e1, inline, ((var_name, type_expression), e2));
      }
    );
    
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
  | E_deref _ -> (env, e)
  | E_assign (binder, expression) -> 
    let env, expression = lift env expression in 
    (env, {e with content = E_assign (binder, expression) })
  | E_for_each (collection, collection_type, (fe_binder, fe_body)) ->
    let env, collection = lift env collection in 
    let env, fe_body = lift env fe_body in 
    (env, {e with content = E_for_each (collection, collection_type, (fe_binder, fe_body)) })
  | E_for (start, final, inc, (b, f_body))  ->
    let env, start = lift env start in 
    let env, final = lift env final in 
    let env, inc = lift env inc in 
    let env, f_body = lift env f_body in 
    (env, {e with content = E_for (start, final, inc, (b, f_body)) })
  | E_while (cond, body) ->
    let env, cond = lift env cond in 
    let env, body = lift env body in
    (env, {e with content = E_while (cond, body)})

let rec toplevel_inner : env -> string -> expression -> expression =
 fun env entrypoint e ->
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
      | _  ->         
        lift {empty_env with functions = var_name :: env.functions; variables = (binder, type_expression) :: env.variables} b
    in
    let env, body = aux body in 
    let _, r = List.fold_left
      ~f:(fun (functions, prev) export -> 
        export env.replacements functions prev
      )
      ~init:
        (env.functions, {
          e with
          content =
            E_let_in
              ( {e1 with content = E_closure {binder; body}},
                inline,
                ((var_name, type_expression), toplevel_inner env entrypoint e2) );
        })
      env.exported_funcs
    in 
    r
  | E_let_in (e1, inline, ((var_name, type_expression), e2)) ->
    {
      e with
      content =
        E_let_in
          ( e1,
            inline,
            ((var_name, type_expression), toplevel_inner env entrypoint  e2) );
    }
  | E_closure _ as e1 -> 
    let ep = Value_var.of_input_var entrypoint in
    toplevel_inner env entrypoint { e with 
      content = E_let_in (
        {e with content = e1}, 
        false, 
        ((ep, e.type_expression), {content = E_variable ep; location = (Location.generated) ; type_expression = { type_content = T_base TB_unit; location = Location.generated; source_type = None }}))} 
  | _ -> e

let toplevel = toplevel_inner empty_env
