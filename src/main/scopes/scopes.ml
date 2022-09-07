open Ligo_prim
open Types
open Misc

module Formatter = Formatter
module Api_helper = Api_helper

type sub_module = { type_env : tenv  ; bindings : bindings_map }

let scopes : with_types:bool -> options:Compiler_options.middle_end -> Ast_core.program -> (def_map * scopes) = fun ~with_types ~options core_prg ->
  let make_v_def_from_core = make_v_def_from_core ~with_types  in
  let make_v_def_option_type = make_v_def_option_type ~with_types in

  let rec find_scopes' = fun (i,all_defs,env,scopes,lastloc) (bindings:bindings_map) (e : Ast_core.expression) ->
    match e.expression_content with
    | E_let_in { let_result ; attr= { hidden = true ; _ } ; _ } -> (
      find_scopes' (i,all_defs,env,scopes,let_result.location) bindings let_result
    )
    | E_let_in { let_binder = {var ; ascr ; attributes=_} ; rhs ; let_result ; attr=_} -> (
      let (i,all_defs,_, scopes) = find_scopes' (i,all_defs,env,scopes,e.location) bindings rhs in
      let def = make_v_def_option_type bindings var ascr (Value_var.get_location var) rhs.location in
      let (i,env) = add_shadowing_def (i,get_binder_name var) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,let_result.location) bindings let_result
    )
    | E_type_in { type_binder; rhs ; let_result } -> (
      let def = make_t_def (get_type_binder_name type_binder) e.location rhs in
      let (i,env) = add_shadowing_def (i,get_type_binder_name type_binder) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,let_result.location) bindings let_result
    )
    | E_mod_in { module_binder; rhs; let_result } -> (
      let (i,new_outer_def_map,scopes) = module_expr ~options ~env ~scopes i rhs in
      let def = make_m_def (get_mod_binder_name module_binder) e.location new_outer_def_map in
      let env = Def_map.add (get_mod_binder_name module_binder) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,let_result.location) bindings let_result
    )
    | E_recursive { fun_name ; fun_type ; lambda = { binder = {var;ascr=input_type; attributes=_} ; output_type = _ ; result ; _ } } -> (
      let (i,env) =
        let def = make_v_def_option_type bindings fun_name (Some fun_type) (Value_var.get_location fun_name) result.location in
        add_shadowing_def (i,get_binder_name fun_name) def env
      in
      let (i,env) =
        let def = make_v_def_option_type bindings var (Some input_type) (Value_var.get_location var) result.location in
        add_shadowing_def (i,get_binder_name var) def env
      in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,result.location) bindings result
    )
    | E_lambda { binder={var;ascr=input_type; attributes=_} ; output_type = _ ; result } -> (
      let (i,env) =
        let def = make_v_def_option_type bindings var input_type (Value_var.get_location var) result.location in
        add_shadowing_def (i,get_binder_name var) def env
      in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,result.location) bindings result
    )
    | E_type_abstraction { type_binder; result } -> (
      let def = make_t_def (get_type_binder_name type_binder) e.location (Ast_core.t_variable type_binder ()) in
      let (i,env) = add_shadowing_def (i,get_type_binder_name type_binder) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,result.location) bindings result
    )
    | E_matching {matchee; cases} -> (
      let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,matchee.location) bindings matchee in
      let aux = fun (i,all_defs,scopes) ({pattern;body}: (Ast_core.expression,_) Match_expr.match_case) ->
        let aux (i,env) (p: _ Pattern.t) =
          match p.wrap_content with
          | P_var binder ->
            let loc = Value_var.get_location binder.var in
            let proj_def = make_v_def_from_core bindings binder.var loc loc in
            add_shadowing_def (i,get_binder_name binder.var) proj_def env
          | _ -> (i,env)
        in
        let (i,env) = Pattern.fold_pattern aux (i,env) pattern in
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,body.location) bindings body in
        let all_defs = merge_defs env all_defs in
        (i,all_defs,scopes)
      in
      let (i,all_defs,scopes) = List.fold_left ~f:aux ~init:(i,all_defs,scopes) cases in
      (i,all_defs,env,scopes)
    )
    | E_record emap -> (
      let aux = fun (i,all_defs,scopes) (exp:Ast_core.expression) ->
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,exp.location) bindings exp in
        (i,all_defs,scopes)
      in
      let (i,all_defs,scopes) = List.fold_left ~f:aux ~init:(i,all_defs,scopes) (Record.LMap.to_list emap) in
      (i,all_defs,env,scopes)
    )
    | E_update { struct_ ; update ; _ } -> (
      (*TODO: here record has a virtual location, check this out.. not normal *)
      let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,struct_.location) bindings struct_ in
      find_scopes' (i,all_defs,env,scopes,update.location) bindings update
    )
    | E_constant { arguments ; _ } -> (
      let aux = fun (i,all_defs,scopes) (exp:Ast_core.expression) ->
        let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,exp.location) bindings exp in
        (i,all_defs,scopes)
      in
      let (i,all_defs,scopes) = List.fold_left ~f:aux ~init:(i,all_defs,scopes) arguments in
      (i,all_defs,env,scopes)
    )
    | E_application { lamb ; args } -> (
      let (i,all_defs,_,scopes) = find_scopes' (i,all_defs,env,scopes,lamb.location) bindings lamb in
      find_scopes' (i,all_defs,env,scopes,args.location) bindings args
    )
    | E_ascription { anno_expr=e;_ } | E_accessor { struct_=e;_ } | E_constructor { element=e;_ } -> (
      find_scopes' (i,all_defs,env,scopes,e.location) bindings e
    )
    | E_module_accessor _ -> (
      (* TODOREWORK we should update all_defs so that references to variable accessed are taken into account *)
      let scopes = add_scope (lastloc, env) scopes in
      (i,all_defs,env,scopes)
    )
    | E_variable x -> (
      let env = add_reference x env in
      let all_defs = merge_defs env all_defs in
      let scopes = add_scope (lastloc, env) scopes in
      (i,all_defs,env,scopes)
    )
    | E_literal _ | E_raw_code _ -> (
      let scopes = add_scope (lastloc, env) scopes in
      (i,all_defs,env,scopes)
    )
    | E_assign { binder ; expression ; _ } -> (
      let def = make_v_def_option_type bindings binder.var binder.ascr (Value_var.get_location binder.var) expression.location in
      let (i,env) = add_shadowing_def (i, get_binder_name binder.var) def env in
      let all_defs = merge_defs env all_defs in
      find_scopes' (i,all_defs,env,scopes,expression.location) bindings expression
    )
    | E_for _ ->
      failwith ("TODO "^__LOC__)
    | E_for_each _ ->
      failwith ("TODO "^__LOC__)
    | E_while _ ->
      failwith ("TODO "^__LOC__)
  and find_scopes (i,top_lvl_defs,scopes,loc) bindings  e =
    let (i,defs,_,scopes) = find_scopes' (i,top_lvl_defs,top_lvl_defs,scopes,loc) bindings e in
    (i,defs,scopes)

  and module_expr ~options ~env ~scopes i (me: Ast_core.module_expr) =
    match me.wrap_content with
    | M_struct decls -> (
      let (i,new_def_map,_,scopes,_) = module_ ~options i decls in
      (i,new_def_map,scopes)
    )
    | M_module_path path -> (
      let aux (env:def_map) binder : def_map =
        match Def_map.find_opt (get_mod_binder_name binder) env with
        | Some (Module m) -> m.content
        | _ -> env
      in
      let def_map = List.fold_left ~f:aux ~init:env(List.Ne.to_list path) in
      (i,def_map,scopes)
    )
    | M_variable mv -> (
      let env_opt = Def_map.find_opt (get_mod_binder_name mv) env in
      let def_map = match env_opt with
        | Some (Module m) -> m.content
        | _ -> Def_map.empty
      in
      (i,def_map,scopes)
    )

  and declaration = fun (i,top_def_map,inner_def_map,scopes,partials) (decl : Ast_core.declaration) ->
    let compile_declaration ~raise env decl () = Checking.type_declaration ~raise ~options ~env decl in
    let typed_prg =
      (*
        if --with-types optional flag is enabled, we try typing the declaration
        to build a partial Ast_typed program.
        if a declaration do not type, we will still try to type the next one
      *)
      if with_types then Simple_utils.Trace.to_option (compile_declaration partials.type_env decl ())
      else None
    in
    let partials = match typed_prg with
      | Some (decl') ->
        let bindings = extract_variable_types partials.bindings decl'.wrap_content in
        let type_env = Environment.add_declaration decl' partials.type_env in
        { type_env ; bindings }
      | None -> partials
    in
    match decl.wrap_content with
    | D_value { attr = { hidden = true ; _ } ; _ } -> (
      ( i, top_def_map, inner_def_map, scopes , partials )
    )
    | D_value { binder= { var ; ascr ; attributes=_ } ; expr ; _ } -> (
      let (i,new_inner_def_map,scopes) = find_scopes (i,top_def_map,scopes,decl.location) partials.bindings expr in
      let inner_def_map = merge_defs new_inner_def_map inner_def_map in
      let def = make_v_def_option_type partials.bindings var ascr (Value_var.get_location var) expr.location in
      let (i,top_def_map) = add_shadowing_def (i,get_binder_name var) def top_def_map in
      ( i, top_def_map, inner_def_map, scopes , partials )
    )
    | D_type {type_attr={hidden = true; _} ; _} -> (
      ( i, top_def_map, inner_def_map, scopes, partials )
    )
    | D_type {type_binder; type_expr ; type_attr=_} -> (
      let def = make_t_def (get_type_binder_name type_binder) decl.location type_expr in
      let (i,top_def_map) = add_shadowing_def (i,get_type_binder_name type_binder) def top_def_map in
      ( i, top_def_map, inner_def_map, scopes, partials )
    )
    | D_module {module_attr={hidden = true; _} ; _} -> (
      ( i, top_def_map, inner_def_map, scopes, partials )
    )
    | D_module {module_binder; module_ ; module_attr=_} -> (
      let (i,new_outer_def_map,scopes) = module_expr ~options ~env:top_def_map ~scopes i module_ in
      let def = make_m_def (get_mod_binder_name module_binder) decl.location new_outer_def_map in
      let top_def_map = Def_map.add (get_mod_binder_name module_binder) def top_def_map in
      ( i, top_def_map, inner_def_map, scopes, partials )
    )

  and decl = fun (i,top_def_map,inner_def_map,scopes,partials) (decl : Ast_core.decl) ->
    declaration (i,top_def_map,inner_def_map,scopes,partials) decl
  and module_ ~options i m =
    let init = { type_env = options.init_env ; bindings = Bindings_map.empty } in
    List.fold_left ~f:decl ~init:(i, Def_map.empty, Def_map.empty, [], init) m
  in
  let program ~(options : Compiler_options.middle_end) i core_prg =
    let init = { type_env = options.init_env ; bindings = Bindings_map.empty } in
    List.fold_left ~f:declaration ~init:(i, Def_map.empty, Def_map.empty, [], init) core_prg
  in
  let (_,top_d,inner_d,s,_) = program ~options 0 core_prg in
  let d = Def_map.union merge_refs top_d inner_d in
  (d,s)
