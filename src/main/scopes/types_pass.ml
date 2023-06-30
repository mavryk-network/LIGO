open Simple_utils
open Ligo_prim
open Types
module LMap = Simple_utils.Map.Make (Location_ordered)
module Pattern = Ast_typed.Pattern

type t = type_case LMap.t

let empty = LMap.empty

module Of_Ast_typed = struct
  let add_binding
      : t -> Ast_typed.expression_variable * Ast_typed.type_expression option -> t
    =
   fun env binding ->
    let v, t = binding in
    let loc = Value_var.get_location v in
    let type_case =
      match t with
      | None -> Unresolved
      | Some t ->
        let t =
          match t.orig_var with
          | Some t' -> { t with type_content = T_variable t' }
          | None -> t
        in
        Resolved t
    in
    LMap.add loc type_case env


  let add_bindings env bindings = List.fold bindings ~init:env ~f:add_binding

  let rec extract_binding_types_from_signature : t -> Ast_typed.signature -> t =
   fun bindings sig_ ->
    List.fold sig_ ~init:bindings ~f:(fun bindings -> function
      | S_value (v, t, _) -> add_bindings bindings [ v, Some t ]
      | S_type _ -> bindings
      | S_module (_, sig_) -> extract_binding_types_from_signature bindings sig_)


  let rec extract_binding_types : t -> Ast_typed.declaration_content -> t =
   fun prev decl ->
    let aux : t -> Ast_typed.expression -> t =
     fun env exp ->
      let return = add_bindings env in
      let loc = exp.location in
      match exp.expression_content with
      (* FIXME: @Melywn
         What should be the expected behaviour here for erroneous 
         expressions? *)
      | E_error _
      | E_literal _
      | E_application _
      | E_raw_code _
      | E_constructor _
      | E_assign _
      | E_deref _
      | E_while _
      | E_type_abstraction _
      | E_record _
      | E_accessor _
      | E_update _
      | E_constant _ -> return []
      | E_type_inst _ -> return []
      | E_variable v -> return [ v, Some exp.type_expression ]
      | E_lambda { binder; _ } ->
        return [ Param.get_var binder, Some (Param.get_ascr binder) ]
      | E_recursive { fun_name; fun_type; lambda = { binder; _ }; force_lambdarec = _ } ->
        return
          [ fun_name, Some fun_type; Param.get_var binder, Some (Param.get_ascr binder) ]
      | E_let_mut_in { let_binder; rhs = _; _ } | E_let_in { let_binder; rhs = _; _ } ->
        return
        @@ List.map
             ~f:(fun binder -> Binder.get_var binder, Some (Binder.get_ascr binder))
             (Pattern.binders let_binder)
      | E_matching { matchee = _; cases } ->
        let bindings =
          List.concat
          @@ List.map cases ~f:(fun { pattern; _ } ->
                 let binders = Pattern.binders pattern in
                 List.map binders ~f:(fun b -> Binder.get_var b, Some (Binder.get_ascr b)))
        in
        return bindings
      | E_module_accessor { element = e; _ } -> return [ e, Some exp.type_expression ]
      | E_for { binder; start; _ } -> return [ binder, Some start.type_expression ]
      | E_for_each { fe_binder = binder1, Some binder2; collection; _ } ->
        let key_type, val_type = Ast_typed.get_t_map_exn collection.type_expression in
        return [ binder1, Some key_type; binder2, Some val_type ]
      | E_for_each { fe_binder = binder, None; collection; _ } ->
        let type_ = collection.type_expression in
        if Ast_typed.is_t_set type_
        then return [ binder, Some (Ast_typed.get_t_set_exn type_) ]
        else if Ast_typed.is_t_list type_
        then return [ binder, Some (Ast_typed.get_t_list_exn type_) ]
        else if Ast_typed.is_t_map type_
        then (
          let k, v = Ast_typed.get_t_map_exn type_ in
          return [ binder, Some (Ast_typed.t_pair ~loc k v) ])
        else return []
      | E_mod_in { rhs = { signature; _ }; _ } ->
        extract_binding_types_from_signature env signature
    in
    match decl with
    | D_value { attr = { hidden = true; _ }; _ } -> prev
    | D_irrefutable_match { attr = { hidden = true; _ }; _ } -> prev
    | D_value { binder; expr; _ } ->
      let prev = add_bindings prev [ Binder.get_var binder, Some expr.type_expression ] in
      Self_ast_typed.Helpers.fold_expression aux prev expr
    | D_irrefutable_match { pattern; expr; _ } ->
      let prev =
        let f acc binder =
          add_bindings acc [ Binder.get_var binder, Some expr.type_expression ]
        in
        List.fold (Pattern.binders pattern) ~f ~init:prev
      in
      Self_ast_typed.Helpers.fold_expression aux prev expr
    | D_type _ -> prev
    | D_module { module_; _ } ->
      (match module_.module_content with
      | M_variable _ -> prev
      | M_module_path _ -> prev
      | M_struct ds ->
        List.fold_left ds ~init:prev ~f:(fun prev d ->
            extract_binding_types prev d.wrap_content))
end

module Of_Ast_core = struct
  let add_binding_in_map : t -> Location.t * type_case -> t =
   fun env (loc, type_case) -> LMap.add loc type_case env


  let add_bindings_in_map : t -> (Location.t * type_case) list -> t =
   fun env bindings -> List.fold bindings ~init:env ~f:add_binding_in_map


  let add_binders : t -> Ast_core.type_expression option Binder.t list -> t =
   fun env bindings ->
    let bindings =
      List.filter_map
        ~f:(fun binder ->
          match Binder.get_ascr binder with
          | None -> None
          | Some t -> Some (Binder.get_loc binder, Core t))
        bindings
    in
    add_bindings_in_map env bindings


  let add_vvar_type : t -> Value_var.t * Ast_core.type_expression -> t =
   fun env (v, t) ->
    let loc = Value_var.get_location v in
    let t = Core t in
    add_binding_in_map env (loc, t)


  let add_param_type : t -> Ast_core.type_expression Param.t -> t =
   fun env param -> add_vvar_type env (Param.get_var param, Param.get_ascr param)


  let add_param_type_opt : t -> Ast_core.type_expression option Param.t -> t =
   fun env param ->
    match Param.get_ascr param with
    | None -> env
    | Some t -> add_vvar_type env (Param.get_var param, t)


  (** [set_core_type_if_possible] detects patterns like 
      
      {[
        let x : int = 1
      ]}

      The abstraction pass will create AST like
      [D_irrefutable_match 
        (E_ascription ({ expr = E_literal 1; type_annotation = int }), _)]

      So here we want the x to have [Core int]
  
      Hence we extract the type annotation from the rhs and we set it back to
      the binder.
  *)
  let set_core_type_if_possible
      :  Ast_core.type_expression option Binder.t list -> Ast_core.expression
      -> Ast_core.type_expression option Binder.t list * Ast_core.expression
    =
   fun binders expr ->
    match binders, expr.expression_content with
    | [ binder ], Ast_core.E_ascription { anno_expr; type_annotation } ->
      let binder = Binder.set_ascr binder (Some type_annotation) in
      [ binder ], anno_expr
    | _ -> binders, expr


  let rec expression : t -> Ast_core.expression -> t =
   fun bindings expr ->
    match expr.expression_content with
    | E_literal _ | E_variable _ | E_module_accessor _ -> bindings
    | E_raw_code { code; _ } -> expression bindings code
    | E_constant { arguments; _ } -> List.fold arguments ~init:bindings ~f:expression
    | E_application { lamb; args } ->
      let bindings = expression bindings lamb in
      expression bindings args
    | E_type_abstraction { result; _ } -> expression bindings result
    | E_type_in { let_result; _ } -> expression bindings let_result
    | E_constructor { element; _ } -> expression bindings element
    | E_record lmap -> Record.fold lmap ~init:bindings ~f:expression
    | E_accessor { struct_; _ } -> expression bindings struct_
    | E_update { struct_; update; _ } ->
      let bindings = expression bindings struct_ in
      expression bindings update
    | E_ascription { anno_expr; _ } -> expression bindings anno_expr
    | E_assign { binder; expression = e } ->
      let bindings = add_binders bindings [ binder ] in
      expression bindings e
    | E_for { start; final; incr; f_body; _ } ->
      let bindings = expression bindings start in
      let bindings = expression bindings final in
      let bindings = expression bindings incr in
      expression bindings f_body
    | E_for_each { collection; fe_body; _ } ->
      let bindings = expression bindings collection in
      expression bindings fe_body
    | E_while { cond; body } ->
      let bindings = expression bindings cond in
      expression bindings body
    | E_lambda { binder; result; _ } ->
      let bindings = add_param_type_opt bindings binder in
      expression bindings result
    | E_recursive { fun_name; fun_type; lambda = { binder; result; _ }; _ } ->
      let bindings = add_vvar_type bindings (fun_name, fun_type) in
      let bindings = add_param_type bindings binder in
      expression bindings result
    | E_matching { matchee; cases } ->
      let bindings = expression bindings matchee in
      List.fold cases ~init:bindings ~f:(fun bindings { pattern; body } ->
          let bindings = expression bindings body in
          let binders = Pattern.binders pattern in
          add_binders bindings binders)
    | E_let_mut_in { let_binder; rhs; let_result; _ }
    | E_let_in { let_binder; rhs; let_result; _ } ->
      let binders = Pattern.binders let_binder in
      let binders, rhs = set_core_type_if_possible binders rhs in
      let bindings = add_binders bindings binders in
      let bindings = expression bindings rhs in
      expression bindings let_result
    | E_mod_in { rhs; let_result; _ } ->
      let bindings =
        match Location.unwrap rhs with
        | M_variable _ | M_module_path _ -> bindings
        | M_struct decls -> declarations bindings decls
      in
      expression bindings let_result


  and declaration : t -> Ast_core.declaration -> t =
   fun bindings decl ->
    match Location.unwrap decl with
    | D_value { binder; expr; _ } ->
      let binders, expr = set_core_type_if_possible [ binder ] expr in
      let bindings = add_binders bindings binders in
      expression bindings expr
    | D_irrefutable_match { pattern; expr; _ } ->
      let binders = Pattern.binders pattern in
      let binders, expr = set_core_type_if_possible binders expr in
      let bindings = add_binders bindings binders in
      expression bindings expr
    | D_type _ -> bindings
    | D_module { module_ = { wrap_content = M_struct decls; _ }; _ } ->
      declarations bindings decls
    | D_module { module_ = { wrap_content = M_variable _; _ }; _ }
    | D_module { module_ = { wrap_content = M_module_path _; _ }; _ } -> bindings
    | D_signature _ -> bindings


  and declarations : t -> Ast_core.declaration list -> t =
   fun bindings decls -> List.fold decls ~init:bindings ~f:declaration
end

module Typing_env = struct
  (** The typer normall call {!Trace.error} which internally always calls {!Stdlib.raise}
      which stops the program, But here we want to recover from the error. That is the reason 
      we use {!collect_warns_and_errs} *)
  let collect_warns_and_errs
      ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
      tracer
      (es, ws)
    =
    List.iter ws ~f:raise.warning;
    List.iter es ~f:(fun e -> raise.log_error (tracer e))


  let collect_recovered_errors_from_module prg =
    Self_ast_typed.Helpers.fold_expression_in_module
      (fun errs (expr : Ast_typed.expression) ->
        match expr.expression_content with
        | E_error { error; _ } -> error :: errs
        | _ -> errs)
      []
      prg


  let resolve
      ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
      ~options
      ~stdlib_env
      decls
    =
    let typed_prg =
      Simple_utils.Trace.to_stdlib_result
      @@ Checking.type_program ~options ~env:stdlib_env ~should_recover:true decls
    in
    Result.(
      match typed_prg with
      | Ok (prg, ws) ->
        let bindings =
          List.fold_left prg ~init:empty ~f:(fun bindings decl ->
              Of_Ast_typed.extract_binding_types bindings decl.wrap_content)
        in
        let () = List.iter ws ~f:raise.warning in
        let es = collect_recovered_errors_from_module prg in
        collect_warns_and_errs ~raise Main_errors.scopes_recovered_error (es, ws);
        prg, bindings
      | Error (e, ws) ->
        collect_warns_and_errs ~raise Main_errors.checking_tracer ([ e ], ws);
        (* For now, assume errors are recorded in the AST *)
        [], empty)


  (* (match Location.unwrap decl with
        | D_module { module_; module_binder; _ } ->
          { tenv with
            type_env =
              (tenv.type_env
              @
              let sig_ = sig_of_module module_ ~original:tenv.type_env in
              [ Ast_typed.S_module (module_binder, sig_) ])
          }
        | _ -> tenv)) *)

  let self_ast_typed_pass
      ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
      ~(options : Compiler_options.middle_end)
      decls
    =
    ignore options;
    match Simple_utils.Trace.to_stdlib_result @@ Self_ast_typed.all_program decls with
    | Ok (_, ws) -> List.iter ws ~f:raise.warning
    | Error (e, ws) ->
      collect_warns_and_errs ~raise Main_errors.self_ast_typed_tracer ([ e ], ws)
end

(** [resolve] takes your [Ast_core.program] and gives you the typing information
    in the form of [t]
   
    Here we run the typer related thing first because in the following example
    {[
      let x : int = 1
      let t : int = "2"
      let y = x + ""
    ]}

    In the first pass for each [Ast_core.declaration] we [Typing_env.update_typing_env]
    this gives use

    pass1 (just typer) -> [ x -> resolved (int) ; t -> unresolved ; y -> unresolved ]

    After running the typer we traverse the [Ast_core.program] to fill in the [t]
    with type annotations available in the program 
    
    pass2 -> [ x -> resolved (int) ; t -> core (int) ; y -> unresolved ]

    {i Note:} If we do pass2 before pass1 we will end up with
    [ x -> resolved (int) ; t -> unresolved ; y -> unresolved ]
    but the lsp expects the type t should be [int] (which is annotated by the user)
*)
let resolve
    :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
    -> options:Compiler_options.middle_end -> stdlib_decls:Ast_typed.program
    -> Ast_core.program -> t
  =
 fun ~raise ~options ~stdlib_decls prg ->
  let stdlib_env = Ast_typed.to_signature stdlib_decls in
  let tprg, bindings = Typing_env.resolve ~raise ~options ~stdlib_env prg in
  let () = Typing_env.self_ast_typed_pass ~raise ~options tprg in
  Of_Ast_core.declarations bindings prg


let rec patch : t -> Types.def list -> Types.def list =
 fun bindings defs ->
  List.map defs ~f:(fun def ->
      match def with
      | Variable v ->
        (match v.t, LMap.find_opt v.range bindings with
        | Unresolved, Some t -> Types.Variable { v with t }
        | _ -> Variable v)
      | Type t -> Type t
      | Module m ->
        let mod_case =
          match m.mod_case with
          | Alias (a, resolved) -> Types.Alias (a, resolved)
          | Def defs -> Def (patch bindings defs)
        in
        Module { m with mod_case })
