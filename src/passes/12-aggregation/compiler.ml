module I = Ast_typed
module O = Ast_aggregated

(* open Simple_utils.Trace *)
open Ligo_prim

(*
  This pass does the following:
  - aggregates declarations into a chain of let-ins
  - flatten modules into let-ins
*)

module Data = struct
  type scope =
    { exp : var_binding list
    ; mod_ : mod_ list
    ; decls : decl list
    }

  and decl =
    | Mod of mod_
    | Exp of exp_
    | Pat of pat_

  and ('a, 'b) binding_ =
    { name : 'a
    ; fresh_name : 'b
    }

  and var_binding = (Value_var.t, Value_var.t) binding_
  and pat_binding = (I.ty_expr I.Pattern.t, O.ty_expr O.Pattern.t) binding_

  and mod_ =
    { name : Module_var.t
    ; in_scope : scope
    }

  and pat_ =
    { binding : pat_binding
    ; item : O.expression
    ; attr : O.ValueAttr.t
    }

  and exp_ =
    { binding : var_binding
    ; item : O.expression
    ; attr : O.ValueAttr.t
    }

  (* Important note: path is _only_ used for naming of fresh variables, so that debuging a printed AST is easier *)
  and path = Module_var.t list

  module PP_DEBUG = struct
    (* open Format *)
    (* open Simple_utils.PP_helpers *)

    (* let rec pp ppf { exp; mod_ } =
      let pp_mod_ ppf { name; in_scope } =
        fprintf ppf "{ name = %a ; items = @[<v 2>@.%a@] }" Module_var.pp name pp in_scope
      in
      let pp_exp_ ppf { name; fresh_name; item = _ } =
        fprintf
          ppf
          "{ name = %a ; fresh_name = %a ; items = XX }"
          Value_var.pp
          name
          Value_var.pp
          fresh_name (*Ast_aggregated.PP.expression item*)
      in
      fprintf
        ppf
        "{ exp = @[<v>%a@] ; mod_ = @[<v 2>@.%a@] }"
        Simple_utils.PP_helpers.(list_sep pp_exp_ (tag "@."))
        exp
        Simple_utils.PP_helpers.(list_sep pp_mod_ (tag "@."))
        mod_ *)
  
  end

  let empty = { exp = []; mod_ = []; decls = [] }

  let resolve_path : scope -> path -> scope =
   fun scope requested_path ->
    let f : scope -> Module_var.t -> scope =
     fun acc module_variable ->
      match List.find acc.mod_ ~f:(fun x -> Module_var.equal x.name module_variable) with
      | Some x -> x.in_scope
      | _ -> failwith "xx"
     (* (Format.asprintf
             "couldnt find %a in: \n %a "
             Module_var.pp
             module_variable
             PP_DEBUG.pp
             scope) *)
    in
    List.fold requested_path ~init:scope ~f


  let rm_exp : scope -> I.expression_variable -> scope =
   fun items to_rm ->
    let exp = List.filter items.exp ~f:(fun x -> not @@ Value_var.equal x.name to_rm) in
    { items with exp }


  let add_exp : scope -> exp_ -> scope =
   fun scope new_exp ->
    let exp =
      List.filter scope.exp ~f:(fun x ->
          not (Value_var.equal x.name new_exp.binding.name))
    in
    { scope with exp = new_exp.binding :: exp; decls = Exp new_exp :: scope.decls }


  let add_exp_pat : scope -> pat_ -> scope =
   fun scope new_pat ->
    let new_bound : var_binding list =
      let names = List.map ~f:Binder.get_var @@ I.Pattern.binders new_pat.binding.name in
      let fresh_names =
        List.map ~f:Binder.get_var @@ I.Pattern.binders new_pat.binding.fresh_name
      in
      List.map
        ~f:(fun (name, fresh_name) -> { name; fresh_name })
        (List.zip_exn names fresh_names)
    in
    let exp =
      List.filter scope.exp ~f:(fun x ->
          not @@ List.exists new_bound ~f:(fun new_ -> Value_var.equal x.name new_.name))
    in
    { scope with exp = new_bound @ exp; decls = Pat new_pat :: scope.decls }


  let add_module : scope -> Module_var.t -> scope -> scope =
   fun scope mod_var new_scope ->
    let mod_ =
      List.filter scope.mod_ ~f:(fun x -> not (Module_var.equal x.name mod_var))
    in
    let mod_ = { name = mod_var; in_scope = new_scope } :: mod_ in
    let decls = Mod { name = mod_var; in_scope = new_scope } :: scope.decls in
    { scope with mod_; decls }


  let resolve_variable : scope -> Value_var.t -> Value_var.t =
   fun scope v ->
    match List.find scope.exp ~f:(fun x -> Value_var.equal v x.name) with
    | Some x -> x.fresh_name
    | None -> v


  let resolve_variable_in_path : scope -> Module_var.t list -> Value_var.t -> Value_var.t =
   fun scope path v ->
    let x = resolve_path scope path in
    resolve_variable x v
end

let aggregate_scope : Data.scope -> leaf:O.expression -> O.expression =
 fun scope ~leaf ->
  let rec f : O.expression -> Data.decl -> O.expression =
   fun acc_exp d ->
    match d with
    | Pat { binding = { name = _; fresh_name }; item; attr } ->
      O.e_a_let_in ~loc:item.location fresh_name item acc_exp attr
    | Exp { binding = { name = _; fresh_name }; item; attr } ->
      let binder =
        O.Pattern.var ~loc:(Value_var.get_location fresh_name)
        @@ Binder.make fresh_name item.type_expression
      in
      O.e_a_let_in ~loc:item.location binder item acc_exp attr
    | Mod { in_scope = { decls; _ }; _ } -> List.fold_left decls ~f ~init:acc_exp
  in
  List.fold_left scope.decls ~f ~init:leaf


let build_context : Data.scope -> O.context =
 fun scope ->
  let rec f : Data.decl -> O.declaration list =
   fun d ->
    match d with
    | Pat { binding = { name = _; fresh_name }; item; attr } ->
      [ Location.wrap
          ~loc:item.location
          (O.D_irrefutable_match { pattern = fresh_name; expr = item; attr })
      ]
    | Exp { binding = { name = _; fresh_name }; item; attr } ->
      let binder = Binder.make fresh_name item.type_expression in
      [ Location.wrap ~loc:item.location (O.D_value { binder; expr = item; attr }) ]
    | Mod { in_scope = { decls; _ }; _ } -> List.join (List.map decls ~f)
  in
  List.join (List.map ~f scope.decls)


let rec compile ~raise : Data.scope -> Data.path -> I.expression -> I.program -> O.program
  =
 fun scope path hole module_ ->
  let scope = compile_declarations ~raise scope path module_ in
  let hole = compile_expression ~raise scope [] hole in
  build_context scope, aggregate_scope scope ~leaf:hole


and compile_declarations ~raise : Data.scope -> Data.path -> I.module_ -> Data.scope =
 fun init_scope path lst ->
  let f : Data.scope -> I.declaration -> Data.scope =
   fun acc_scope decl ->
    match decl.wrap_content with
    | I.D_type _ -> acc_scope
    | I.D_irrefutable_match { pattern; expr; attr } ->
      let pat =
        let item = compile_expression ~raise acc_scope [] expr in
        let fresh_name = fresh_pattern ~raise pattern path in
        (* Data.{ name = binder.var; fresh_name; item; attr; attributes = binder.attributes } *)
        (Data.{ binding = { name = pattern; fresh_name }; item; attr } : Data.pat_)
      in
      Data.add_exp_pat acc_scope pat
    | I.D_value { binder; expr; attr } ->
      let exp =
        let item = compile_expression ~raise acc_scope [] expr in
        let fresh_name = fresh_name binder.var path in
        (* Data.{ name = binder.var; fresh_name; item; attr; attributes = binder.attributes } *)
        (Data.{ binding = { name = binder.var; fresh_name }; item; attr } : Data.exp_)
      in
      Data.add_exp acc_scope exp
    | I.D_module { module_binder; module_; module_attr = _ } ->
      let rhs_glob =
        compile_module_expr ~raise acc_scope (path @ [ module_binder ]) module_
      in
      Data.add_module acc_scope module_binder rhs_glob
  in
  List.fold lst ~init:init_scope ~f


and compile_module_expr ~raise : Data.scope -> Data.path -> I.module_expr -> Data.scope =
 fun scope path mexpr ->
  match mexpr.module_content with
  | M_struct prg -> compile_declarations ~raise { scope with decls = [] } path prg
  | M_variable v ->
    let res = Data.resolve_path scope [ v ] in
    { res with decls = [] }
  | M_module_path path ->
    let res = Data.resolve_path scope (List.Ne.to_list path) in
    { res with decls = [] }


and compile_type ~raise : I.type_expression -> O.type_expression =
 fun ty ->
  let self = compile_type ~raise in
  let return type_content : O.type_expression =
    { type_content
    ; orig_var = ty.orig_var
    ; location = ty.location
    ; source_type = Some ty
    }
  in
  match ty.type_content with
  | T_variable x -> return (T_variable x)
  | T_constant { language; injection; parameters } ->
    let parameters = List.map parameters ~f:self in
    return (T_constant { language; injection; parameters })
  | T_sum r ->
    let r = I.Row.map self r in
    return (T_sum r)
  | T_record r ->
    let r = I.Row.map self r in
    return (T_record r)
  | T_arrow { type1; type2 } ->
    let type1 = self type1 in
    let type2 = self type2 in
    return (T_arrow { type1; type2 })
  | T_singleton x -> return (T_singleton x)
  | T_abstraction _ -> assert false
  | T_for_all { ty_binder; kind; type_ } ->
    let type_ = self type_ in
    return (T_for_all { ty_binder; kind; type_ })


and compile_expression ~raise : Data.scope -> Data.path -> I.expression -> O.expression =
 fun scope path expr ->
  let self ?(data = scope) = compile_expression ~raise data path in
  let self_ty = compile_type ~raise in
  let return expression_content : O.expression =
    let type_expression = compile_type ~raise expr.type_expression in
    { expression_content; type_expression; location = expr.location }
  in
  match expr.expression_content with
  (* resolving variable names *)
  | I.E_variable v ->
    let v = Data.resolve_variable scope v in
    return (O.E_variable v)
  | I.E_module_accessor { module_path; element } ->
    let v = Data.resolve_variable_in_path scope module_path element in
    return (O.E_variable v)
  (* bounding expressions *)
  | I.E_matching { matchee; cases } ->
    let cases =
      List.map
        ~f:(fun { pattern; body } ->
          let data =
            List.fold
              ~init:scope
              ~f:(fun scope binder -> Data.rm_exp scope (Binder.get_var binder))
              (I.Pattern.binders pattern)
          in
          O.Match_expr.{ pattern = I.Pattern.map self_ty pattern; body = self ~data body })
        cases
    in
    return (O.E_matching { matchee = self matchee; cases })
  | I.E_for { binder; start; final; incr; f_body } ->
    let data = Data.rm_exp scope binder in
    return
      (O.E_for
         { binder
         ; start = self start
         ; final = self final
         ; incr = self incr
         ; f_body = self ~data f_body
         })
  | I.E_recursive
      { fun_name; fun_type; lambda = { binder; result; output_type }; force_lambdarec } ->
    let data = Data.rm_exp scope (Param.get_var binder) in
    let data = Data.rm_exp data fun_name in
    let result = self ~data result in
    let fun_type = self_ty fun_type in
    let output_type = self_ty output_type in
    let binder = Param.map self_ty binder in
    return
    @@ O.E_recursive
         { fun_name; fun_type; lambda = { binder; result; output_type }; force_lambdarec }
  | I.E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let data =
      List.fold
        ~init:scope
        ~f:(fun scope binder -> Data.rm_exp scope (Binder.get_var binder))
        (I.Pattern.binders let_binder)
    in
    let rhs = self rhs in
    let let_result = self ~data let_result in
    let let_binder = I.Pattern.map self_ty let_binder in
    return @@ O.E_let_in { let_binder; rhs; let_result; attributes }
  | I.E_let_in { let_binder; rhs; let_result; attributes } ->
    let data =
      List.fold
        ~init:scope
        ~f:(fun scope binder -> Data.rm_exp scope (Binder.get_var binder))
        (I.Pattern.binders let_binder)
    in
    let rhs = self rhs in
    let let_result = self ~data let_result in
    let let_binder = I.Pattern.map self_ty let_binder in
    return @@ O.E_let_in { let_binder; rhs; let_result; attributes }
  | I.E_for_each { fe_binder = b, b_opt; collection; collection_type; fe_body } ->
    let data = Data.rm_exp scope b in
    let data = Option.value_map b_opt ~default:data ~f:(fun b -> Data.rm_exp data b) in
    return
      (O.E_for_each
         { fe_binder = b, b_opt
         ; collection = self collection
         ; collection_type
         ; fe_body = self ~data fe_body
         })
  (* recursively call aggregation *)
  | I.E_mod_in { module_binder; rhs; let_result } ->
    let data =
      let rhs_scope =
        compile_module_expr
          ~raise
          scope
          (Module_var.add_prefix "LOCAL#in" module_binder :: path)
          rhs
      in
      Data.add_module scope module_binder rhs_scope
    in
    let x = Data.resolve_path data [ module_binder ] in
    aggregate_scope x ~leaf:(self ~data let_result)
  (* trivials *)
  | I.E_literal l -> return (O.E_literal l)
  | I.E_raw_code x -> return (O.E_raw_code (Raw_code.map self x))
  | I.E_accessor x -> return (O.E_accessor (I.Accessor.map self x))
  | I.E_record m -> return (O.E_record (Record.map m ~f:self))
  | I.E_update x -> return (O.E_update (I.Update.map self x))
  | I.E_constructor x -> return (O.E_constructor (Constructor.map self x))
  | I.E_application x -> return (O.E_application (Application.map self x))
  | I.E_lambda x -> return (O.E_lambda (Lambda.map self self_ty x))
  | I.E_type_abstraction x -> return (O.E_type_abstraction (Type_abs.map self x))
  | I.E_type_inst { forall; type_ } ->
    return (O.E_type_inst { forall = self forall; type_ = self_ty type_ })
  | I.E_constant x -> return (O.E_constant (Constant.map self x))
  | I.E_assign x -> return (O.E_assign (Assign.map self self_ty x))
  | I.E_deref x -> return (O.E_deref x)
  | I.E_while x -> return (O.E_while (While_loop.map self x))


and fresh_name : Value_var.t -> Data.path -> Value_var.t =
 fun v path ->
  match path with
  | [] -> v
  | _ ->
    let name, _ = Value_var.internal_get_name_and_counter v in
    let name =
      List.fold_right ~f:(fun s r -> Module_var.to_name_exn s ^ "#" ^ r) ~init:name path
    in
    let name = "#" ^ name in
    Value_var.fresh ~loc:(Value_var.get_location v) ~name ()


and fresh_pattern ~raise : I.ty_expr I.Pattern.t -> Data.path -> O.ty_expr O.Pattern.t =
 fun pattern path ->
  let pattern = I.Pattern.map (compile_type ~raise) pattern in
  O.Pattern.map_pattern
    (Location.map (function
        | Linear_pattern.P_var x ->
          O.Pattern.P_var (Binder.set_var x (fresh_name (Binder.get_var x) path))
        | x -> x))
    pattern
