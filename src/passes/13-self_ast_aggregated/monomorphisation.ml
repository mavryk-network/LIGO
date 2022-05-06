module PP_helpers = Simple_utils.PP_helpers
module AST = Ast_aggregated

let fold_map_expression = Helpers.fold_map_expression

let to_name_safe v =
  fst (AST.ValueVar.internal_get_name_and_counter v)
let poly_counter = ref 0
let poly_name v = poly_counter := ! poly_counter + 1 ;
                  AST.ValueVar.of_input_var ("poly_" ^ (to_name_safe v) ^ "_" ^ string_of_int (! poly_counter))

module Longident = struct
  type t = { variable : AST.expression_variable }

  let compare ({ variable } : t) ({ variable = variable' } : t) =
    AST.ValueVar.compare variable variable'

  let pp ppf ({ variable } : t) =
    Format.fprintf ppf "%a" AST.PP.expression_variable variable

  let of_variable variable : t = { variable }

  let to_expression (lid : t) type_ =
    AST.make_e (E_variable lid.variable) type_
end

module Instance = struct
  (* This is a polymorphic instance of the polymorphic function (or value) lid *)
  type t = { vid : Longident.t ; type_instances : AST.type_expression list ; type_ : AST.type_expression }

  let pp ppf { vid ; type_instances ; type_ } =
    Format.fprintf ppf "{ vid = %a ; type_ = %a ; type_instances = [ %a ] }"
      Longident.pp vid AST.PP.type_expression type_ (PP_helpers.list_sep_d AST.PP.type_expression) type_instances
end

module Data = struct
  module LIMap = Simple_utils.Map.Make(struct type t = Longident.t let compare x y = Longident.compare x y end)

  type t = { env : unit ; instances : (Instance.t list) LIMap.t }
  let empty : t = { env = () ; instances = LIMap.empty }

  let pp ppf { instances ; env=_ } =
    let f (lid, instances_of_lid) =
      Format.fprintf ppf "{ lid = %a ~> %a }"
        Longident.pp lid (PP_helpers.list_sep_d Instance.pp) instances_of_lid
    in
    List.iter (LIMap.to_kv_list instances) ~f

  let instances_lookup_and_remove (lid : Longident.t) (data : t) =
    Option.value ~default:[] @@ LIMap.find_opt lid data.instances,
    { data with instances = LIMap.add lid [] data.instances }

  let instance_lookup_opt (lid : Longident.t) (type_instances' : AST.type_expression list) (type_' : AST.type_expression) (data : t) =
    let rec aux = function
      | [] -> None
      | { Instance.vid ; type_instances ; type_ } :: _
           when AST.Helpers.type_expression_eq (type_, type_') &&
                  List.equal (fun t1 t2 -> AST.Helpers.type_expression_eq (t1, t2)) type_instances type_instances' ->
         Some (vid, type_instances)
      | _ :: tl -> aux tl in
    aux @@ Option.value ~default:[] (LIMap.find_opt lid data.instances)

  let instance_add (lid : Longident.t) (instance : Instance.t) (data : t) =
    let lid_instances = instance :: (Option.value ~default:[] @@ LIMap.find_opt lid data.instances) in
    { data with instances = LIMap.add lid lid_instances data.instances }

  let instances_add (lid : Longident.t) (instances : Instance.t list) (data : t) =
    let lid_instances = instances @ (Option.value ~default:[] @@ LIMap.find_opt lid data.instances) in
    { data with instances = LIMap.add lid lid_instances data.instances }
end

(* This is not a proper substitution, it might capture variables: it should be used only with v' a fresh variable *)
let rec subst_var_expr v v' (e : AST.expression) =
  let (), e = fold_map_expression (fun () e ->
                  let (=) = AST.ValueVar.equal in
                  let return expression_content = (true, (), { e with expression_content }) in
                  let return_f expression_content = (false, (), { e with expression_content }) in
                  match e.expression_content with
                  | E_variable w when (w = v) -> return @@ E_variable v'
                  | E_lambda { binder ; result } when (binder.var = v) -> return_f @@ E_lambda { binder ; result }
                  | E_recursive { fun_name ; fun_type ; lambda = { binder ; result } } when (fun_name = v) || (binder.var = v) ->
                     return_f @@ E_recursive { fun_name ; fun_type ; lambda = { binder ; result } }
                  | E_let_in { let_binder ; rhs ; let_result ; attr } when (let_binder.var = v) ->
                     let rhs = subst_var_expr v v' rhs in
                     return_f @@ E_let_in { let_binder ; rhs ; let_result ; attr }
                  | E_matching { matchee ; cases = Match_variant { cases ; tv } } ->
                     let f = function
                         ({ constructor ; pattern ; body } : AST.matching_content_case) when not (pattern = v) ->
                          let body = subst_var_expr v v' body in
                          { AST.constructor ; pattern ; body }
                       | cc -> cc in
                     let matchee = subst_var_expr v v' matchee in
                     let cases = List.map cases ~f in
                     return_f @@ E_matching { matchee ; cases = Match_variant { cases ; tv } }
                  | E_matching { matchee ; cases = Match_record { fields ; body ; tv } }
                       when List.mem (List.map (AST.LMap.to_list fields) ~f:(fun b -> b.var)) v ~equal:(=)  ->
                     let matchee = subst_var_expr v v' matchee in
                     return_f @@ E_matching { matchee ; cases = Match_record { fields ; body ; tv } }
                  | _ -> return e.expression_content
                  ) () e in
  e

let apply_table_expr table (e : AST.expression) =
  let apply_table_type u = List.fold_right table ~f:(fun (v, t) u -> AST.Helpers.subst_type v t u) ~init:u in
  let (), e = fold_map_expression (fun () e ->
                  let e = { e with type_expression = apply_table_type e.type_expression } in
                  let return expression_content = (true, (), { e with expression_content }) in
                  match e.expression_content with
                  | E_type_inst { forall ; type_ } ->
                     return @@ E_type_inst { forall ; type_ = apply_table_type type_ }
                  | E_recursive { fun_name ; fun_type ; lambda } ->
                     let fun_type = apply_table_type fun_type in
                     return @@ E_recursive { fun_name ; fun_type ; lambda }
                  | E_matching { matchee ; cases = Match_variant { cases ; tv } } ->
                     return @@ E_matching { matchee ; cases = Match_variant { cases ; tv = apply_table_type tv } }
                  | E_matching { matchee ; cases = Match_record { fields ; body ; tv } } ->
                     let fields = AST.LMap.map (fun (b : _ AST.binder) -> {b with ascr = Option.map ~f:apply_table_type b.ascr}) fields in
                     return @@ E_matching { matchee ; cases = Match_record { fields ; body ; tv = apply_table_type tv } }
                  | _ -> return e.expression_content) () e in
  e

let rec subst_external_type et t (u : AST.type_expression) =
  let self = subst_external_type in
  match u.type_content with
  | T_arrow {type1;type2} ->
     let type1 = self et t type1 in
     let type2 = self et t type2 in
     { u with type_content = T_arrow {type1;type2} }
  | T_for_all {ty_binder;kind;type_} ->
     let type_ = self et t type_ in
     { u with type_content = T_for_all {ty_binder;kind;type_} }
  | T_constant { injection = External _ ; parameters = _ ; _ } when AST.Helpers.type_expression_eq (et, u) ->
     t
  | T_constant {language;injection;parameters} ->
     let parameters = List.map ~f:(self et t) parameters in
     { u with type_content = T_constant {language;injection;parameters} }
  | T_sum {content; layout} ->
     let content = AST.(LMap.map (fun {associated_type; michelson_annotation; decl_pos} : row_element ->
                       {associated_type = self et t associated_type; michelson_annotation;decl_pos}) content) in
     { u with type_content = T_sum {content; layout} }
  | T_record {content; layout} ->
     let content = AST.(LMap.map (fun {associated_type; michelson_annotation; decl_pos} : row_element ->
                       {associated_type = self et t associated_type; michelson_annotation;decl_pos}) content) in
     { u with type_content = T_record {content; layout} }
  | _ -> u


let subst_external_term et t (e : AST.expression) =
  let (), e = fold_map_expression (fun () e ->
                  let e = { e with type_expression = subst_external_type et t e.type_expression } in
                  let return expression_content = (true, (), { e with expression_content }) in
                  match e.expression_content with
                  | E_type_inst { forall ; type_ } ->
                     return @@ E_type_inst { forall ; type_ = subst_external_type et t type_ }
                  | E_recursive { fun_name ; fun_type ; lambda } ->
                     let fun_type =  subst_external_type et t fun_type in
                     return @@ E_recursive { fun_name ; fun_type ; lambda }
                  | E_matching { matchee ; cases = Match_variant { cases ; tv } } ->
                     return @@ E_matching { matchee ; cases = Match_variant { cases ; tv =  subst_external_type et t tv } }
                  | E_matching { matchee ; cases = Match_record { fields ; body ; tv } } ->
                     let fields = AST.LMap.map (fun binder -> AST.{ binder with ascr = Option.map ~f:(subst_external_type et t) binder.ascr }) fields in
                     return @@ E_matching { matchee ; cases = Match_record { fields ; body ; tv = subst_external_type et t tv } }
                  | _ -> return e.expression_content) () e in
  e

(* A term might have remaining external typer usages around. If the
   type we are monomorphising is of the form
     `t1 -> t2 -> ... -> _ external_type`
   then we will replaced `_ external_type` from the value `u` we get
   from the `typed` argument, which is of the form
     `u1 -> u2 -> ... -> u` *)
let evaluate_external_typer typed rhs =
  let l, external_type = AST.Helpers.destruct_arrows rhs.AST.type_expression in
  match external_type.type_content with
  | T_constant { injection = External _ ; parameters = _ ; _ } ->
     let _, external_type_for = AST.Helpers.destruct_arrows_n typed (List.length l) in
     subst_external_term external_type external_type_for rhs
  | _ -> rhs

let rec mono_polymorphic_expression : Data.t -> AST.expression -> Data.t * AST.expression = fun data expr ->
  let self = mono_polymorphic_expression in
  let return ec = { expr with expression_content = ec } in
  match expr.expression_content with
  | E_variable _ | E_literal _ | E_raw_code _ -> data, expr
  | E_constant { cons_name ; arguments } ->
     let data, arguments = List.fold_right arguments ~init:(data, [])
                             ~f:(fun arg (data, args) -> let data, arg = self data arg in data, arg :: args) in
     data, return (E_constant { cons_name ; arguments })
  | E_application { lamb ; args } ->
     let data, lamb = self data lamb in
     let data, args = self data args in
     data, return (E_application { lamb; args })
  | E_lambda { binder ; result } ->
     let binder_instances, data = Data.instances_lookup_and_remove { variable = binder.var } data in
     let data, result = self data result in
     let _, data = Data.instances_lookup_and_remove { variable = binder.var } data in
     let data = Data.instances_add  { variable = binder.var } binder_instances data in
     data, return (E_lambda { binder ; result })
  | E_type_abstraction { type_binder ; result } ->
    ignore (type_binder,result);
    failwith "not implemented yet"
  | E_recursive { fun_name ; fun_type ; lambda = { binder ; result } } ->
     let data, result = self data result in
     let _, data = Data.instances_lookup_and_remove { variable = binder.var } data in
     let _, data = Data.instances_lookup_and_remove { variable = fun_name } data in
     data, return (E_recursive { fun_name ; fun_type ; lambda = { binder ; result } })
  | E_let_in { let_binder ; rhs ; let_result ; attr } -> (
    match rhs.type_expression.type_content with
    | T_for_all _ ->
       let type_vars, type_ = AST.Helpers.destruct_for_alls rhs.type_expression in
       let build_let (lid : Longident.t) { Instance.vid ; type_instances ; type_ = typed } (let_result, data) =
         let let_binder = vid.variable in
         let table = List.zip_exn type_vars type_instances in
         let rhs = { rhs with type_expression = type_ } in
         let rhs, data = match rhs.expression_content with
           | E_recursive { fun_name ; fun_type = _ ; lambda = { binder ; result } } ->
              let lambda = { AST.binder ; result = subst_var_expr lid.variable vid.variable (subst_var_expr fun_name vid.variable result) } in
              let data = Data.instance_add lid { vid ; type_instances ; type_ = typed } data in
              { rhs with expression_content = E_recursive { fun_name = vid.variable ; fun_type = type_ ; lambda } }, data
           | _ -> rhs, data in
         let rhs = apply_table_expr table rhs in
         let data, rhs = self data rhs in
         let rhs = evaluate_external_typer typed rhs in
         let rhs = { rhs with type_expression = typed } in
         (AST.e_a_let_in {var=let_binder;ascr=Some rhs.type_expression;attributes=Stage_common.Helpers.empty_attribute} rhs let_result attr, data) in
       let data, let_result = self data let_result in
       let instances, data = Data.instances_lookup_and_remove (Longident.of_variable let_binder.var) data in
       let expr, data = List.fold_right instances ~f:(build_let @@ Longident.of_variable let_binder.var) ~init:(let_result, data) in
       data, expr
    | _ ->
       let binder_instances, data = Data.instances_lookup_and_remove (Longident.of_variable let_binder.var) data in
       let data, let_result = self data let_result in
       let _, data = Data.instances_lookup_and_remove (Longident.of_variable let_binder.var) data in
       let data = Data.instances_add (Longident.of_variable let_binder.var) binder_instances data in
       let data, rhs = self data rhs in
       data, return (E_let_in { let_binder ; rhs ; let_result ; attr })
  )
  | E_type_in { type_binder ; rhs ; let_result } ->
     let data, let_result = self data let_result in
     data, return (E_type_in { type_binder ; rhs ; let_result })
  | E_constructor { constructor ; element } ->
     let data, element  = self data element in
     data, return (E_constructor { constructor ; element })
  | E_matching { matchee ; cases } ->
     let data, cases = mono_polymorphic_cases data cases in
     let data, matchee = self data matchee in
     data, return (E_matching { matchee ; cases })
  | E_record lmap ->
     let data, lmap = AST.LMap.fold (fun l expr (data, r) ->
                          let data, v = self data expr in
                          data, AST.LMap.add l v r) lmap (data, AST.LMap.empty) in
     data, return (E_record lmap)
  | E_record_accessor { record ; path } ->
     let data, record = self data record in
     data, return (E_record_accessor { record ; path })
  | E_record_update { record ; path ; update } ->
     let data, update = self data update in
     let data, record = self data record in
     data, return (E_record_update { record ; path ; update })
  | E_type_inst _ ->
     let rec aux type_insts (e : AST.expression) = match e.expression_content with
       | E_type_inst {forall; type_} ->
          aux (type_ :: type_insts) forall
       | E_variable variable -> (
         (List.rev type_insts, Longident.of_variable variable))
       | _ -> failwith "Cannot resolve non-variables with instantiations" in
     let type_instances, lid = aux [] expr in
     let type_ = expr.type_expression in
     let vid, data = match Data.instance_lookup_opt lid type_instances type_ data with
       | Some (vid, _) -> vid, data
       | None ->
          let vid = Longident.of_variable (poly_name lid.variable) in
          vid, Data.instance_add lid { vid ; type_instances ; type_ } data in
     data, Longident.to_expression vid type_
  | E_assign {binder;access_path;expression} ->
      let binder_instances, data = Data.instances_lookup_and_remove (Longident.of_variable binder.var) data in
      let _, data = Data.instances_lookup_and_remove (Longident.of_variable binder.var) data in
      let data = Data.instances_add (Longident.of_variable binder.var) binder_instances data in
      let data, expression = self data expression in
      data, return (E_assign {binder;access_path;expression})

and mono_polymorphic_cases : Data.t -> AST.matching_expr -> Data.t * AST.matching_expr = fun data m ->
  match m with
  | Match_variant { tv ; cases } ->
     let aux { AST.constructor ; pattern ; body } (data, r) =
       let binder_instances, data = Data.instances_lookup_and_remove (Longident.of_variable pattern) data in
       let data, body = mono_polymorphic_expression data body in
       let _, data = Data.instances_lookup_and_remove (Longident.of_variable pattern) data in
       let data = Data.instances_add (Longident.of_variable pattern) binder_instances data in
       data, { AST.constructor ; pattern ; body} :: r in
     let data, cases = List.fold_right cases ~f:aux ~init:(data, []) in
     data, Match_variant { tv ; cases }
  | Match_record { tv ; body ; fields } ->
     let binders = List.map ~f:(fun (b : _ AST.binder) -> b.var) @@ AST.LMap.to_list fields in
     let data, binders_instances = List.fold_right binders ~init:(data, []) ~f:(fun binder (data, binders_instances) ->
                                      let binder_instances, data = Data.instances_lookup_and_remove (Longident.of_variable binder) data in
                                      data, (binder, binder_instances) :: binders_instances) in
     let data, body = mono_polymorphic_expression data body in
     let data = List.fold_right binders ~init:data ~f:(fun binder data ->
                    let _, data = Data.instances_lookup_and_remove (Longident.of_variable binder) data in data) in
     let data = List.fold_right binders_instances ~init:data ~f:(fun (binder, binder_instances) data ->
                    Data.instances_add (Longident.of_variable binder) binder_instances data) in
     data, Match_record { tv ; body ; fields }

let mono_polymorphic_expr e =
  let _, m = mono_polymorphic_expression Data.empty e in
  m
