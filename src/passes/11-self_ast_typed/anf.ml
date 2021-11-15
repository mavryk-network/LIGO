module H = Helpers

open Ast_typed

let var_counter = ref 0
let var_name () = var_counter := ! var_counter + 1 ;
                 Location.wrap @@ Var.of_name ("anf_" ^ string_of_int (! var_counter))

let answer_type_cell = ref @@ t_function (t_int ()) (t_int ()) ()

let answer_type () : type_expression =
  ! answer_type_cell

let monad (type_ : type_expression) : type_expression =
  (t_function (t_function type_ (answer_type ()) ()) (answer_type ()) ())

let unmonad (type_ : type_expression) : type_expression =
  let type_, _ = get_t_function_exn type_ in
  let type_, _ = get_t_function_exn type_ in
  type_

let ret (expr : expression) : expression =
  let location = expr.location in
  let k_binder = var_name () in
  let k_type = t_function expr.type_expression (answer_type ()) () in
  (e_a_lambda ~location { binder = k_binder ;
                result = e_a_application ~location (e_a_variable ~location k_binder k_type) expr (answer_type ()) } k_type (answer_type ()))

let bind ?location (m : expression) (f : expression) =
  let location = Option.value location ~default:m.location in
  let a_type, mb_type = get_t_function_exn f.type_expression in
  let b_type = unmonad mb_type in
  let k_binder = var_name () in
  let k_type = t_function b_type (answer_type ()) () in
  let a_binder = var_name () in
  let inner = e_a_lambda ~location { binder = a_binder ;
                           result = e_a_application ~location (e_a_application ~location f (e_a_variable a_binder a_type) mb_type) (e_a_variable ~location k_binder k_type) (answer_type ()) } a_type (answer_type ()) in
  (e_a_lambda ~location { binder = k_binder ;
                result = e_a_application ~location m inner (answer_type ()) }
     k_type (answer_type ()))

let rec transform_expression (expr : expression) : expression =
  let self = transform_expression in
  let expr_type = expr.type_expression in
  let location = expr.location in
  match expr.expression_content with
  | E_type_inst { forall ; type_ = _ } ->
     ret forall
  | E_variable _ | E_literal _ ->
     ret expr
  | E_lambda { binder ; result } ->
     let in_type, out_type = get_t_function_exn expr_type in
     let result = self result in
     ret (e_a_lambda ~location { binder ; result } in_type out_type)
  | E_recursive { fun_name ; fun_type ; lambda = { binder ; result } } ->
     let result = self result in
     ret (e_a_recursive ~location { fun_name ; fun_type ; lambda = { binder ; result } })
  | E_let_in { let_binder ; rhs ; let_result } ->
     let rhs_type = rhs.type_expression in
     let rhs = self rhs in
     let let_result = self let_result in
     bind ~location rhs (e_a_lambda ~location { binder = let_binder ;
                                                result = let_result } rhs_type (monad expr_type))
  | E_constructor { constructor ; element } ->
     let (Label constructor) = constructor in
     let element_type = element.type_expression in
     let element = self element in
     let element_binder = var_name () in
     bind element (e_a_lambda ~location { binder = element_binder ;
                                result = ret (e_a_constructor constructor (e_a_variable element_binder element_type) expr_type) } element_type (monad expr_type))
  | E_constant { cons_name ; arguments } ->
     let aux (argument, argument_binder) expr =
       let argument_type = argument.type_expression in
       let argument = self argument in
       bind argument (e_a_lambda { binder = argument_binder ;
                                   result = expr } argument_type (monad expr_type)) in
     let arguments = List.map arguments ~f:(fun argument -> (argument, var_name ())) in
     let final = ret (e_a_constant cons_name (List.map arguments ~f:(fun (argument, v) -> e_a_variable v argument.type_expression)) (expr_type)) in
     List.fold_right arguments ~f:aux ~init:final
  | E_application { lamb ; args } ->
     let lamb_type = lamb.type_expression in
     let args_type = args.type_expression in
     let lamb = self lamb in
     let args = self args in
     let lamb_binder : expression_variable = var_name () in
     let args_binder : expression_variable = var_name () in
     bind lamb (e_a_lambda ~location { binder = lamb_binder ;
                             result = bind args (e_a_lambda { binder = args_binder ;
                                                              result = e_a_application ~location (e_a_variable ~location lamb_binder lamb_type) (e_a_variable ~location args_binder args_type) (monad expr_type) } args_type (monad expr_type)) } lamb_type (monad expr_type))
  | E_record map ->
     let aux (_, (argument, argument_binder)) expr =
       let argument_type = argument.type_expression in
       let argument = self argument in
       bind argument (e_a_lambda { binder = argument_binder ;
                                   result = expr } argument_type expr.type_expression) in
     let map = LMap.map (fun argument -> (argument, var_name ())) map in
     let layout = (get_t_record_exn expr_type).layout in
     let final = ret (e_a_record ~layout (LMap.map (fun (argument, v) -> e_a_variable v argument.type_expression) map)) in
     let map = LMap.to_kv_list map in
     List.fold_right map ~f:aux ~init:final
  | E_record_accessor { record ; path } ->
     let record_type = record.type_expression in
     let record = self record in
     let record_binder : expression_variable = var_name () in
     bind record (e_a_lambda { binder = record_binder ;
                               result = e_a_record_accessor (e_a_variable record_binder record_type) path (monad expr_type) } record_type (monad expr_type))
  | E_record_update { record ; path ; update } ->
     let record_type = record.type_expression in
     let record = self record in
     let record_binder : expression_variable = var_name () in
     let update_type = update.type_expression in
     let update = self update in
     let update_binder : expression_variable = var_name () in
     bind record (e_a_lambda ~location { binder = record_binder ;
                               result = bind update (e_a_lambda ~location { binder = update_binder ;
                                                                  result = e_a_record_update (e_a_variable record_binder record_type)
                                                                                             path
                                                                                             (e_a_variable update_binder update_type)
                                                                                             (monad expr_type) } update_type (monad expr_type)) }
       record_type (monad expr_type))
  | E_matching { matchee ; cases } ->
     let matchee_type = matchee.type_expression in
     let matchee = self matchee in
     let matchee_binder : expression_variable = var_name () in
     bind matchee (e_a_lambda { binder = matchee_binder ;
                                result = e_a_matching (e_a_variable matchee_binder matchee_type) (transform_cases cases) (monad expr_type) } matchee_type (monad expr_type))
  | E_type_in { type_binder ; rhs ; let_result } ->
     let let_result = self let_result in
     e_a_type_in type_binder rhs let_result (monad expr_type)
  | E_raw_code { language ; code } ->
     e_a_raw_code language code expr_type
  | E_module_accessor { module_name ; element } ->
     ret (e_a_module_accessor module_name element (monad expr_type))
  | E_mod_in { module_binder ; rhs ; let_result } ->
     let let_result = self let_result in
     e_a_mod_in module_binder rhs let_result
  | E_mod_alias { alias ; binders ; result } ->
     let result = self result in
     e_a_mod_alias alias binders result

and transform_cases cases =
  match cases with
  | Match_variant { cases ; tv } ->
     let f { constructor ; body ; pattern } =
       let body = transform_expression body in
       { constructor ; body ; pattern } in
     let cases = List.map cases ~f in
     Match_variant { cases ; tv }
  | Match_record { body ; fields ; tv } ->
     let body = transform_expression body in
     Match_record { body ; fields ; tv }

let subst_expr (v : expression_variable) (u : expression) (t : expression) : expression =
  H.map_expression (fun expr ->
      match expr.expression_content with
      | E_variable v' when equal_expression_variable v v' -> u
      | _ -> expr) t

let fix_app (fun_name : expression_variable)(t : expression) : expression =
  let (), expr = H.fold_map_expression (fun () expr ->
      match expr.expression_content with
      | E_lambda { binder ; _ } when equal_expression_variable binder fun_name ->
         (false, (), expr)
      | E_recursive { fun_name = fun_name' ; lambda = { binder ; _ } ; _ } when equal_expression_variable binder fun_name || equal_expression_variable fun_name' fun_name ->
         (false, (), expr)
      | E_application { lamb = { expression_content = E_application { lamb = { expression_content = E_variable _v ; type_expression }
                                                                      ; args = x0 } } ;
                        args = x1 } when equal_expression_variable _v fun_name ->
         (true, (), e_a_application ~location:expr.location (e_a_variable fun_name type_expression) (e_a_pair x0 x1) expr.type_expression)
      | _ -> (true, (), expr)) () t in
  expr

let rec reduce (expr : expression) : expression =
  let b, expr = H.fold_map_expression (fun b expr ->
  match expr.expression_content with
  | E_application { lamb = { expression_content = E_lambda { binder ; result } } ; args } ->
     (true, true, subst_expr binder args result)
  | E_application { lamb = { expression_content = E_matching { matchee ; cases = Match_variant { cases ; tv } } } ; args } ->
     let cases = List.map ~f:(fun { constructor ; pattern ; body } ->
                     let body = e_a_application body args expr.type_expression in
                     { constructor ; pattern ; body }) cases in
     (true, true, { expr with expression_content = E_matching { matchee ; cases = Match_variant { cases ; tv } } })
  | E_application { lamb = { expression_content = E_matching { matchee ; cases = Match_record { fields ; body ; tv } } } ; args } ->
     let body = e_a_application body args expr.type_expression in
     (true, true, { expr with expression_content = E_matching { matchee ; cases = Match_record { fields ; body ; tv } } })
  | _ -> (true, b, expr)) false expr in
  if b then reduce expr else expr

let  rec transform_recursive (expr : expression) : expression =
  let expr_type = expr.type_expression in
  let (), expr = H.fold_map_expression (fun () expr ->
  let location = expr.location in
  match expr.expression_content with
  | E_recursive { fun_name ; fun_type ; lambda = { binder ; result } } ->
     let in_type, out_type = get_t_function_exn fun_type in
     answer_type_cell := out_type;
     let result = transform_expression result in
     let result = reduce result in
     let result = fix_app fun_name result in
     let k_binder, result, pre_type, post_type = match result.expression_content, result.type_expression.type_content with
       | E_lambda { binder ; result }, T_arrow { type1 = pre_type; type2 = post_type } -> binder, result, pre_type, post_type
       | _ -> failwith "cont expected" in
     let pair_binder = var_name () in
     let fields = LMap.of_list [(Label "0", (binder, in_type)); (Label "1", (k_binder, pre_type))] in
     let result = e_a_matching (e_a_variable ~location pair_binder (t_pair in_type pre_type))
       (Match_record { fields ; body = result ; tv = t_pair in_type pre_type } ) out_type in
     let fun_type = t_function (t_pair in_type pre_type) post_type () in
     let transform = e_a_recursive ~location { fun_name ; fun_type ; lambda = { binder = pair_binder ; result } } in
     let out_binder = var_name () in
     let id_binder = var_name () in
     let id_expr = e_a_lambda ~location { binder = id_binder ; result = e_a_variable ~location id_binder out_type } out_type out_type in
     let transform = e_a_lambda ~location { binder = out_binder ; result = e_a_let_in fun_name transform
                                                                   (e_a_application ~location (e_a_variable ~location fun_name fun_type)
                       (e_a_pair (e_a_variable ~location out_binder in_type) id_expr) out_type) { inline = false ; no_mutation = false ; public = true ; view = false } }
                       in_type out_type in
     let transform = reduce transform in
     let transform = fix_app fun_name transform in
     let transform = { transform with type_expression = expr_type ; location } in
     (false, (), transform)
  | _ -> (true, (), expr)) () expr in
  expr

and transform_module : module_fully_typed -> module_fully_typed = fun (Module_Fully_Typed p) ->
  let aux = fun (x : declaration) ->
    let return (d : declaration) = d in
    match x with
    | Declaration_constant {name; binder; expr ; attr} -> (
        let expr = transform_recursive expr in
        return @@ Declaration_constant {name; binder; expr ; attr}
    )
    | Declaration_type t -> return @@ Declaration_type t
    | Declaration_module {module_binder;module_} ->
      let module_ = transform_module module_ in
      return @@ Declaration_module {module_binder; module_ ; module_attr = { public = true } }
    | Module_alias _ -> return x
  in
  let p = List.map ~f:(Location.map aux) p in
  Module_Fully_Typed p
