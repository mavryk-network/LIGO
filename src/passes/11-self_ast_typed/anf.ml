(* open Helpers *)
open Ast_typed
(* open Trace *)

(*
let monad (type_ : type_expression) : type_expression =
  (t_pair type_ (t_int ()))

let ret (expr : expression) : expression =
  (e_a_pair expr (e_a_int (Z.of_int 0)))

let bind (m : expression) (f : expression) =
  let m_type, type_expression = get_t_function_exn f.type_expression in
  let result_type, _ = match get_t_pair type_expression with
    | Some (a, b) -> a, b
    | None -> failwith "foo" in
  let m0 = e_a_record_accessor m (Label "0") m_type in
  let m1 = e_a_record_accessor m (Label "1") (t_int ()) in
  let result = e_a_application f m0 (t_pair result_type (t_int ())) in
  let result0 = e_a_record_accessor result (Label "0") result_type in
  let result1 = e_a_record_accessor result (Label "1") (t_int ()) in
  let sum = e_a_constant C_ADD [m1 ; result1] (t_int ()) in
  let expression_content = e_pair result0 sum in
  make_e expression_content type_expression
*)
let monad (type_ : type_expression) : type_expression =
  type_

let ret (expr : expression) : expression =
  expr

let bind (m : expression) (f : expression) =
  let _, type_expression = get_t_function_exn f.type_expression in
  let expression_content = E_application { lamb = f ; args = m } in
  make_e expression_content type_expression

let rec transform_expression (expr : expression) : expression =
  let self = transform_expression in
  let expr_type = expr.type_expression in
  match expr.expression_content with
  | E_variable _ | E_literal _ ->
     ret expr
  | E_lambda { binder ; result } ->
     let in_type, out_type = get_t_function_exn expr_type in
     let result = self result in
     ret (e_a_lambda { binder ; result } in_type out_type)
  | E_recursive { fun_name ; fun_type ; lambda = { binder ; result } } ->
     let result = self result in
     ret (e_a_recursive { fun_name ; fun_type ; lambda = { binder ; result } })
  | E_let_in { let_binder ; rhs ; let_result } ->
     let rhs_type = rhs.type_expression in
     let rhs = self rhs in
     let let_result = self let_result in
     bind rhs (e_a_lambda { binder = let_binder ;
                            result = let_result } rhs_type (monad expr_type))
  | E_constructor { constructor ; element } ->
     let (Label constructor) = constructor in
     let element_type = element.type_expression in
     let element = self element in
     let element_binder = Location.wrap @@ Var.fresh () in
     bind element (e_a_lambda { binder = element_binder ;
                                result = ret (e_a_constructor constructor (e_a_variable element_binder element_type) expr_type) } element_type (monad expr_type))
  | E_constant { cons_name ; arguments } ->
     let aux (argument, argument_binder) expr =
       let argument_type = argument.type_expression in
       let argument = self argument in
       bind argument (e_a_lambda { binder = argument_binder ;
                                   result = expr } argument_type (monad expr_type)) in
     let arguments = List.map arguments ~f:(fun argument -> (argument, Location.wrap @@ Var.fresh ())) in
     let final = ret (e_a_constant cons_name (List.map arguments ~f:(fun (argument, v) -> e_a_variable v argument.type_expression)) (monad expr_type)) in
     List.fold_right arguments ~f:aux ~init:final
  | E_application { lamb ; args } ->
     let lamb_type = lamb.type_expression in
     let args_type = args.type_expression in
     let lamb = self lamb in
     let args = self args in
     let lamb_binder : expression_variable = Location.wrap @@ Var.fresh () in
     let args_binder : expression_variable = Location.wrap @@ Var.fresh () in
     bind lamb (e_a_lambda { binder = lamb_binder ;
                             result = bind args (e_a_lambda { binder = args_binder ;
                                                              result = e_a_application (e_a_variable lamb_binder lamb_type) (e_a_variable args_binder args_type) (monad expr_type) } args_type (monad expr_type)) } lamb_type (monad expr_type))
  | E_record map ->
     let aux (_, (argument, argument_binder)) expr =
       let argument_type = argument.type_expression in
       let argument = self argument in
       bind argument (e_a_lambda { binder = argument_binder ;
                                   result = expr } argument_type expr.type_expression) in
     let map = LMap.map (fun argument -> (argument, Location.wrap @@ Var.fresh ())) map in
     let layout = (get_t_record_exn expr_type).layout in
     let final = ret (e_a_record ~layout (LMap.map (fun (argument, v) -> e_a_variable v argument.type_expression) map)) in
     let map = LMap.to_kv_list map in
     List.fold_right map ~f:aux ~init:final
  | E_record_accessor { record ; path } ->
     let record_type = record.type_expression in
     let record = self record in
     let record_binder : expression_variable = Location.wrap @@ Var.fresh () in
     bind record (e_a_lambda { binder = record_binder ;
                               result = e_a_record_accessor (e_a_variable record_binder record_type) path (monad expr_type) } record_type (monad expr_type))
  | E_record_update { record ; path ; update } ->
     let record_type = record.type_expression in
     let record = self record in
     let record_binder : expression_variable = Location.wrap @@ Var.fresh () in
     let update_type = update.type_expression in
     let update = self update in
     let update_binder : expression_variable = Location.wrap @@ Var.fresh () in
     bind record (e_a_lambda { binder = record_binder ;
                               result = bind update (e_a_lambda { binder = update_binder ;
                                                                  result = e_a_record_update (e_a_variable record_binder record_type)
                                                                                             path
                                                                                             (e_a_variable update_binder update_type)
                                                                                             (monad expr_type) } update_type (monad expr_type)) }
       record_type (monad expr_type))
  | E_matching { matchee ; cases } ->
     let matchee_type = matchee.type_expression in
     let matchee = self matchee in
     let matchee_binder : expression_variable = Location.wrap @@ Var.fresh () in
     bind matchee (e_a_lambda { binder = matchee_binder ;
                                result = e_a_matching (e_a_variable matchee_binder matchee_type) (transform_cases cases) (monad expr_type) } matchee_type (monad expr_type))
  | E_type_in { type_binder ; rhs ; let_result } ->
     let let_result = self let_result in
     e_a_type_in type_binder rhs let_result (monad expr_type)
  | E_raw_code { language ; code } ->
     e_a_raw_code language code expr_type
  | _ -> expr

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

and transform_module : module_fully_typed -> module_fully_typed = fun (Module_Fully_Typed p) ->
  let aux = fun (x : declaration) ->
    let return (d : declaration) = d in
    match x with
    | Declaration_constant {name; binder; expr ; attr} -> (
        let expr = transform_expression expr in
        return @@ Declaration_constant {name; binder; expr ; attr}
    )
    | Declaration_type t -> return @@ Declaration_type t
    | Declaration_module {module_binder;module_} ->
      let module_ = transform_module module_ in
      return @@ Declaration_module {module_binder; module_}
    | Module_alias _ -> return x
  in
  let p = List.map ~f:(Location.map aux) p in
  Module_Fully_Typed p
