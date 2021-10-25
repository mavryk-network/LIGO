open Types

let extend :
  env -> expression_variable -> ?no_mutation:bool -> (Ast_typed.type_expression * value) -> env
  = fun env name ?(no_mutation = false) (ast_type,eval_term) ->
  Expression {name ; item = { ast_type = ast_type ; eval_term } ; no_mutation } :: env

let extend_mod :
  env -> module_variable -> env -> env
  = fun env name item ->
  Module {name; item} :: env

let expressions :
  env -> (expression_variable * (value_expr * bool)) list
  = fun env ->
  List.filter_map env ~f:(function | Expression {name;item;no_mutation} -> Some (name, (item, no_mutation)) | Module _ -> None)

let modules :
  env -> (module_variable * env) list
  = fun env ->
  List.filter_map env ~f:(function | Module {name;item} -> Some (name, item) | Expression _ -> None)

let lookup :
  env -> expression_variable -> (value_expr * bool) option
  = fun env var ->
  let open Location in
  let equal a b = Var.compare a.wrap_content b.wrap_content = 0 in
  List.Assoc.find (expressions env) ~equal var

let empty_env = []

let to_kv_list v = v
let to_kv_list_rev v = List.rev v

let filter :
  env -> (value_expr -> bool) -> env
    = fun env pred ->
  let rec aux = function
    | [] -> []
    | Expression {name = _; item; no_mutation = _} :: xs when not (pred item) -> aux xs
    | x :: xs -> x :: aux xs in
  aux env

module Free_variables :
  sig
    val value : value -> (module_variable list * expression_variable list)
  end
  = struct

  let get_fv_value : value -> _ = fun v ->
    match v with
    | V_Func_val { orig_lambda ; _ } -> Self_ast_typed.Helpers.Free_variables.expression orig_lambda
    | _ -> ([], [])

  let value v =
    let fmv, fv = get_fv_value v in
    (fmv, fv)
end

module Free_module_variables :
  sig
    val value : value -> (module_variable list * expression_variable list)
    val env : env -> (module_variable list * expression_variable list)
  end
  = struct

  let unions xss =
    let (a, b) = List.fold_right ~init:([], []) ~f:(fun (xs, ys) (rs, ss) -> (xs @ rs, ys @ ss)) xss in
    (List.dedup_and_sort ~compare:compare_module_variable a, List.dedup_and_sort ~compare:(Location.compare_content ~compare:Var.compare) b)

  let rec get_fv_value : value -> _ = fun v ->
    match v with
    | V_Func_val { orig_lambda ; _ } -> Self_ast_typed.Helpers.Free_module_variables.expression orig_lambda
    | _ -> ([], [])

  and get_fv_env : env -> _ = fun env ->
    let aux = fun x ->
      match x with
      | Expression {name = _ ; item ; no_mutation = _} ->
        get_fv_value item.eval_term
      | Module {name=_ ; item} ->
        get_fv_env item
    in
    unions @@ List.map ~f:aux env


  let get_fv_value : value -> _ = fun v ->
    match v with
    | V_Func_val { orig_lambda ; _ } -> Self_ast_typed.Helpers.Free_module_variables.expression orig_lambda
    | _ -> ([], [])

  let value v =
    let fmvs, fvs = get_fv_value v in
    (fmvs, fvs)
  let env e =
    let fmvs, fvs = get_fv_env e in
    (fmvs, fvs)
end
