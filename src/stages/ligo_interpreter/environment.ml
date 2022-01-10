open Types

let extend :
  env -> expression_variable -> ?no_mutation:bool -> (Ast_aggregated.type_expression * value) -> env
  = fun env name ?(no_mutation = false) (ast_type,eval_term) ->
  Expression {name ; item = { ast_type = ast_type ; eval_term } ; no_mutation } :: env

let expressions :
  env -> (expression_variable * (value_expr * bool)) list
  = fun env ->
  List.filter_map env ~f:(function | Expression {name;item;no_mutation} -> Some (name, (item, no_mutation)))

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
