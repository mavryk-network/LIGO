open Types

let extend :
  env -> expression_variable -> ?no_mutation:bool -> (Ast_typed.type_expression * value) -> env
  = fun env name ?(no_mutation = false) (ast_type,eval_term) ->
  Expression {name ; item = { ast_type = ast_type ; eval_term } ; no_mutation } :: env

let extend_mod :
  env -> module_variable -> env -> env
  = fun env name item ->
  Module {name; item} :: env

let extend_mod_alias :
  env -> module_variable -> module_variable List.Ne.t -> env
  = fun env name binders ->
  Module_rename {name; binders} :: env

let expressions :
  env -> (expression_variable * (value_expr * bool)) list
  = fun env ->
  List.filter_map env ~f:(function | Expression {name;item;no_mutation} -> Some (name, (item, no_mutation)) | _ -> None )

let modules :
  env -> (module_variable * env) list
  = fun env ->
  List.filter_map env ~f:(function | Module {name;item} -> Some (name, item) | _ -> None)

let lookup :
  env -> expression_variable -> (value_expr * bool) option
  = fun env var ->
  let open Location in
  let equal a b = Var.compare a.wrap_content b.wrap_content = 0 in
  List.Assoc.find (expressions env) ~equal var

let rec lookup_mod = fun env module_name ->
  let rec find_mod module_name = function
      [] -> None
    | Module_rename { name ; binders } :: tl when String.equal module_name name ->
       Some (lookup_binders binders tl)
    | Module { name ; item } :: _ when String.equal module_name name -> Some item
    | _ :: tl -> find_mod module_name tl in
  find_mod module_name env
and lookup_binders binders env =
  (* print_endline (Format.asprintf "looking: %a" (PP_helpers.list_sep_d PP_helpers.string) (List.Ne.to_list binders));
   * print_endline (Format.asprintf "env: %a" PP.pp_env env); *)
  let aux (e : env) (m : module_variable) =
    match lookup_mod e m with
    | None -> failwith "foo"
    | Some env -> env in
  List.Ne.fold_left aux env binders

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

