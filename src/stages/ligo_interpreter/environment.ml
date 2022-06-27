open Types

let extend :
  env -> expression_variable -> ?inline:bool -> ?no_mutation:bool -> (Ast_aggregated.type_expression * value) -> env
  = fun env name ?(inline = false) ?(no_mutation = false) (ast_type,eval_term) ->
  let () = match VHashtbl.find_opt env name with
  | None -> VHashtbl.add env name ({ item = { ast_type = ast_type ; eval_term } ; no_mutation ; inline } :: [])
  | Some xs -> VHashtbl.add env name ({ item = { ast_type = ast_type ; eval_term } ; no_mutation ; inline } :: xs) in
  env
  (* VMap.update name (function
   *     | None -> Some [ { item = { ast_type = ast_type ; eval_term } ; no_mutation ; inline } ]
   *     | Some env -> Some ({ item = { ast_type = ast_type ; eval_term } ; no_mutation ; inline } :: env)) env *)

(* let expressions :
 *   env -> (expression_variable * (value_expr * bool * bool)) list
 *   = fun env -> env *)
  (* print_endline "EXPRESSIONS";
   * List.filter_map env ~f:(function | Expression {name;item;no_mutation;inline} -> Some (name, (item, no_mutation, inline))) *)

let lookup : env -> expression_variable -> (value_expr * bool * bool) option
  = fun env var ->
  match VHashtbl.find_opt env var with
  | None -> None
  | Some [] -> None
  | Some ({item;no_mutation;inline} :: _) -> Some (item, no_mutation, inline)
  (* match VMap.find_opt var env with
   * | None -> None
   * | Some [] -> None
   * | Some ({item;no_mutation;inline} :: _) -> Some (item, no_mutation, inline) *)

let empty_env : env = VHashtbl.create 100 (* VMap.empty *)
