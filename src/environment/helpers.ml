open Ast_typed

let wrap_var s = Location.wrap @@ Var.of_name s

let add_bindings_in_env bs env =
  List.fold_left bs ~init:env ~f:(fun env (v,e) -> 
    let attr = { inline = false ; no_mutation = false; public = true; view = false } in
    Ast_typed.Environment.add_ez_declaration ~public:true (wrap_var v) e attr env)

let add_types_in_module_env ts env = 
  List.fold_left ts ~init:env ~f:(fun env (v,t) -> 
    Ast_typed.Environment.add_type ~public:true v t env)

let make_module parent_env module_name bindings = 
  let module_env = add_bindings_in_env bindings parent_env in
  Ast_typed.Environment.add_module ~public:true module_name module_env parent_env 
