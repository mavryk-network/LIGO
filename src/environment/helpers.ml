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
  let module_env = add_bindings_in_env bindings Ast_typed.Environment.empty in
  Ast_typed.Environment.add_module ~public:true module_name module_env parent_env 

let e_a_raw_code = Ast_typed.Combinators.e_a_raw_michelson_code

let new_var t = 
  let x = Location.wrap @@ Var.fresh () in
  let x_var = e_a_variable x t in
  (x,x_var)
let curryfy2 = fun f t (tx,ty) -> 
  let x,x_var = new_var tx in
  let y,y_var = new_var ty in
  e_a_lambda x 
    (e_a_lambda y
      (e_a_application
        (f @@ t_function (t_pair tx ty) t ()) (e_a_pair x_var y_var)
        t
      ) ty t
    ) tx (t_function ty t ())

(* 
  curry3: fun f => fun a -> fun b -> fun c -> f ((a,b),c) 
  curry2: fun f => fun a -> fun b -> f (a,b)

  fun f -> fun a -> fun b ->
    curry2 (curry2 f)

let curryfy3 = fun f t (tx, ty, tz) ->
  let f = fun _a -> curryfy2 (f) t (t_pair tx ty, tz) in
  curryfy2 f (t_function tz t ()) (tx,ty)
*)

let curryfy3 = fun f t (tx,ty,tz) -> 
  let x = Location.wrap @@ Var.fresh () in
  let y = Location.wrap @@ Var.fresh () in
  let z = Location.wrap @@ Var.fresh () in
  let x_var = e_a_variable x tx in  
  let y_var = e_a_variable y ty in
  let z_var = e_a_variable z tz in
  e_a_lambda x 
    (e_a_lambda y
      (e_a_lambda z
        (e_a_application
          (f @@ t_function (t_pair (t_pair tx ty) tz) t ()) 
          (e_a_pair (e_a_pair x_var y_var) z_var)
          t
        ) tz t
      ) ty (t_function tz t ())
    ) tx (t_function ty (t_function tz t ()) ())