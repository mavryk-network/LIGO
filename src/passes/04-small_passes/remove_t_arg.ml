open Ast_unified
open Passes
let pass_t_arg : expr pass =
  let name = "pass_remove_t_arg" in
  let f_ty_expr : _ ty_expr_ -> ty_expr =
   fun ty ->
    match ty with
    | { location = loc; wrap_content = T_Arg s } ->
      t_var ~loc (Ty_variable.of_input_var s)
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  let compile _syntax = Catamorphism.on_types f_ty_expr in
  let decompile _ = failwith "wait a little bit" in
  let check_reductions expr =
    let _ =
      Catamorphism.on_types
        (function
          | { wrap_content = T_Arg _; _ } -> failwith "This, is bad :( use iter?"
          | { location = loc; wrap_content } -> make_t ~loc wrap_content)
        expr
    in
    true
  in
  { name; compile; decompile; check_reductions }