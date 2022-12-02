open Ast_unified
open Passes
let pass_t_arg : expr pass =
  let name = "pass_remove_t_arg" in
  let f : _ ty_expr_ -> ty_expr =
   fun ty ->
    match ty with
    | { location = loc; wrap_content = T_Arg s } ->
      t_var ~loc (Ty_variable.of_input_var s)
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  let compile _syntax =
    Catamorphism.(cata_expr ~f:{ defaults with ty_expr = f })
  in
  let decompile _ = failwith "wait a little bit" in
  let check_reductions expr = failwith "wait, use iter?" in
  { name; compile; decompile; check_reductions }