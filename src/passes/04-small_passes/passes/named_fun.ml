
open Ast_unified
open Pass_type

let compile =
  (* let pass_ty : _ ty_expr_ -> ty_expr = function
    | { location = loc; wrap_content = T_Named_fun named_arrow } ->
      let quote_var var = "'" ^ var in
      t_var ~loc (Ty_variable.of_input_var (quote_var s))
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in *)
  let pass_ty _ = failwith "lol" in
  { idle_pass with ty_expr = pass_ty }


let reduction_check = Iter.defaults
let decompile = idle_pass
let pass = cata_morph ~name:__MODULE__ ~compile ~decompile ~reduction_check