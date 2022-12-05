open Ast_unified
open Passes

let compile =
  let pass_ty : _ ty_expr_ -> ty_expr = function
    | { location = loc; wrap_content = T_Arg s } ->
      let quote_var var = "'" ^ var in
      t_var ~loc (Ty_variable.of_input_var (quote_var s))
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  { idle_pass with ty_expr = pass_ty }


let reduction_check = Iter.defaults
let decompile = idle_pass
let pass = cata_morph ~name:__MODULE__ ~compile ~decompile ~reduction_check

let%expect_test "addition" =
  let in_expr =
    S_exp.expr_of_sexp
    @@ Sexp.of_string
         {|
    (E_TypeIn
    ((type_binder ((name my_binder))) (rhs (T_Arg f))
      (body (E_Literal Literal_unit))))
  |}
  in
  let out_expr = pass.expression.forward in_expr in
  let toto = S_exp.sexp_of_expr out_expr in
  let _ = Format.printf "%a" (Sexp.pp_hum_indent 2) toto in
  [%expect
    {|
    (E_TypeIn
      ((type_binder ((name my_binder))) (rhs (T_Var ((name 'f))))
        (body (E_Literal Literal_unit)))) |}]