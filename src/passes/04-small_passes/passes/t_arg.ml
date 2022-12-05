open Ast_unified
open Pass_type

let compile =
  let pass_ty : _ ty_expr_ -> ty_expr = function
    | { location = loc; wrap_content = T_Arg s } ->
      let quote_var var = "'" ^ var in
      t_var ~loc (Ty_variable.of_input_var (quote_var s))
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  `Cata { idle_cata_pass with ty_expr = pass_ty }


let reduction_check = Iter.defaults
let decompile = `Cata idle_cata_pass
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
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Of_sexp_error "lol_of_sexp: polymorphic variant does not take arguments"
    (invalid_sexp ((name my_binder))))
  Raised at Sexplib0__Sexp_conv.of_sexp_error in file "src/sexp_conv.ml", line 156, characters 30-72
  Called from Temp_prim__Type_in.t_of_sexp.(fun).iter__021_ in file "src/stages/2-ast_unified/temp_prim/type_in.ml", line 2, characters 4-15
  Called from Temp_prim__Type_in.t_of_sexp.(fun) in file "src/stages/2-ast_unified/temp_prim/type_in.ml", line 1, characters 0-142
  Called from Ast_unified__Types.expression_content__of_sexp.(fun) in file "src/stages/2-ast_unified/types.ml", line 244, characters 0-1023
  Called from Simple_utils__Location.wrap_of_sexp in file "vendors/ligo-utils/simple-utils/location.ml", line 77, characters 19-39
  Called from Ast_unified__S_exp.expr_of_sexp in file "src/stages/2-ast_unified/s_exp.ml", line 44, characters 11-136
  Called from Passes__T_arg.(fun) in file "src/passes/04-small_passes/passes/t_arg.ml", line 20, characters 4-169
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]