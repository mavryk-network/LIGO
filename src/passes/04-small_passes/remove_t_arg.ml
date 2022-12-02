open Ast_unified
open Passes
let pass_t_arg : expr pass =
  let f : _ ty_expr_ -> ty_expr =
   fun ty ->
    match ty with
    | { location = loc; wrap_content = T_Arg s } ->
      let quote_var var = "'" ^ var in
      t_var ~loc (Ty_variable.of_input_var (quote_var s))
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  let compile _syntax =
    Catamorphism.(cata_expr ~f:{ defaults with ty_expr = f })
  in
  let decompile _ = failwith "wait a little bit" in
  let check_reductions _expr = failwith "wait, use iter?" in
  { name = "pass_remove_t_arg"; compile; decompile; check_reductions }



let%expect_test "addition" =
  printf "%d" (1 + 2);
  [%expect {| 4 |}]

(* let%test_unit "addition" = [%test_result: int] (1 + 2) ~expect:4 *)
