open Ast_unified
open Passes
open Simple_utils.Trace

let name = "remove_t_arg"
let pass_t_arg : expr pass =
  let f : _ ty_expr_ -> ty_expr =
   function
  | { location = loc; wrap_content = T_Arg s } ->
    let quote_var var = "'" ^ var in
    t_var ~loc (Ty_variable.of_input_var (quote_var s))
  | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  let compile _syntax =
    Catamorphism.(cata_expr ~f:{ defaults with ty_expr = f })
  in
  let decompile _ = failwith "wait a little bit" in
  let check_reductions ~raise e =
    let ty_expr : _ ty_expr_ -> unit = function
      | { location = loc; wrap_content = T_Arg s } -> raise.error (Errors.wrong_reduction name"targ not removed")
      | _ -> ()
    in
    Iter.(iter_expr ~f:{defaults with ty_expr} e)
  in
  { name ; compile; decompile; check_reductions }



let%expect_test "addition" =
  printf "%d" (1 + 2);
  [%expect {| 4 |}]

(* let%test_unit "addition" = [%test_result: int] (1 + 2) ~expect:4 *)



(* let () =
  let my_sexp : Sexp.t = Ast_unified.S_exp.sexp_of_expr my_expr in
  Format.printf "Sexp test : %a@." (Sexp.pp_hum_indent 2) my_sexp


let () =
  let my_expr = Ast_unified.S_exp.expr_of_sexp my_sexp in
  let my_sexp : Sexp.t = Ast_unified.S_exp.sexp_of_expr my_expr in
  Format.printf "Sexp test et vice-versa : %a@." (Sexp.pp_hum_indent 2) my_sexp *)