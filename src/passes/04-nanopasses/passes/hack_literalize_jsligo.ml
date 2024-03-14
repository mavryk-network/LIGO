open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors

(* 
This pass handles the special cases of type annotation in JsLIGO.
These are the cases where a E_annot remain a E_annot

1. The first such case is nat and tez/mumav annotations.

2. The second case is type annotation of code injection.
  
*)
include Flag.No_arg ()

let compile ~raise:_ =
  (* TODO : Retrict pass to JsLIGO syntax *)
  let pass_expr : (expr, ty_expr, pattern, block, mod_expr) expr_ -> expr =
   fun expr ->
    let loc = Location.get_location expr in
    let expr = Location.unwrap expr in
    let unchanged () = make_e ~loc expr in
    match expr with
    | E_annot (e, t) ->
      (match get_e e, get_t t with
      (* Conversion of number literals s*)
      | E_literal (Literal_int i), T_var tv ->
        if Ty_variable.is_name tv "nat"
        then e_nat_z ~loc i
        else if Ty_variable.is_name tv "tez"
        then (
          let mumav = Z.mul (Z.of_int 1_000_000) i in
          e_mumav_z ~loc mumav)
        else if Ty_variable.is_name tv "mumav"
        then e_mumav_z ~loc i
        else unchanged ()
      (* Type-annotated code injection *)
      | E_raw_code { language; code }, _ ->
        e_raw_code ~loc { language; code = e_annot ~loc (code, t) }
      | _ -> unchanged ())
    | _ -> unchanged ()
  in
  Fold { idle_fold with expr = pass_expr }


let reduction ~raise =
  let expr : _ expr_ -> unit =
   fun e ->
    match Location.unwrap e with
    | E_annot (e, t) ->
      (match get_e e, get_t t with
      | E_literal (Literal_int _), T_var tv ->
        if Ty_variable.is_name tv "nat"
           || Ty_variable.is_name tv "tez"
           || Ty_variable.is_name tv "mumav"
        then raise.error (wrong_reduction __MODULE__)
        else ()
      | _ -> ())
    | _ -> ()
  in
  { Iter.defaults with expr }


let name = __MODULE__
let decompile ~raise:_ = Nothing

open Unit_test_helpers.Expr

let%expect_test "number_42_as_nat" =
  {| (E_annot ((E_literal (Literal_int 42)) (T_var nat))) |} |-> compile;
  [%expect {| (E_literal (Literal_nat 42)) |}]

let%expect_test "number_42_as_mumav" =
  {| (E_annot ((E_literal (Literal_int 42)) (T_var mumav))) |} |-> compile;
  [%expect {| (E_literal (Literal_mumav 42))  |}]

let%expect_test "number_42_as_tez" =
  {| ( E_annot ((E_literal (Literal_int 42)) (T_var tez))) |} |-> compile;
  [%expect {|(E_literal (Literal_mumav 42000000)) |}]

let%expect_test "code_inj" =
  {|
    (E_annot
      ((E_raw_code ((language Michelson)
        (code (E_literal (Literal_string (Verbatim "{ UNPAIR ; ADD }") )))))
        (TY_EXPR)))
      |}
  |-> compile;
  [%expect
    {|
      (E_raw_code
       ((language Michelson)
        (code
         (E_annot
          ((E_literal (Literal_string (Verbatim "{ UNPAIR ; ADD }"))) (TY_EXPR))))))
    |}]
