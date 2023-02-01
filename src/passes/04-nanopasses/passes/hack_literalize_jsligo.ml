open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
open Syntax_types

(* 
This pass handles the special cases of type annotation in JsLIGO.
These are the cases where a E_Annot remain a E_Annot

1. The first such case is nat and tez/mutez annotations.

2. The second case is type annotation of code injection.
  
*)

let compile ~raise ~syntax =
  let () = ignore raise in
  (* TODO : Retrict pass to JsLIGO syntax *)
  let pass_expr : (expr, ty_expr, pattern, block, mod_expr) expr_ -> expr =
   fun expr ->
    let loc = Location.get_location expr in
    let expr = Location.unwrap expr in
    let unchanged () = make_e ~loc expr in
    match expr with
    | E_Annot (e, t) ->
      (match get_e e, get_t t with
      (* Conversion of number literals s*)
      | E_Literal (Literal_int i), T_Var tv ->
        if Ty_variable.is_name tv "nat"
        then e_nat_z ~loc i
        else if Ty_variable.is_name tv "tez"
        then (
          let mutez = Z.mul (Z.of_int 1_000_000) i in
          e_mutez_z ~loc mutez)
        else if Ty_variable.is_name tv "mutez"
        then e_mutez_z ~loc i
        else unchanged ()
      (* Type-annotated code injection *)
      | E_RawCode { language; code }, _ ->
        e_rawcode ~loc { language; code = e_annot ~loc (code, t) }
      | _ -> unchanged ())
    | _ -> unchanged ()
  in
  match syntax with
  | JsLIGO -> `Cata { idle_cata_pass with expr = pass_expr }
  | _ -> `Cata idle_cata_pass


let reduction ~raise =
  let expr : _ expr_ -> unit =
   fun e ->
    match Location.unwrap e with
    | E_Annot (e, t) ->
      (match get_e e, get_t t with
      | E_Literal (Literal_int _), T_Var tv ->
        if Ty_variable.is_name tv "nat"
           || Ty_variable.is_name tv "tez"
           || Ty_variable.is_name tv "mutez"
        then raise.error (wrong_reduction __MODULE__)
        else ()
      | _ -> ())
    | _ -> ()
  in
  { Iter.defaults with expr }


let pass ~raise ~syntax =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise ~syntax)
    ~decompile:`None
    ~reduction_check:(reduction ~raise)


open Unit_test_helpers

let%expect_test "number_42_as_nat" =
  {|
  ((PE_Declaration
    (D_Var (
      (pattern (P_var y))
      (let_rhs (
        E_Annot (
          (E_Literal (Literal_int 42))
          (T_Var nat)
        )
      ))
    ))
  ))
  |}
  |-> pass ~raise ~syntax:JsLIGO;
  [%expect
    {|
    ((PE_Declaration
      (D_Var ((pattern (P_var y)) (let_rhs (E_Literal (Literal_nat 42))))))) |}]

let%expect_test "number_42_as_mutez" =
  {|
  ((PE_Declaration
    (D_Var (
      (pattern (P_var y))
      (let_rhs (
        E_Annot (
          (E_Literal (Literal_int 42))
          (T_Var mutez)
        )
      ))
    ))
  ))
  |}
  |-> pass ~raise ~syntax:JsLIGO;
  [%expect
    {|
    ((PE_Declaration
      (D_Var ((pattern (P_var y)) (let_rhs (E_Literal (Literal_mutez 42))))))) |}]

let%expect_test "number_42_as_tez" =
  {|
  ((PE_Declaration
    (D_Var (
      (pattern (P_var y))
      (let_rhs (
        E_Annot (
          (E_Literal (Literal_int 42))
          (T_Var tez)
        )
      ))
    ))
  ))
  |}
  |-> pass ~raise ~syntax:JsLIGO;
  [%expect
    {|
    ((PE_Declaration
      (D_Var
       ((pattern (P_var y)) (let_rhs (E_Literal (Literal_mutez 42000000))))))) |}]

let%expect_test "code_inj" =
  {|
  ((PE_Declaration
    (D_Var (
      (pattern (P_var y))
      (let_rhs
        (E_Annot (
          (E_RawCode (
            (language Michelson)
            (code (E_Literal (Literal_string (Verbatim "{ UNPAIR ; ADD }") )))
          ))
          (T_Fun (
            (T_Prod ((T_Var nat) (T_Var nat)))
            (T_Var nat)
          ))
        ))
      )
    ))
  ))
  |}
  |-> pass ~raise ~syntax:JsLIGO;
  [%expect
    {|
    ((PE_Declaration
      (D_Var
       ((pattern (P_var y))
        (let_rhs
         (E_RawCode
          ((language Michelson)
           (code
            (E_Annot
             ((E_Literal (Literal_string (Verbatim "{ UNPAIR ; ADD }")))
              (T_Fun ((T_Prod ((T_Var nat) (T_Var nat))) (T_Var nat))))))))))))) |}]
