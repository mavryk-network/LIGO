(* 
Certain types should translate directly to specific Michelson types
and thus have their own AST node. (e.g. michelson_or, sapling_state).

This pass transforms :
  T_App ( "michelson_or" | "michelson_pair" | "sapling_state" | "sapling_transaction" )
Into a dedicated node :
  T_Michelson_pair | T_Michelson_pair | T_Sapling_state | T_Sapling_transaction

needs  : - t_app_pascaligo -->
*)

(* let open Location in
  let ty_expr : ty_expr ty_expr_ -> options = fun t ->
  match t with
  | { wrap_content = T_App { constr = { fp = { wrap_content = T_Var _; _} }; type_args=_ }; _ } -> ()
  | { wrap_content = T_App { constr = { fp = { wrap_content = _; _} }; type_args=_ }; _ } -> raise.error (wrong_reduction __MODULE__)
  | _ -> ()
  in
  { Iter.defaults with ty_expr } *)

open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let compile ~raise =
  let pass_ty : ty_expr ty_expr_ -> ty_expr =
   fun t ->
    let loc = t.location in
    let return_self () = make_t ~loc t.wrap_content in
    let () = ignore raise in
    match Location.unwrap t with
    | T_App { constr; type_args } ->
      (match get_t constr with
      | T_Var tv ->
        let make_michelson_type name make_t =
          match type_args with
          | t1, [ t2; t3; t4 ] ->
            (match get_t t1, get_t t2, get_t t3, get_t t4 with
            | T_Var v1, T_String s1, T_Var v2, T_String s2 ->
              let tv1 = t_var ~loc:(Ty_variable.get_location v1) v1 in
              let tv2 = t_var ~loc:(Ty_variable.get_location v2) v2 in
              make_t tv1 s1 tv2 s2
            | _ -> raise.error @@ michelson_type_wrong name loc)
          | _ -> raise.error @@ michelson_type_wrong_arity name loc
        in
        let make_sapling name make_t =
          match type_args with
          | t1, [] ->
            (match get_t t1 with
            | T_Int (s, z) -> make_t s z
            | _ -> raise.error @@ michelson_type_wrong name loc)
          | _ -> raise.error @@ michelson_type_wrong_arity name loc
        in
        if Ty_variable.is_name tv "michelson_or"
        then make_michelson_type "michelson_or" (t_michelson_or ~loc)
        else if Ty_variable.is_name tv "michelson_pair"
        then make_michelson_type "michelson_pair" (t_michelson_pair ~loc)
        else if Ty_variable.is_name tv "sapling_state"
        then make_sapling "sapling_state" (t_sapling_state ~loc)
        else if Ty_variable.is_name tv "sapling_transaction"
        then make_sapling "sapling_transaction" (t_sapling_transaction ~loc)
        else return_self ()
      | _ -> return_self ())
    | _ -> return_self ()
  in
  `Cata { idle_cata_pass with ty_expr = pass_ty }


let reduction ~raise =
  let () = ignore raise in
  Iter.defaults


let decompile = `Cata idle_cata_pass

let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile
    ~reduction_check:(reduction ~raise)


open Unit_test_helpers

let%expect_test "compile_michelson_pair" =
  {|
  ((P_Declaration
    (D_Type
      ((name t_string)
        (type_expr
          (T_App
            ((constr (T_Var michelson_pair))
              (type_args
                ((T_Var int) (T_String w) (T_Var nat) (T_String v))))))))))
  |}
  |-> pass ~raise;
  [%expect
    {|
    ((P_Declaration
       (D_Type
         ((name t_string)
           (type_expr (T_Michelson_pair (T_Var int) w (T_Var nat) v)))))) |}]

let%expect_test "compile_michelson_or" =
  {|
  ((P_Declaration
     (D_Type
       ((name t_string)
         (type_expr
           (T_App
             ((constr (T_Var michelson_or))
               (type_args
                 ((T_Var int) (T_String w) (T_Var nat) (T_String v))))))))))
  |}
  |-> pass ~raise;
  [%expect
    {|
    ((P_Declaration
       (D_Type
         ((name t_string)
           (type_expr (T_Michelson_or (T_Var int) w (T_Var nat) v)))))) |}]

let%expect_test "compile_sapling_state" =
  {|
  ((P_Declaration
  (D_Type
    ((name my_sapling)
      (type_expr
        (T_App
          ((constr (T_Var sapling_state))
            (type_args ((T_Int 8 8))))))))))
  |}
  |-> pass ~raise;
  [%expect
    {|
    ((P_Declaration
       (D_Type ((name my_sapling) (type_expr (T_Sapling_state 8 8)))))) |}]

let%expect_test "compile_sapling_transaction" =
  {|
  ((P_Declaration
     (D_Type
       ((name my_sapling)
         (type_expr
           (T_App
             ((constr (T_Var sapling_transaction))
               (type_args ((T_Int 12 12))))))))))
  |}
  |-> pass ~raise;
  [%expect
    {|
    ((P_Declaration
       (D_Type ((name my_sapling) (type_expr (T_Sapling_transaction 12 12)))))) |}]

let%expect_test "compile_michelson_or_wrong_arity" =
  {|
  ((P_Declaration
     (D_Type
       ((name t_string)
         (type_expr
           (T_App
             ((constr (T_Var michelson_or))
               (type_args
                 ((T_String w) (T_Var nat) (T_String v))))))))))
  |}
  |->! pass;
  [%expect {| Err : (Small_passes_michelson_type_wrong_arity (michelson_or "")) |}]

let%expect_test "compile_michelson_or_type_wrong" =
  {|
  ((P_Declaration
     (D_Type
       ((name t_string)
         (type_expr
           (T_App
             ((constr (T_Var michelson_or))
               (type_args
                 ((T_Var int) (T_Var tez) (T_Var nat) (T_String v))))))))))
  |}
  |->! pass;
  [%expect {| Err : (Small_passes_michelson_type_wrong (michelson_or "")) |}]
