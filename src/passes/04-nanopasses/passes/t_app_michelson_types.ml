(* 
Certain types should translate directly to specific Michelson types
and thus have their own AST node. (e.g. michelson_or, sapling_state).

This pass transforms :
  T_App ( "michelson_or" | "michelson_pair" | "sapling_state" | "sapling_transaction" )
Into a dedicated node :
  T_Michelson_pair | T_Michelson_pair | T_Sapling_state | T_Sapling_transaction

needs  : - t_app_pascaligo -->
*)

open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let t_michelson_or ~loc (l:ty_expr) l_ann (r:ty_expr) r_ann =
  t_sum_raw
    ~loc
    (Non_linear_rows.make [ Label "M_left", Some l, [l_ann]; Label "M_right", Some r, [r_ann] ])


let t_michelson_pair ~loc l l_ann r r_ann =
  t_record_raw ~loc (Non_linear_rows.make [ Label "0", Some l, [l_ann]; Label "1", Some r, [r_ann]])


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
              make_t tv1 (Attribute.make "annot" s1) tv2 (Attribute.make "annot" s2)
            | _ -> raise.error @@ michelson_type_wrong name constr)
          | _ -> raise.error @@ michelson_type_wrong_arity name constr
        in
        if Ty_variable.is_name tv "michelson_or"
        then make_michelson_type "michelson_or" (t_michelson_or ~loc)
        else if Ty_variable.is_name tv "michelson_pair"
        then make_michelson_type "michelson_pair" (t_michelson_pair ~loc)
        (* else if Ty_variable.is_name tv "sapling_state"
        then make_sapling "sapling_state" (t_sapling_state ~loc)
        else if Ty_variable.is_name tv "sapling_transaction"
        then make_sapling "sapling_transaction" (t_sapling_transaction ~loc) *)
        else return_self ()
      | _ -> return_self ())
    | _ -> return_self ()
  in
  `Cata { idle_cata_pass with ty_expr = pass_ty }


let reduction ~raise =
  let () = ignore raise in
  let ty_expr : ty_expr ty_expr_ -> unit =
   fun te_ ->
    match Location.unwrap te_ with
    | T_App { constr; type_args = _ } ->
      (match get_t constr with
      | T_Var tv ->
        if Ty_variable.is_name tv "michelson_or"
           || Ty_variable.is_name tv "michelson_pair"
           (* || Ty_variable.is_name tv "sapling_state"
           || Ty_variable.is_name tv "sapling_transaction" *)
        then raise.error (wrong_reduction __MODULE__)
        else ()
      | _ -> ())
    | _ -> ()
  in
  { Iter.defaults with ty_expr }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None
    ~reduction_check:(reduction ~raise)


open Unit_test_helpers

let%expect_test "compile_michelson_pair" =
  {|
  ((PE_Declaration
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
    ((PE_Declaration
      (D_Type
       ((name t_string)
        (type_expr
         (T_Record_raw
          (((Label 0)
            ((associated_type ((T_Var int)))
             (attributes (((key annot) (value (w))))) (decl_pos 0)))
           ((Label 1)
            ((associated_type ((T_Var nat)))
             (attributes (((key annot) (value (v))))) (decl_pos 1)))))))))) |}]

let%expect_test "compile_michelson_or" =
  {|
  ((PE_Declaration
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
    ((PE_Declaration
      (D_Type
       ((name t_string)
        (type_expr
         (T_Sum_raw
          (((Label M_left)
            ((associated_type ((T_Var int)))
             (attributes (((key annot) (value (w))))) (decl_pos 0)))
           ((Label M_right)
            ((associated_type ((T_Var nat)))
             (attributes (((key annot) (value (v))))) (decl_pos 1)))))))))) |}]

let%expect_test "compile_sapling_state" =
  {|
  ((PE_Declaration
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
    ((PE_Declaration
      (D_Type
       ((name my_sapling)
        (type_expr
         (T_App ((constr (T_Var sapling_state)) (type_args ((T_Int 8 8)))))))))) |}]

let%expect_test "compile_sapling_transaction" =
  {|
  ((PE_Declaration
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
    ((PE_Declaration
      (D_Type
       ((name my_sapling)
        (type_expr
         (T_App
          ((constr (T_Var sapling_transaction)) (type_args ((T_Int 12 12)))))))))) |}]

let%expect_test "compile_michelson_or_wrong_arity" =
  {|
  ((PE_Declaration
     (D_Type
       ((name t_string)
         (type_expr
           (T_App
             ((constr (T_Var michelson_or))
               (type_args
                 ((T_String w) (T_Var nat) (T_String v))))))))))
  |}
  |->! pass;
  [%expect
    {|
    Err : (Small_passes_michelson_type_wrong_arity
              (michelson_or (T_Var michelson_or))) |}]

let%expect_test "compile_michelson_or_type_wrong" =
  {|
  ((PE_Declaration
     (D_Type
       ((name t_string)
         (type_expr
           (T_App
             ((constr (T_Var michelson_or))
               (type_args
                 ((T_Var int) (T_Var tez) (T_Var nat) (T_String v))))))))))
  |}
  |->! pass;
  [%expect
    {| Err : (Small_passes_michelson_type_wrong (michelson_or (T_Var michelson_or))) |}]
