open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* this should be called 'call_syntax' ? *)

let array_to_list ~raise ~loc (arguments : expr Array_repr.t) =
  match arguments with
  | [ Expr_entry hd; Rest_entry tl ] ->
    e_constant ~loc { cons_name = C_CONS; arguments = [ hd; tl ] }
  | _ ->
    let arguments =
      List.map arguments ~f:(function
          | Expr_entry x -> x
          | Rest_entry e -> raise.error (array_rest_not_supported e))
    in
    e_list ~loc arguments


let compile ~raise ~syntax =
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let same = make_e ~loc e.wrap_content in
    match Location.unwrap e with
    | E_Call (f, [ args ]) ->
      (match get_e f, get_e args with
      | E_variable v, E_Array args when Variable.is_name v "list" ->
        array_to_list ~raise ~loc args
      | _ -> same)
    | _ -> same
  in
  if Syntax_types.equal syntax (JsLIGO)
  then `Cata { idle_cata_pass with expr = pass_expr }
  else `Cata idle_cata_pass


let reduction ~raise ~syntax =
  let fail () = raise.error (wrong_reduction __MODULE__) in
  if Syntax_types.equal syntax (JsLIGO)
  then
    { Iter.defaults with
      expr =
        (function
        | { wrap_content = E_Call (f, _); _ }
          when Option.value_map ~default:false (get_e_variable f) ~f:(fun x ->
                   Variable.is_name x "list") -> fail ()
        | _ -> ())
    }
  else Iter.defaults


let decompile ~syntax =
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let same = make_e ~loc e.wrap_content in
    let list_var = e_variable ~loc (Variable.of_input_var ~loc "list") in
    match Location.unwrap e with
    | E_constant { cons_name = C_CONS; arguments = [ hd; tl ] } ->
      e_call ~loc list_var [ e_array ~loc Array_repr.[ Expr_entry hd; Rest_entry tl ] ]
    | E_List elements ->
      e_call
        ~loc
        list_var
        [ e_array ~loc (List.map ~f:(fun x -> Array_repr.Expr_entry x) elements) ]
    | _ -> same
  in
  if Syntax_types.equal syntax JsLIGO
  then `Cata { idle_cata_pass with expr = pass_expr }
  else `Cata idle_cata_pass


let pass ~raise ~syntax =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise ~syntax)
    ~decompile:(decompile ~syntax)
    ~reduction_check:(reduction ~raise ~syntax)


open Unit_test_helpers

let%expect_test "compile_cons" =
  {|
  ((PE_Declaration
     (D_Const
       ((pattern (P_var x))
         (let_rhs
           (E_Call (E_variable list)
             ((E_Array
                ((Expr_entry (E_variable hd)) (Rest_entry (E_variable tl)))))))))))
  |}
  |-> pass ~raise ~syntax:(JsLIGO);
  [%expect
    {|
    ((PE_Declaration
       (D_Const
         ((pattern (P_var x))
           (let_rhs
             (E_constant
               ((cons_name C_CONS) (arguments ((E_variable hd) (E_variable tl))))))))))
    |}]

let%expect_test "compile_list" =
  {|
  ((PE_Declaration
     (D_Const
       ((pattern (P_var x))
         (let_rhs
           (E_Call (E_variable list)
             ((E_Array
                ((Expr_entry (E_variable a)) (Expr_entry (E_variable b)) (Expr_entry (E_variable c)))))))))))
  |}
  |-> pass ~raise ~syntax:(JsLIGO);
  [%expect
    {|
  ((PE_Declaration
     (D_Const
       ((pattern (P_var x))
         (let_rhs (E_List ((E_variable a) (E_variable b) (E_variable c))))))))
  |}]

let%expect_test "compile_fail" =
  {|
  ((PE_Declaration
     (D_Const
       ((pattern (P_var x))
         (let_rhs
           (E_Call (E_variable list)
             ((E_Array
                ((Expr_entry (E_variable hd)) (Rest_entry (E_variable tl1)) (Rest_entry (E_variable tl2)))))))))))
  |}
  |->! pass ~syntax:(JsLIGO);
  [%expect {|
  Err : (Small_passes_array_rest_not_supported (E_variable tl1))
  |}]

let%expect_test "decompile_cons" =
  {|
  ((PE_Declaration
    (D_Const
      ((pattern (P_var x))
        (let_rhs
          (E_constant
            ((cons_name C_CONS) (arguments ((E_variable hd) (E_variable tl))))))))))
  |}
  <-| pass ~raise ~syntax:(JsLIGO);
  [%expect
    {|
      ((PE_Declaration
         (D_Const
           ((pattern (P_var x))
             (let_rhs
               (E_Call (E_variable list)
                 ((E_Array
                    ((Expr_entry (E_variable hd)) (Rest_entry (E_variable tl)))))))))))

    |}]

let%expect_test "decompile_list" =
  {|
  ((PE_Declaration
     (D_Const
       ((pattern (P_var x))
         (let_rhs (E_List ((E_variable a) (E_variable b) (E_variable c))))))))
  |}
  <-| pass ~raise ~syntax:(JsLIGO);
  [%expect
    {|
      ((PE_Declaration
         (D_Const
           ((pattern (P_var x))
             (let_rhs
               (E_Call (E_variable list)
                 ((E_Array
                    ((Expr_entry (E_variable a)) (Expr_entry (E_variable b))
                      (Expr_entry (E_variable c)))))))))))

  |}]