open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

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


let compile ~raise =
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let same = make_e ~loc e.wrap_content in
    match Location.unwrap e with
    | E_call (f, { wrap_content = [ args ]; location = _ }) ->
      (match get_e f, get_e args with
      | E_variable v, E_array args when Variable.is_name v "list" ->
        array_to_list ~raise ~loc args
      | _ -> same)
    | _ -> same
  in
  `Cata { idle_cata_pass with expr = pass_expr }


let reduction ~raise =
  let fail () = raise.error (wrong_reduction __MODULE__) in
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_call (f, _); _ }
        when Option.value_map ~default:false (get_e_variable f) ~f:(fun x ->
                 Variable.is_name x "list") -> fail ()
      | _ -> ())
  }


let decompile =
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let same = make_e ~loc e.wrap_content in
    let list_var = e_variable ~loc (Variable.of_input_var ~loc "list") in
    match Location.unwrap e with
    | E_constant { cons_name = C_CONS; arguments = [ hd; tl ] } ->
      let args =
        Location.wrap ~loc [ e_array ~loc Array_repr.[ Expr_entry hd; Rest_entry tl ] ]
      in
      e_call ~loc list_var args
    | E_list elements ->
      let args =
        Location.wrap
          ~loc
          [ e_array ~loc (List.map ~f:(fun x -> Array_repr.Expr_entry x) elements) ]
      in
      e_call ~loc list_var args
    | _ -> same
  in
  `Cata { idle_cata_pass with expr = pass_expr }


let pass ~raise ~syntax:_ =
  morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile
    ~reduction_check:(reduction ~raise)


open Unit_test_helpers.Program

let p_test ~raise ~syntax:_ = pass ~raise ~syntax:Syntax_types.JsLIGO

let%expect_test "compile_cons" =
  {|
      ((PE_declaration
         (D_const
           ((pattern (P_var x))
             (let_rhs
               (E_call (E_variable list)
                 ((E_array
                    ((Expr_entry (E_variable hd)) (Rest_entry (E_variable tl)))))))))))
      |}
  |-> p_test ~raise;
  [%expect
    {|
        ((PE_declaration
          (D_const
           ((pattern (P_var x))
            (let_rhs
             (E_constant
              ((cons_name C_CONS) (arguments ((E_variable hd) (E_variable tl))))))))))
        |}]

let%expect_test "compile_list" =
  {|
      ((PE_declaration
         (D_const
           ((pattern (P_var x))
             (let_rhs
               (E_call (E_variable list)
                 ((E_array
                    ((Expr_entry (E_variable a)) (Expr_entry (E_variable b)) (Expr_entry (E_variable c)))))))))))
      |}
  |-> p_test ~raise;
  [%expect
    {|
      ((PE_declaration
        (D_const
         ((pattern (P_var x))
          (let_rhs (E_list ((E_variable a) (E_variable b) (E_variable c))))))))
      |}]

let%expect_test "compile_fail" =
  {|
      ((PE_declaration
         (D_const
           ((pattern (P_var x))
             (let_rhs
               (E_call (E_variable list)
                 ((E_array
                    ((Expr_entry (E_variable hd)) (Rest_entry (E_variable tl1)) (Rest_entry (E_variable tl2)))))))))))
      |}
  |->! pass;
  [%expect
    {|
      Err : (Small_passes_array_rest_not_supported (E_variable tl1))
      |}]

let%expect_test "decompile_cons" =
  {|
      ((PE_declaration
        (D_const
          ((pattern (P_var x))
            (let_rhs
              (E_constant
                ((cons_name C_CONS) (arguments ((E_variable hd) (E_variable tl))))))))))
      |}
  <-| p_test ~raise;
  [%expect
    {|
          ((PE_declaration
            (D_const
             ((pattern (P_var x))
              (let_rhs
               (E_call (E_variable list)
                ((E_array ((Expr_entry (E_variable hd)) (Rest_entry (E_variable tl)))))))))))
    
        |}]

let%expect_test "decompile_list" =
  {|
      ((PE_declaration
         (D_const
           ((pattern (P_var x))
             (let_rhs (E_list ((E_variable a) (E_variable b) (E_variable c))))))))
      |}
  <-| p_test ~raise;
  [%expect
    {|
          ((PE_declaration
            (D_const
             ((pattern (P_var x))
              (let_rhs
               (E_call (E_variable list)
                ((E_array
                  ((Expr_entry (E_variable a)) (Expr_entry (E_variable b))
                   (Expr_entry (E_variable c)))))))))))
    
      |}]
