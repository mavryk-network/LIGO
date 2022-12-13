open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(*
  if T_App lhs should be a T_Var or a T_Module_Access , else error
*)

let compile ~raise =
  let pass_ty : _ ty_expr_ -> ty_expr =
   fun t ->
    let return_self () = make_t ~loc:t.location t.wrap_content in
    match Location.unwrap t with
    | T_App { constr; type_args = _ } ->
      if Option.is_some (get_t_var_opt constr)
      then return_self ()
      else raise.error (expected_variable ({ fp = t } : ty_expr))
    | _ -> return_self ()
  in
  `Cata { idle_cata_pass with ty_expr = pass_ty }


let reduction ~raise =
  let open Location in
  let ty_expr : ty_expr ty_expr_ -> options = fun t ->
  match t with
  | { wrap_content = T_App { constr = { fp = { wrap_content = T_Var _; _} }; type_args=_ }; _ } -> ()
  | { wrap_content = T_App { constr = { fp = { wrap_content = _; _} }; type_args=_ }; _ } -> raise.error (wrong_reduction __MODULE__)
  | _ -> ()
  in
  { Iter.defaults with ty_expr }


let decompile = `Cata idle_cata_pass

let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile
    ~reduction_check:(reduction ~raise)

open Unit_test_helpers

let%expect_test "compile_t_app_t_var" =
  {|
  ((
    P_Declaration (D_Type ((name t) (type_expr (
      T_App (
        (constr (T_Var my_var))
        (type_args (
          (T_Var arg1)
          (T_Var arg2)
        ))
      )
    ))))
  ))
  |} |-> pass ~raise ;
  [%expect
    {|
    ((P_Declaration
       (D_Type
         ((name t)
           (type_expr
             (T_App
               ((constr (T_Var my_var)) (type_args ((T_Var arg1) (T_Var arg2)))))))))) |}]

let%expect_test "compile_t_app_wrong" =
  {|((
      P_Declaration (D_Type ((name t) (type_expr (
        T_App (
          (constr (T_Arg should_be_a_t_var))
          (type_args (
            (T_Var arg1)
            (T_Var arg2)
          ))
        )
      ))))
  ))|} |->! pass ;
  [%expect
    {|
    Err : (Small_passes_expected_variable
              (T_App
                  ((constr (T_Arg should_be_a_t_var))
                      (type_args ((T_Var arg1) (T_Var arg2)))))) |}]