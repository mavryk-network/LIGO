open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors

(*
 '

if T_App lhs should be a T_Var , else error

*)

let compile =
  let pass_ty : _ ty_expr_ -> ty_expr = fun t ->
    let return_self () = make_t ~loc:t.location t.wrap_content in
    match get_t_app_opt {fp=t} with
    | Some t -> (
        match get_t t.constr with
        | T_Var _ -> return_self ()
        | _ -> failwith "T_App expect a T_Var as lhs"
      )
    | None -> return_self ()
    in
  `Cata { idle_cata_pass with ty_expr = pass_ty }

let reduction ~raise =
  let ty_expr : _ ty_expr_ -> options = fun t ->
    match get_t_app_opt {fp=t} with
    | Some t -> (
        match get_t_var_opt t.constr with
        | Some _ -> ()
        | None -> raise.error (wrong_reduction __MODULE__ "named_fun")
    )
    | None -> ()
  in
  { Iter.defaults with ty_expr }

let decompile = `Cata idle_cata_pass
let pass ~raise = cata_morph ~name:__MODULE__ ~compile ~decompile ~reduction_check:(reduction ~raise)


let%expect_test "compile_single_t_var" =
  let raise = raise_failwith "test" in
  let in_prg =
    S_exp.program_of_sexp
    @@ Sexp.of_string
    {|((
      P_Declaration (D_Type ((name t) (type_expr (
        T_Var toto
      ))))
    ))|}
  in
  let out_expr = (pass ~raise).program.forward in_prg in
  let toto = S_exp.sexp_of_program out_expr in
  let _ = Format.printf "%a" (Sexp.pp_hum_indent 2) toto in
  [%expect
    {|
    ((P_Declaration (D_Type ((name t) (type_expr (T_Var toto)))))) |}]

let%expect_test "compile_t_app_t_var" =
  let raise = raise_failwith "test" in
  let in_prg =
    S_exp.program_of_sexp
    @@ Sexp.of_string
    {|((
      P_Declaration (D_Type ((name t) (type_expr (
        T_App (
          (constr (T_Var my_var))
          (type_args (
            (T_Var arg1)
            (T_Var arg2)
          ))
        )
      ))))
    ))|}
  in
  let out_expr = (pass ~raise).program.forward in_prg in
  let toto = S_exp.sexp_of_program out_expr in
  let _ = Format.printf "%a" (Sexp.pp_hum_indent 2) toto in
  [%expect
    {|
    ((P_Declaration
       (D_Type
         ((name t)
           (type_expr
             (T_App
               ((constr (T_Var my_var)) (type_args ((T_Var arg1) (T_Var arg2)))))))))) |}]

(* TODO : How should we raise exceptions in the [compile] passes ? *)

(* let%expect_test "compile_t_app_wrong" =
  let raise = raise_failwith "test" in
  let in_prg =
    S_exp.program_of_sexp
    @@ Sexp.of_string
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
    ))|}
  in
  let out_expr = (pass ~raise).program.forward in_prg in
  let toto = S_exp.sexp_of_program out_expr in
  let _ = Format.printf "%a" (Sexp.pp_hum_indent 2) toto in
  [%expect
    {|
    ((P_Declaration
       (D_Type
         ((name t)
           (type_expr
             (T_App
               ((constr (T_Var my_var)) (type_args ((T_Var arg1) (T_Var arg2)))))))))) |}] *)
