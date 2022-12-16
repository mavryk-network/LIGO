(* 
The reverse application operator '|>' is syntactic sugar for function application.
For example, this :
   let res = x \> f \> g |> h
Is equivalent to 
  let res = h (g (f x))
This pass unsugars the E_RevApp operator into normal function application E_Application
*)

open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let compile ~raise =
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = e.location in
    let return_self () = make_e ~loc e.wrap_content in
    let () = ignore raise in
    match Location.unwrap e with
    | E_RevApp { x; f } -> e_call ~loc f [ x ]
    | _ -> return_self ()
  in
  `Cata { idle_cata_pass with expr = pass_expr }


let reduction ~raise =
  let () = ignore raise in
  let expr : _ expr_ -> unit =
   fun e ->
    match Location.unwrap e with
    | E_RevApp _ -> raise.error (wrong_reduction __MODULE__)
    | _ -> ()
  in
  { Iter.defaults with expr }


let decompile = `Cata idle_cata_pass

let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile
    ~reduction_check:(reduction ~raise)


open Unit_test_helpers

let%expect_test "compile_rev_app_simple" =
  {|
  ((P_Declaration
    (D_Let
      ((is_rec false) (type_params ()) (pattern ((P_var res)))
        (rhs_type ())
        (let_rhs (E_RevApp ((x (E_variable x)) (f (E_variable f)))))))))
  |}
  |-> pass ~raise;
  [%expect
    {|
    ((P_Declaration
       (D_Let
         ((is_rec false) (type_params ()) (pattern ((P_var res))) (rhs_type ())
           (let_rhs (E_Call (E_variable f) ((E_variable x)))))))) |}]

let%expect_test "compile_rev_app_successive" =
  {|
  ((P_Declaration
    (D_Let
      ((is_rec false) (type_params ()) (pattern ((P_var res)))
        (rhs_type ())
        (let_rhs
          (E_RevApp
            ((x
               (E_RevApp
                 ((x
                    (E_RevApp
                      ((x (E_variable x)) (f (E_variable f)))))
                   (f (E_variable g)))))
              (f (E_variable h)))))))))
  |}
  |-> pass ~raise;
  [%expect
    {|
    ((P_Declaration
       (D_Let
         ((is_rec false) (type_params ()) (pattern ((P_var res))) (rhs_type ())
           (let_rhs
             (E_Call (E_variable h)
               ((E_Call (E_variable g)
                  ((E_Call (E_variable f) ((E_variable x)))))))))))) |}]

let%expect_test "compile_rev_app_precedence" =
  {|
  ((P_Declaration
    (D_Let
      ((is_rec false) (type_params ()) (pattern ((P_var res)))
        (rhs_type ())
        (let_rhs
          (E_RevApp
            ((x
               (E_constant
                 ((cons_name C_POLYMORPHIC_ADD)
                   (arguments
                     ((E_variable x) (E_Literal (Literal_int 1)))))))
              (f (E_Call (E_variable f) ((E_variable y)))))))))))
   |}
  |-> pass ~raise;
  [%expect
    {|
    ((P_Declaration
       (D_Let
         ((is_rec false) (type_params ()) (pattern ((P_var res))) (rhs_type ())
           (let_rhs
             (E_Call (E_Call (E_variable f) ((E_variable y)))
               ((E_constant
                  ((cons_name C_POLYMORPHIC_ADD)
                    (arguments ((E_variable x) (E_Literal (Literal_int 1))))))))))))) |}]
