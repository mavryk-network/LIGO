open Simple_utils.Trace
open Ast_unified
open Pass_type
open Errors

(*
in Jsligo, parameters in arrow types have name, we simply drop it
`(foo:int) => (bar:string) => nat` |-> `int -> string -> nat`

note: we could remember them somehow in the future
*)
let compile =
  let pass_ty : _ ty_expr_ -> ty_expr = function
    | { location = loc; wrap_content = T_Named_fun (params, ret) } ->
      let params = List.map ~f:(fun { type_expr; name = _ } -> type_expr) params in
      List.fold_right ~f:(fun x acc -> t_fun ~loc (x, acc)) ~init:ret params
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  `Cata { idle_cata_pass with ty_expr = pass_ty }


let reduction ~raise =
  { Iter.defaults with
    ty_expr =
      (function
      | { wrap_content = T_Named_fun _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }

let pass ~raise =
  cata_morph ~name:__MODULE__ ~compile ~decompile:`None ~reduction_check:(reduction ~raise)


open Unit_test_helpers

let%expect_test "compile" =
  {|
    ((PE_Declaration
      (D_Type
        ( (name t)
          (type_expr
            (T_Named_fun
              ((((name foo) (type_expr (T_Var int))) ((name bar) (type_expr (T_Var string))))
                (T_Var nat))))))))
  |}
  |-> pass ~raise;
  [%expect
    {|
    ((PE_Declaration
      (D_Type
       ((name t)
        (type_expr (T_Fun ((T_Var int) (T_Fun ((T_Var string) (T_Var nat)))))))))) |}]