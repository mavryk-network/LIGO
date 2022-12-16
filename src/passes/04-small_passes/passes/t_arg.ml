open Ast_unified
open Pass_type

(* this pass is stupid, lol :D *)

let compile =
  let pass_ty : _ ty_expr_ -> ty_expr = function
    | { location = loc; wrap_content = T_Arg s } ->
      let quote_var var = "'" ^ var in
      t_var ~loc (Ty_variable.of_input_var ~loc (quote_var s))
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  `Cata { idle_cata_pass with ty_expr = pass_ty }


let reduction_check = Iter.defaults
let decompile = `Cata idle_cata_pass
let pass = cata_morph ~name:__MODULE__ ~compile ~decompile ~reduction_check

open Unit_test_helpers

let%expect_test "addition" =
  {|
    ((PE_Declaration
      (D_Type_abstraction ((name t) (params (x))
        (type_expr (T_Arg x))))))
  |} |-> pass ;
  [%expect{|
    ((PE_Declaration
       (D_Type_abstraction ((name t) (params ((x))) (type_expr (T_Var 'x)))))) |}]