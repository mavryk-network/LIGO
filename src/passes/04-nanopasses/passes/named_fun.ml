open Simple_utils.Trace
open Ast_unified
open Pass_type
open Errors

module rec _ : DOC = struct
  (*
  in Jsligo, parameters in arrow types have name, we simply drop it
  `(foo:int) => (bar:string) => nat` |-> `int -> string -> nat`

  note: we could remember them somehow in the future
  *)
  open Unit_test_helpers.Program

  let%expect_test "compile" =
    {|
    ((PE_declaration
      (D_type
        ( (name t)
          (type_expr
            (T_named_fun
              ((((name (foo)) (type_expr (T_var int))) ((name (bar)) (type_expr (T_var string))))
                (T_var nat))))))))
  |}
    |-> Pass.pass ~raise;
    [%expect
      {|
    ((PE_declaration
      (D_type
       ((name t)
        (type_expr (T_fun ((T_var int) (T_fun ((T_var string) (T_var nat)))))))))) |}]
end

and Pass : PASS = struct
  let compile =
    let pass_ty : _ ty_expr_ -> ty_expr = function
      | { location = loc; wrap_content = T_named_fun (params, ret) } ->
        let params = List.map ~f:(fun { type_expr; name = _ } -> type_expr) params in
        t_fun_of_list ~loc (params @ [ ret ])
      | { location = loc; wrap_content } -> make_t ~loc wrap_content
    in
    `Cata { idle_cata_pass with ty_expr = pass_ty }


  let reduction ~raise =
    { Iter.defaults with
      ty_expr =
        (function
        | { wrap_content = T_named_fun _; _ } -> raise.error (wrong_reduction __MODULE__)
        | _ -> ())
    }


  let pass ~raise ~syntax:_ =
    morph ~name:__MODULE__ ~compile ~decompile:`None ~reduction_check:(reduction ~raise)
end
