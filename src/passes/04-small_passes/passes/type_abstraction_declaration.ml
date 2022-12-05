open Errors
open Ast_unified
open Pass_type
open Simple_utils.Trace

let compile =
  let pass_declaration : _ declaration_ -> declaration = function
    | { location = loc
      ; wrap_content = D_Type_abstraction { name; params = None; type_expr }
      } -> d_type ~loc { name; type_expr }
    | { location = loc
      ; wrap_content = D_Type_abstraction { name; params = Some params; type_expr }
      } ->
      let type_expr =
        List.Ne.fold_right params ~init:type_expr ~f:(fun ty_binder acc ->
            t_abstraction ~loc { ty_binder; kind = Type; type_ = acc })
      in
      d_type ~loc { name; type_expr }
    | { location = loc; wrap_content } -> make_d ~loc wrap_content
  in
  `Cata { idle_cata_pass with declaration = pass_declaration }


let reduction ~raise =
  { Iter.defaults with
    declaration =
      (function
      | { wrap_content = D_Type_abstraction _; _ } ->
        raise.error (wrong_reduction __MODULE__ "type abs")
      | _ -> ())
  }


let decompile =
  let pass_declaration ({ fp } : declaration) =
    Location.map
      (function
        | D_Type { name; type_expr }
        | D_Type_abstraction { name; params = None; type_expr } ->
          let params =
            Catamorphism.cata_ty_expr
              ~f:(fun x ->
                match x with
                | { wrap_content = T_Abstraction { ty_binder; kind = _; type_ = params } ; _ } -> ty_binder::params
                | _ -> [])
              type_expr
          in
          let tail =
            Catamorphism.cata_ty_expr
              ~f:(function
                | { wrap_content = T_Abstraction { type_ ; _ } ; _ } -> type_
                | { location = loc; wrap_content } -> make_t ~loc wrap_content)
              type_expr
          in
          let params = List.Ne.of_list_opt params in
          D_Type_abstraction {name ; params ; type_expr = tail }
        | x -> x)
      fp
  in
  `Ana { idle_ana_pass with declaration = pass_declaration }


let pass ~raise =
  cata_morph ~name:__MODULE__ ~compile ~decompile ~reduction_check:(reduction ~raise)


let%expect_test "decompile" =
  let raise = raise_failwith "test" in
  let in_prg =
    S_exp.program_entry_of_sexp
    @@ Sexp.of_string
         {|
      (P_Declaration
        (D_Type (
          (name my_t)
          (type_expr
            (T_Abstraction
              ((ty_binder a) (kind Type) (type_ 
              (T_Abstraction
                ((ty_binder b) (kind Type) (type_ (T_Var whatever)))))))))))
    |}
  in
  let out_expr = (pass ~raise).program.backward [ in_prg ] in
  let toto = S_exp.sexp_of_program out_expr in
  let _ = Format.printf "%a" (Sexp.pp_hum_indent 2) toto in
  [%expect
    {|
    ((P_Declaration
       (D_Type_abstraction
         ((name my_t) (params ((a b))) (type_expr (T_Var whatever)))))) |}]