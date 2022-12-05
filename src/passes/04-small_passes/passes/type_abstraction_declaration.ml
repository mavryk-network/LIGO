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
          | (D_Type { name; type_expr } | D_Type_abstraction { name; type_expr ; params = None}) as d -> (
            match get_t type_expr with
            | T_Abstraction { ty_binder ; kind ; type_ } ->
              ignore kind ; (* I guess that's normal ? *)
              D_Type_abstraction {name ; params = Some (List.Ne.singleton ty_binder) ; type_expr = type_ }
            | _ -> d
          )
          | D_Type_abstraction { name; type_expr ; params = Some params } as d -> (
            match get_t type_expr with
            | T_Abstraction { ty_binder ; kind ; type_ } ->
              ignore kind ; (* I guess that's normal ? *)
              D_Type_abstraction {name ; params = Some (List.Ne.cons ty_binder params) ; type_expr = type_ }
            | _ -> d
          )
          | x -> x )
        fp
  in
  `Ana { idle_ana_pass with declaration = pass_declaration }


let pass ~raise =
  cata_morph ~name:__MODULE__ ~compile ~decompile ~reduction_check:(reduction ~raise)

let%expect_test "lol" =
  let b = Ty_variable.of_input_var "a" in
  let bv : ty_expr = { fp = Location.wrap (T_Var b )} in
  let type_ : ty_expr = {fp = Location.wrap (T_Prod (bv,[bv; bv]))} in
  (* let toto = S_exp.sexp_of_ty_expr ({fp = Location.wrap (T_Abstraction {ty_binder = b ; kind = Type ; type_ })}) in *)
  let toto = S_exp.sexp_of_declaration ({fp = Location.wrap (D_Type_abstraction {name = b ; params= Some (b,[b; b]) ; type_expr = type_ })}) in
  let _ = Format.printf "%a" (Sexp.pp_hum_indent 2) toto in
  [%expect
    {|
      (D_Type_abstraction
        ((name a) (params ((a a a)))
          (type_expr (T_Prod ((T_Var a) (T_Var a) (T_Var a))))))  |}]


let%expect_test "addition" =
  let raise = raise_failwith "test" in
  let in_prg =
    S_exp.program_entry_of_sexp
    @@ Sexp.of_string
    {|
      (P_Declaration
        (D_Type_abstraction (
          (name my_t)
          (params ((a b)))
          (type_expr (
            T_Prod (
              (T_Var a)
              (T_Var b)))))))
    |}
  in
  let out_expr = (pass ~raise).program.backward [in_prg] in
  let toto = S_exp.sexp_of_program out_expr in
  let _ = Format.printf "%a" (Sexp.pp_hum_indent 2) toto in
  [%expect{|
    ((P_Declaration
       (D_Type_abstraction
         ((name my_t) (params ((a b)))
           (type_expr (T_Prod ((T_Var a) (T_Var b))))))))
  |}]