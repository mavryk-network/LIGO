open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let wrap_multi_bindings
    : default:'a -> wrap:(loc:Location.t -> declaration -> 'a) -> declaration -> 'a list
  =
 fun ~default ~wrap d ->
  let loc_of_sdecl Simple_decl.{ type_params = _; pattern; rhs_type; let_rhs } =
    List.fold
      ~f:Location.cover
      ~init:Location.generated
      [ get_p_loc pattern
      ; Option.value_map ~default:Location.generated ~f:get_t_loc rhs_type
      ; get_e_loc let_rhs
      ]
  in
  match get_d d with
  | D_Multi_const x ->
    List.map
      ~f:(fun x ->
        let loc = loc_of_sdecl x in
        wrap ~loc @@ d_const ~loc x)
      (List.Ne.to_list x)
  | D_Multi_var x ->
    List.map
      ~f:(fun x ->
        let loc = loc_of_sdecl x in
        wrap ~loc @@ d_var ~loc x)
      (List.Ne.to_list x)
  | _ -> [ default ]


let singlify_block : _ block_ -> block =
 fun block ->
  let f : statement -> statement list -> statement list =
   fun s acc ->
    match get_s s with
    | S_Attr (attr, s) ->
      (match get_s s with
      | S_Decl d ->
        let decls =
          wrap_multi_bindings
            ~default:s
            ~wrap:(fun ~loc d -> s_decl ~loc (d_attr ~loc (attr, d)))
            d
        in
        decls @ acc
      | _ -> s :: acc)
    | S_Decl d ->
      let decls = wrap_multi_bindings ~default:s ~wrap:(fun ~loc d -> s_decl ~loc d) d in
      decls @ acc
    | _ -> s :: acc
  in
  let stmts = List.Ne.to_list (Location.unwrap block) in
  block_of_statements (List.Ne.of_list (List.fold_right ~f ~init:[] stmts))


let singlify_program : program -> program =
 fun prg ->
  let f : program_entry -> program -> program =
   fun pe acc ->
    match get_pe pe with
    | PE_Attr (attr, pe) ->
      (match get_pe pe with
      | PE_Declaration d ->
        let decls =
          wrap_multi_bindings
            ~default:pe
            ~wrap:(fun ~loc d -> pe_declaration (d_attr ~loc (attr, d)))
            d
        in
        decls @ acc
      | _ -> pe :: acc)
    | PE_Declaration d ->
      let decls =
        wrap_multi_bindings ~default:pe ~wrap:(fun ~loc:_ d -> pe_declaration d) d
      in
      decls @ acc
    | _ -> pe :: acc
  in
  List.fold_right ~f ~init:[] prg


let compile =
  let mod_expr : _ mod_expr_ -> mod_expr =
   fun m ->
    let loc = Location.get_location m in
    match Location.unwrap m with
    | M_Body prg -> m_body ~loc (List.Ne.of_list (singlify_program (List.Ne.to_list prg)))
    | m -> make_m ~loc m
  in
  `Cata { idle_cata_pass with mod_expr ; block = singlify_block }


let reduction ~raise =
  { Iter.defaults with
    declaration =
      (function
      | { wrap_content = D_Multi_const _ | D_Multi_var _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  let pass =
    cata_morph
      ~name:__MODULE__
      ~compile
      ~decompile:`None (* for now ? *)
      ~reduction_check:(reduction ~raise)
  in
  let open Simple_utils.Function in
  { pass with
    program = { pass.program with forward = pass.program.forward <@ singlify_program }
  }


open Unit_test_helpers

let%expect_test "multi declaration in block" =
  {|
    ((PE_Declaration
      (D_Const
      ((pattern (P_var f))
        (let_rhs
        (E_Block_fun
          ((parameters ((P_var n))) (lhs_type ())
          (body
            (FunctionBody
            ((S_Decl
              (D_Multi_var (
                ((pattern (P_var x)) (let_rhs (E_variable foo)))
                ((pattern (P_var y)) (let_rhs (E_variable bar)))
                ((pattern (P_var z)) (let_rhs (E_variable baz))))))
             (S_Instr (I_Return ()))))))))))))
    |}
  |-> pass ~raise;
  [%expect
    {|
    ((PE_Declaration
      (D_Const
       ((pattern (P_var f))
        (let_rhs
         (E_Block_fun
          ((parameters ((P_var n))) (lhs_type ())
           (body
            (FunctionBody
             ((S_Decl (D_Var ((pattern (P_var x)) (let_rhs (E_variable foo)))))
              (S_Decl (D_Var ((pattern (P_var y)) (let_rhs (E_variable bar)))))
              (S_Decl (D_Var ((pattern (P_var z)) (let_rhs (E_variable baz)))))
              (S_Instr (I_Return ()))))))))))))
  |}]

let%expect_test "multi declaration in program" =
  {|
    ((PE_Attr ((key inline) (value ())) (PE_Declaration
      (D_Multi_var (
        ((pattern (P_var x)) (let_rhs (E_variable foo)))
        ((pattern (P_var y)) (let_rhs (E_variable bar)))
        ((pattern (P_var z)) (let_rhs (E_variable baz)))))))
     (PE_Declaration
      (D_Var ((pattern (P_var last)) (let_rhs (E_variable last))))))
    |}
  |-> pass ~raise;
  [%expect
    {|
    ((PE_Declaration
      (D_Attr
       (((key inline) (value ()))
        (D_Var ((pattern (P_var x)) (let_rhs (E_variable foo)))))))
     (PE_Declaration
      (D_Attr
       (((key inline) (value ()))
        (D_Var ((pattern (P_var y)) (let_rhs (E_variable bar)))))))
     (PE_Declaration
      (D_Attr
       (((key inline) (value ()))
        (D_Var ((pattern (P_var z)) (let_rhs (E_variable baz)))))))
     (PE_Declaration
      (D_Var ((pattern (P_var last)) (let_rhs (E_variable last))))))
  |}]