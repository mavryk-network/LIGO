open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

let generalize ty_params init =
  let loc = get_e_loc init in
  List.Ne.fold_right ty_params ~init ~f:(fun type_binder result ->
      e_type_abstraction ~loc Type_abstraction.{ type_binder; result })


let fun_type_from_parameters ~raise parameters body =
  let param_types_opt =
    List.map parameters ~f:(fun Param.{ pattern; _ } -> get_p_typed pattern)
  in
  List.map param_types_opt ~f:(function
      | Some (ty, _) -> ty
      | None -> raise.error (recursive_no_annot body))


let compile ~raise =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_Poly_fun ({ type_params = Some ty_params; _ } as pf) ->
      let body = generalize ty_params pf.body in
      e_poly_fun ~loc { pf with body }
    | E_Poly_recursive ({ lambda = { type_params = Some ty_params; _ } as pf; _ } as x) ->
      let lambda =
        let body = generalize ty_params pf.body in
        { pf with body }
      in
      e_poly_recursive ~loc { x with lambda }
    | e -> make_e ~loc e
  in
  let declaration : _ declaration_ -> declaration =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | D_Var ({ type_params = Some ty_params; _ } as x) ->
      let let_rhs = generalize ty_params x.let_rhs in
      d_var ~loc { x with let_rhs }
    | D_Const ({ type_params = Some ty_params; _ } as x) ->
      let let_rhs = generalize ty_params x.let_rhs in
      d_const ~loc { x with let_rhs }
    | D_Fun { is_rec; fun_name; type_params; parameters; ret_type; return } ->
      let let_rhs =
        match type_params with
        | Some ty_params ->
          let body = generalize ty_params return in
          if is_rec
          then (
            let fun_type = fun_type_from_parameters ~raise parameters return in
            e_poly_recursive
              ~loc:(get_e_loc return)
              { fun_name
              ; fun_type
              ; lambda = { type_params = None; parameters; ret_type; body }
              })
          else
            e_poly_fun
              ~loc:(get_e_loc return)
              { type_params = None; parameters; ret_type; body }
        | None ->
          e_poly_fun
            ~loc:(get_e_loc return)
            { type_params = None; parameters; ret_type; body = return }
      in
      d_const
        ~loc
        { type_params = None; pattern = p_var ~loc fun_name; rhs_type = None; let_rhs }
    | d -> make_d ~loc d
  in
  `Cata { idle_cata_pass with expr; declaration }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content =
            ( E_Poly_fun { type_params = Some _; _ }
            | E_Poly_recursive { lambda = { type_params = Some _; _ }; _ } )
        ; _
        } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; declaration =
      (function
      | { wrap_content =
            ( D_Var { type_params = Some _; _ }
            | D_Const { type_params = Some _; _ }
            | D_Fun _ )
        ; _
        } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None (* for now ? *)
    ~reduction_check:(reduction ~raise)