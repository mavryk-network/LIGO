open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

let generalize ty_params init =
  let loc = get_e_loc init in
  List.Ne.fold_right ty_params ~init ~f:(fun type_binder result ->
      e_abstraction ~loc Type_abstraction.{ type_binder; result })


let compile =
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
    | d -> make_d ~loc d
  in
  `Cata { idle_cata_pass with expr; declaration }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content =
            ( E_Poly_fun { type_params = Some _ ; _ }
            | E_Poly_recursive { lambda = { type_params = Some _; _ }; _ } )
        ; _
        } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; declaration =
      (function
      | { wrap_content = D_Var { type_params = Some _ ; _ } | D_Const { type_params = Some _ ; _}
        ; _
        } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile
    ~decompile:`None (* for now ? *)
    ~reduction_check:(reduction ~raise)