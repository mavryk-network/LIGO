open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

(* this pass handle the let-syntax, e.g:
  let f (type a b) x y : t = ..
  let v : t = ..
  let f x y = ..

  In essence, discriminating functions binding from a regular binding (match)
*)

let compile_let_rhs ~loc is_rec fun_pattern type_params parameters rhs_type body =
  let fun_name =
    match get_p fun_pattern with
    | P_var x -> x
    | _ -> failwith "unexpect let binding"
  in
  let parameters =
    List.map parameters ~f:(fun pattern ->
        Param.{ param_kind = `Const; pattern; param_type = None })
  in
  let fun_ = Poly_fun.{ type_params; parameters; ret_type = rhs_type; body } in
  (* Note: fun_type is not yet generalized, for now it has unit type *)
  if is_rec
  then e_poly_recursive ~loc Recursive.{ fun_name; fun_type = (); lambda = fun_ }
  else e_poly_fun ~loc fun_


let compile =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_Let_in
        { is_rec = false; type_params = None; lhs = binder, []; rhs_type; rhs; body } ->
      let rhs =
        Option.value_map rhs_type ~default:rhs ~f:(fun t ->
            e_annot ~loc:(get_e_loc rhs) (rhs, t))
      in
      e_simple_let_in ~loc { binder; rhs; let_result = body }
    | E_Let_in { is_rec; type_params; lhs = fun_pattern, parameters; rhs_type; rhs; body }
      ->
      let rhs =
        compile_let_rhs ~loc is_rec fun_pattern type_params parameters rhs_type rhs
      in
      e_simple_let_in ~loc { binder = fun_pattern; rhs; let_result = body }
    | e -> make_e ~loc e
  in
  let declaration : _ declaration_ -> declaration =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | D_Let
        { is_rec = false; type_params = None; pattern = pattern, []; rhs_type; let_rhs }
      ->
      let expr =
        Option.value_map rhs_type ~default:let_rhs ~f:(fun t ->
            e_annot ~loc:(get_e_loc let_rhs) (let_rhs, t))
      in
      d_irrefutable_match ~loc { pattern; expr }
    | D_Let { is_rec; type_params; pattern = fun_pattern, parameters; rhs_type; let_rhs }
      ->
      let rhs =
        compile_let_rhs ~loc is_rec fun_pattern type_params parameters rhs_type let_rhs
      in
      (* TODO: wait, why d_const has type_params ??? *)
      d_const
        ~loc
        Simple_decl.{ type_params = None; pattern = fun_pattern; rhs_type; let_rhs = rhs }
    | d -> make_d ~loc d
  in
  `Cata { idle_cata_pass with expr; declaration }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_Let_in _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; declaration =
      (function
      | { wrap_content = D_Let _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile
    ~decompile:`None (* for now ? *)
    ~reduction_check:(reduction ~raise)