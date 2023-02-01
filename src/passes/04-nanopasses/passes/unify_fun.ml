open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

let compile =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_Block_fun { parameters; lhs_type; body = ExpressionBody body; _ } ->
      let parameters =
        List.map
          ~f:(fun pattern -> Param.{ param_kind = `Const; pattern; param_type = None })
          parameters
      in
      e_poly_fun ~loc { type_params = None; parameters; ret_type = lhs_type; body }
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_Block_fun _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile
    ~decompile:`None (* for now ? *)
    ~reduction_check:(reduction ~raise)