open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let compile =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_Proj { expr; selection = FieldName label } ->
      e_record_access ~loc { struct_ = expr; label }
    | E_Proj { expr; selection = Component_num (label_str, _) } ->
      e_record_access ~loc { struct_ = expr; label = Label.of_string label_str }
    | E_Proj { expr; selection = Component_expr key } ->
      e_constant ~loc { cons_name = C_MAP_FIND_OPT; arguments = [ key; expr ] }
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_Proj _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile
    ~decompile:`None (* for now ? *)
    ~reduction_check:(reduction ~raise)