open Ast_unified
open Pass_type
module Location = Simple_utils.Location

let is_unit e =
  match get_e_literal e with
  | Some Literal_unit -> true
  | _ -> false


let compile =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_constructor { constructor; element }
      when Label.(equal constructor (of_string "Unit")) && is_unit element ->
      e_literal ~loc Literal_unit
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let pass ~raise:_ ~syntax:_ =
  morph ~name:__MODULE__ ~compile ~decompile:`None ~reduction_check:Iter.defaults
