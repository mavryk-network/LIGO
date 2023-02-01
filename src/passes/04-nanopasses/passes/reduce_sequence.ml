open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let compile =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_Sequence lst ->
      List.fold_right lst ~init:(e_unit ~loc) ~f:(fun el acc -> let_unit_in el acc)
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_Sequence _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile
    ~decompile:`None
    ~reduction_check:(reduction ~raise)
