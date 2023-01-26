open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* by default, JsLigo declaration all have attribute @private.
   Upon keyword 'export', attribute private must be removed
*)

let compile =
  let declaration : _ declaration_ -> declaration =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | D_Attr (Attribute.{key = "private" ; value = None},decl) as d ->
      (match get_d_export decl with None -> make_d ~loc d | Some x -> x)
    | d -> make_d ~loc d
  in
  `Cata { idle_cata_pass with declaration }


let reduction ~raise =
  { Iter.defaults with
    declaration =
      (function
      | { wrap_content = D_Export _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile
    ~decompile:`None (* for now ? *)
    ~reduction_check:(reduction ~raise)