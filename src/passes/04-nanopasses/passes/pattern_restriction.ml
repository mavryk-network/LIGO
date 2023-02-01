open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let compile ~raise =
  let pattern : _ pattern_ -> unit = function
    | {wrap_content = P_literal _ ; _} as p -> raise.error (unsupported_pattern_type p)
    | {wrap_content = P_rest _ ; _} as p -> raise.error (unsupported_pattern_type p)
    | _ -> ()
  in
  `Check { Iter.defaults with pattern }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None
    ~reduction_check:Iter.defaults
