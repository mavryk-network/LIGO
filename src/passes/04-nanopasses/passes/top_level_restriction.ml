open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let compile ~raise =
  let program : _ program_entry_ -> unit = function
    | PE_Top_level_instruction i -> raise.error (unsupported_top_level_statement i)
    | _ -> ()
  in
  `Check { Iter.defaults with program }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None
    ~reduction_check:Iter.defaults