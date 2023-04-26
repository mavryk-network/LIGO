open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils.Function
open Errors
module Location = Simple_utils.Location

let rec silent_let_to_const ~raise d =
  let loc = get_d_loc d in
  match get_d d with
  | D_attr (attr, d) -> d_attr ~loc (attr, silent_let_to_const ~raise d)
  | D_var dvar ->
    raise.warning (`Jsligo_deprecated_toplevel_let loc);
    d_const ~loc dvar
  | D_multi_var dmultvar ->
    raise.warning (`Jsligo_deprecated_toplevel_let loc);
    d_multi_const ~loc dmultvar
  | d -> make_d ~loc d


let compile ~raise =
  let program_entry : _ program_entry_ -> program_entry = function
    | PE_top_level_instruction i -> raise.error (unsupported_top_level_statement i)
    | PE_declaration d -> pe_declaration (silent_let_to_const ~raise d)
    | pe -> make_pe pe
  in
  let program : _ program_ -> program =
    make_prg <@ List.map ~f:(program_entry <@ get_pe)
  in
  `Cata { idle_cata_pass with program_entry; program }


let pass ~raise ~syntax:_ =
  morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None
    ~reduction_check:Iter.defaults
