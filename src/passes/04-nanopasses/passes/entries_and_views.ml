open Ast_unified
open Pass_type
open Simple_utils.Trace
(* open Simple_utils.Function *)
open Errors
module Location = Simple_utils.Location

(* Restrictions at top-level :
   - variable declaration (warning)
   - statement (error) *)
let name = __MODULE__

include Flag.With_arg (struct
  type flag = string list * string list
end)

let rec silent_let_to_const ~raise d =
  let entries, _ = get_flag () in
  let _entries = List.map ~f:(Ligo_prim.Value_var.of_input_var ~loc:Location.generated) entries in
  let loc = get_d_loc d in
  ignore raise; ignore silent_let_to_const;
  match get_d d with
  (* | D_attr (attr, d) -> d_attr ~loc (attr, silent_let_to_const ~raise d) *)
  (* | D_const { pattern; _ } -> *)
  (*   let binders = get_pattern_binders pattern in *)
  (*   if List.exists binders ~f:(fun v -> List.mem ~equal:Ligo_prim.Value_var.equal entries v) then *)
  (*     d_attr ~loc ({ key = "entry"; value = None }, d) *)
  (*   else *)
  (*     d *)
  (* | D_let { pattern; _ } -> *)
  (*   let binders = List.concat @@ List.Ne.to_list @@ List.Ne.map get_pattern_binders pattern in *)
  (*   if List.exists binders ~f:(fun v -> List.mem ~equal:Ligo_prim.Value_var.equal entries v) then *)
  (*     d_attr ~loc ({ key = "entry"; value = None }, d) *)
  (*   else *)
  (*     d *)
  (* | D_var dvar -> *)
  (*   raise.warning (`Jsligo_deprecated_toplevel_let loc); *)
  (*   d_const ~loc dvar *)
  (* | D_multi_var dmultvar -> *)
  (*   raise.warning (`Jsligo_deprecated_toplevel_let loc); *)
  (*   d_multi_const ~loc dmultvar *)
  | d -> make_d ~loc d


let compile ~raise =
  let program_entry : (program_entry, declaration, instruction) program_entry_ -> program_entry = function
    | PE_top_level_instruction i -> raise.error (unsupported_top_level_statement i)
    | PE_declaration d -> pe_declaration (silent_let_to_const ~raise d)
    | pe -> make_pe pe
  in
  let top_level : (top_level, program) top_level_ -> top_level = function
    | Top_level (prg : program) ->
      let pes = List.map ~f:get_pe @@ get_prg prg in
      let prg : program = make_prg @@ List.map ~f:program_entry pes in
      make_tl (Top_level prg)
  in
  Fold { idle_fold with top_level }


let decompile ~raise:_ = Nothing
let reduction ~raise:_ = Iter.defaults
