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

let get_entries ()  =
  let entries, _ = get_flag () in
  let entries = List.map ~f:(Ligo_prim.Value_var.of_input_var ~loc:Location.generated) entries in
  entries

let get_views ()  =
  let _, views = get_flag () in
  let views = List.map ~f:(Ligo_prim.Value_var.of_input_var ~loc:Location.generated) views in
  views

let rec strip_attribute ~raise ~pred d =
  let loc = get_d_loc d in
  let self = strip_attribute ~raise ~pred in
  match get_d d with
  | D_attr (attr, d) -> (
      if pred attr then
        self d
      else
        make_d ~loc (D_attr (attr, self d))
    )
  | D_export d ->
    make_d ~loc (D_export (self d))
  | d -> make_d ~loc d

let rec toplevel_wrap ~raise ~when_ ~wrap d =
  let loc = get_d_loc d in
  let self = toplevel_wrap ~raise ~when_ ~wrap in
  match get_d d with
  | D_attr (attr, d) ->
    make_d ~loc (D_attr (attr, self d))
  | D_export d ->
    make_d ~loc (D_export (self d))
  | D_const { pattern; _ } | D_let { pattern = (pattern, _); _ } | D_multi_const ({ pattern ; _ }, _) ->
    if when_ (get_pattern_binders pattern) then
      wrap ~loc d
    else
      d
  | d -> make_d ~loc d


let compile ~raise =
  let program_entry ~pass : (program_entry, declaration, instruction) program_entry_ -> program_entry = function
    | PE_top_level_instruction i -> raise.error (unsupported_top_level_statement i)
    | PE_declaration d -> pe_declaration (pass d)
    | pe -> make_pe pe
  in
  let top_level : (top_level, program) top_level_ -> top_level = function
    | Top_level (prg : program) ->
      let apply_pass ~pass prg =
        make_prg @@ List.map ~f:(program_entry ~pass) @@ List.map ~f:get_pe @@ get_prg prg
      in
      let prg =
        let pass =
          let pred ({ key ; _ } : Attribute.t) =
            not (List.is_empty (get_entries ())) &&
            String.equal key "entry"
          in
          strip_attribute ~raise ~pred in
        apply_pass ~pass prg
      in
      let prg =
        let wrap ~loc d = d_attr ~loc ({ key = "entry"; value = None }, d) in
        let when_ binders =
          not (List.is_empty (get_entries ())) &&
          List.exists
            binders
            ~f:(fun v -> List.mem ~equal:Ligo_prim.Value_var.equal (get_entries ()) v)
        in
        let pass = toplevel_wrap ~raise ~when_ ~wrap in
        apply_pass ~pass prg
      in
      make_tl (Top_level prg)
  in
  Fold { idle_fold with top_level }


let decompile ~raise:_ = Nothing
let reduction ~raise:_ = Iter.defaults
