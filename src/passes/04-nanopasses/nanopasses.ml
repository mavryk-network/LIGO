module I = Ast_unified
module O = Ast_core
open Passes.Pass_type
module Errors = Passes.Errors
module Selector = Passes.Pass_type.Selector
open Simple_utils.Function

(* Note:
  In passes, there should be a set of flags for forward (compile)
  and a set of options for backward (decompile) *)
type flags =
  { initial_node_check : bool
  ; duplicate_identifier : bool
  ; restrict_projection : bool
  ; special_unit_constructor : bool
  ; list_as_function : bool
  ; array_to_tuple : bool
  ; match_as_function : bool
  ; object_to_record : bool
  ; detect_recursion : bool
  }

let passes ~(flags : flags) : ((module T) * bool) list =
  let open Passes in
  let { initial_node_check
      ; duplicate_identifier
      ; restrict_projection
      ; special_unit_constructor
      ; list_as_function
      ; array_to_tuple
      ; match_as_function
      ; object_to_record
      ; detect_recursion
      }
    =
    flags
  in
  let always_enabled = true in
  [ (module Initial_node_check), initial_node_check
  ; (module Duplicate_identifier), duplicate_identifier
  ; (module Restrict_projections), restrict_projection
  ; (module Single_switch_block), always_enabled
  ; (module Export_declaration), always_enabled
  ; (module Top_level_restriction), always_enabled
  ; (module Contract_hack), always_enabled
  ; (module Pattern_restriction), always_enabled
  ; (module Unpuning), always_enabled
  ; (module Module_open_restriction), always_enabled
  ; (module Import_restriction), always_enabled
  ; (module External_hack), always_enabled
  ; (module Linearity), always_enabled
  ; (module T_arg), always_enabled
  ; (module Constructor_application), always_enabled
  ; (module Special_unit_constructor), special_unit_constructor
  ; (module Type_abstraction_declaration), always_enabled
  ; (module Named_fun), always_enabled
  ; (module Reverse_application), always_enabled
  ; (module Freeze_operators), always_enabled
  ; (module Literalize_annotated), always_enabled
  ; (module List_as_function), list_as_function
  ; (module Array_to_tuple), array_to_tuple
  ; (module Match_as_function), match_as_function
  ; (module Object_to_record), object_to_record
  ; (module Hack_literalize_jsligo), always_enabled
  ; (module Restrict_t_app), always_enabled
  ; (module T_app_michelson_types), always_enabled
  ; (module Multi_bindings), always_enabled
  ; (module Loop_variable), always_enabled
  ; (module Disc_union_types), always_enabled
  ; (module Returns), always_enabled
  ; (module Reduce_switch), always_enabled
  ; (module Structural_updates), always_enabled
  ; (module Map_lookup), always_enabled
  ; (module Freeze_containers), always_enabled
  ; (module Unstate), always_enabled
  ; (module Projections), always_enabled
  ; (module Assign_transitivity), always_enabled
  ; (module Reduce_sequence), always_enabled
  ; (module Let_syntax), always_enabled
  ; (module Generalize_functions), always_enabled
  ; (module Detect_recursive), detect_recursion
  ; (module Curry), always_enabled
  ; (module Tuple_as_record), always_enabled
  ; (module If_as_pattern_match), always_enabled
  ; (module Restrict_typed_pattern), always_enabled
  ; (module Compute_layout.Normalize_layout), always_enabled
  ; (module Compute_layout.Normalize_no_layout), always_enabled
  ]


let extract_options
    : disable_initial_check:bool -> Compiler_options.t -> flags * Syntax_types.t
  =
 fun ~disable_initial_check options ->
  let syntax =
    Option.value_map options.frontend.syntax ~default:Syntax_types.CameLIGO ~f:Fun.id
  in
  let is_jsligo = Syntax_types.equal syntax JsLIGO in
  let is_pascaligo = Syntax_types.equal syntax PascaLIGO in
  let duplicate_identifier = if options.frontend.transpiled then false else is_jsligo in
  let flags =
    { initial_node_check = not disable_initial_check
    ; duplicate_identifier
    ; restrict_projection = is_jsligo
    ; special_unit_constructor = is_pascaligo
    ; list_as_function = is_jsligo
    ; array_to_tuple = is_jsligo
    ; match_as_function = is_jsligo
    ; object_to_record = is_jsligo
    ; detect_recursion = is_jsligo
    }
  in
  flags, syntax


let get_passes ~options ~disable_initial_check =
  let flags, syntax = extract_options ~disable_initial_check options in
  passes ~flags, syntax


let get_passes_no_options ~syntax =
  (* TODO: decompile_thing should accept options *)
  let flags,_ =
    extract_options
      ~disable_initial_check:false
      Compiler_options.(make ~syntax ~raw_options:(Raw_options.make ()) ())
  in
  passes ~flags


let execute_nanopasses
    ~raise
    ~options
    ~sort
    ?stop_before
    ?(disable_initial_check = false)
    prg
  =
  let passes, syntax = get_passes ~options ~disable_initial_check in
  nanopasses_until ~raise ~syntax passes ?stop_before ~sort prg


let decompile_program ~raise ~syntax : O.program -> I.program =
 fun _ ->
  ignore (raise, syntax);
  assert false


let decompile_expression ~raise ~syntax : O.expression -> I.expr =
  let passes = get_passes_no_options ~syntax in
  decompile_with_passes ~raise ~syntax ~sort:Selector.expr passes
  <@ Trivial.From_core.expression


let decompile_pattern ~raise ~syntax : O.type_expression option O.Pattern.t -> I.pattern =
  let passes = get_passes_no_options ~syntax in
  decompile_with_passes ~raise ~syntax ~sort:Selector.pattern passes
  <@ Trivial.From_core.pattern


let decompile_ty_expr ~raise ~syntax =
  let passes = get_passes_no_options ~syntax in
  decompile_with_passes ~raise ~syntax ~sort:Selector.ty_expr passes
  <@ Trivial.From_core.type_expression


let compile_program ~raise ~(options : Compiler_options.t) ?stop_before
    : I.program -> O.program
  =
  Trivial.To_core.program ~raise
  <@ execute_nanopasses ~raise ~options ~sort:Selector.program ?stop_before


let compile_expression
    ~raise
    ~(options : Compiler_options.t)
    ?(disable_initial_check = false)
    : I.expr -> O.expression
  =
  Trivial.To_core.expression ~raise
  <@ execute_nanopasses ~raise ~options ~sort:Selector.expr ~disable_initial_check
