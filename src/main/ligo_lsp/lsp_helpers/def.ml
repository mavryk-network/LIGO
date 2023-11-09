open Imports

type t = Scopes.def

(* TODO use this in Scopes instead of `Loc` and `LSet` *)

module Loc_in_file = struct
  type t =
    { path : Path.t
    ; range : Range.t
    }
  [@@deriving eq, ord, sexp]
end

module Def_location = struct
  type t =
    | File of Loc_in_file.t
    | StdLib of { range : Range.t }
    | Virtual of string
  [@@deriving eq, ord, sexp]

  let of_loc : Loc.t -> t = function
    | File region when Helpers_file.is_stdlib region#file ->
      StdLib { range = Range.of_region region }
    | File region ->
      File { range = Range.of_region region; path = Path.from_absolute region#file }
    | Virtual s -> Virtual s
end

module Def_locations = Set.Make (Def_location)

let to_string (def : t) = Format.asprintf "%a" Scopes.PP.definitions [ def ]

let get_location : Scopes.def -> Def_location.t =
 fun def ->
  Def_location.of_loc
  @@
  match def with
  | Variable vdef -> vdef.range
  | Type tdef -> tdef.range
  | Module mdef -> mdef.range


let get_name : Scopes.def -> string = function
  | Variable vdef -> vdef.name
  | Type tdef -> tdef.name
  | Module mdef -> Scopes.Types.get_mod_name_name mdef.name


let get_def_type : Scopes.def -> Scopes.Types.def_type = function
  | Variable vdef -> vdef.def_type
  | Type tdef -> tdef.def_type
  | Module mdef -> mdef.def_type


let get_mod_path : Scopes.def -> string list = function
  | Variable vdef -> vdef.mod_path
  | Type tdef -> tdef.mod_path
  | Module mdef -> mdef.mod_path


let get_path : Scopes.def -> Path.t option =
  Def_location.(
    function
    | File { path; _ } -> Some path
    | StdLib _ | Virtual _ -> None)
  <@ get_location


let references_getter : Scopes.def -> Def_locations.t =
 fun def ->
  let module LSet = Scopes.Types.LSet in
  let lset =
    match def with
    | Variable vdef -> LSet.add vdef.range vdef.references
    | Type tdef -> LSet.add tdef.range tdef.references
    | Module mdef -> LSet.add mdef.range mdef.references
  in
  Def_locations.of_sequence
  @@ Sequence.map ~f:Def_location.of_loc
  @@ Sequence.of_seq (LSet.to_seq lset)


let is_reference : Position.t -> Path.t -> Scopes.def -> bool =
 fun pos file definition ->
  let check_pos : Def_location.t -> bool = function
    | File { path; range } -> Range.contains_position pos range && Path.equal path file
    | StdLib _ | Virtual _ -> false
  in
  Def_locations.exists ~f:check_pos @@ references_getter definition


let get_definition : Position.t -> Path.t -> t list -> t option =
 fun pos uri definitions -> List.find ~f:(is_reference pos uri) definitions


(* E.g. when [type t = A | B], the type info for A would have
   var_name [Some <tvar for t>]
   and contents A | B (which is a TSum)  *)
type type_info =
  { var_name : Ast_core.type_expression option
  ; contents : Ast_core.type_expression
  }

(* Use most compact type expression available *)
let use_var_name_if_available : type_info -> Ast_core.type_expression =
 fun { var_name; contents } -> Option.value ~default:contents var_name


let get_type (vdef : Scopes.Types.vdef) : type_info option =
  match vdef.t with
  | Core ty -> Some { var_name = None; contents = ty }
  | Resolved ({ location; _ } as ty) ->
    let orig_var =
      Option.map
        ~f:(fun x -> Ast_core.{ type_content = T_variable x; location })
        ty.orig_var (* This is non-empty in case there is a name for our type *)
    in
    Some
      { var_name = orig_var
      ; contents =
          (* We want to preserve both the type var and type expression here, so we
             set [use_orig_var = True] so this expression will be pretty,
             and we also set [orig_var = None] before untyping so we're getting
             full expression and not just `T_variable` *)
          Checking.untype_type_expression ~use_orig_var:true { ty with orig_var = None }
      }
  | Unresolved -> None
