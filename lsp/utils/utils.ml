open Lsp.Types
module Location = Simple_utils.Location
module Region = Simple_utils.Region
module Pos = Simple_utils.Pos
module Maybe = Maybe
module LSet = Set.Make (Location)

let file_start_position = Position.create ~character:0 ~line:0
let file_end_position = Position.create ~character:0 (* FIXME *) ~line:1000000000
let whole_file_range = Range.create ~end_:file_end_position ~start:file_start_position

let pos_to_position (pos : Pos.t) : Position.t =
  let line_diff = 1 in
  let character_diff = 0 in
  Position.create
    ~line:(pos#line - line_diff)
    ~character:(pos#point_num - pos#point_bol - character_diff)


let region_to_range (region : Region.t) : Range.t =
  Range.create ~start:(pos_to_position region#start) ~end_:(pos_to_position region#stop)


let region_to_location : Region.t -> Lsp.Types.Location.t =
 fun region ->
  Lsp.Types.Location.create
    ~uri:(DocumentUri.of_path region#file)
    ~range:(region_to_range region)


let position_of_location (l : Location.t) : Position.t option =
  match l with
  | Location.Virtual _ -> None
  | File region -> Some (pos_to_position region#start)


let position_le (position_l : Position.t) (position_r : Position.t) : Bool.t =
  position_l.line < position_r.line
  || (position_l.line = position_r.line && position_l.character <= position_r.character)


let is_position_in_range (position : Position.t) (range : Range.t) : Bool.t =
  position_le range.start position && position_le position range.end_


let get_and_filter_references : (Location.t -> bool) -> Scopes.def -> LSet.t =
 fun location_pred defs ->
  let references =
    match defs with
    | Variable vdef -> LSet.add vdef.range vdef.references
    | Type tdef -> LSet.add tdef.range tdef.references
    | Module mdef -> LSet.add mdef.range mdef.references
  in
  LSet.filter location_pred references


let get_location : Scopes.def -> Location.t = function
  | Variable vdef -> vdef.range
  | Type tdef -> tdef.range
  | Module mdef -> mdef.range


let is_reference : Position.t -> (Location.t -> bool) -> Scopes.def -> bool =
 fun pos location_pred defintion ->
  let check_pos : Location.t -> bool = function
    | File reg -> is_position_in_range pos @@ region_to_range reg
    | Virtual _ -> false
  in
  LSet.exists check_pos @@ get_and_filter_references location_pred defintion


let hashtbl_find_map : ('a -> 'b -> 'c option) -> ('a, 'b) Hashtbl.t -> 'c list =
 fun f h ->
  let go k v l =
    match f k v with
    | Some x -> x :: l
    | None -> l
  in
  Hashtbl.fold go h []


let uri_location_cmp : DocumentUri.t -> Location.t -> bool =
 fun uri -> function
  | File region -> DocumentUri.equal uri (DocumentUri.of_path @@ region#file)
  | Virtual _ -> false
