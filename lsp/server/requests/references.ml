open Lsp.Types
open Ligo_interface
module Location = Simple_utils.Location

module LSet = Set.Make(Location)

let get_references :
  Location.t
  -> Scopes.def list
  -> Range.t list =
fun location defs ->
  defs
  |> List.filter_map (
    fun def ->
      if Location.equal (Utils.get_location def) location
      then Some (LSet.elements @@ Utils.get_and_filter_references ((=) location) def)
      else None)
  |> List.flatten
  |> List.filter_map (function
    | Location.File region -> Some (Utils.region_to_range region)
    | Location.Virtual _ -> None)

let get_all_references :
     Location.t
  -> (DocumentUri.t, get_scope_info) Hashtbl.t
  -> (DocumentUri.t * (Range.t list)) list =
fun location get_scope_buffers ->
  let go file (_, _, defs) =
    match defs with
      | Some (defs, _) ->
        begin match get_references location defs with
          | [] -> None
          | l -> Some (file, l)
        end
      | None -> None in
  Utils.hashtbl_find_map go get_scope_buffers
