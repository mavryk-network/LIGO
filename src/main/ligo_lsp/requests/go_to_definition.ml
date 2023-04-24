open Linol_lwt
open Handler
open Utils
module Loc = Simple_utils.Location

(* TODO: use Set from Core *)
module LSet = Caml.Set.Make (Simple_utils.Location_ordered)

let get_definition : Position.t -> DocumentUri.t -> Scopes.def list -> Scopes.def option =
 fun pos uri definitions -> List.find ~f:(Utils.is_reference pos uri) definitions


let on_req_definition : Position.t -> DocumentUri.t -> Locations.t option Handler.t =
 fun pos uri ->
  with_cached_doc uri None
  @@ fun { get_scope_info; _ } ->
  when_some' (get_definition pos uri get_scope_info.definitions)
  @@ fun definition ->
  let region = get_location definition in
  return
  @@
  match region with
  | File region ->
    (* stdlib ranges have an empty file name. They have no definition location. *)
    Option.some_if String.(region#file <> "") (`Location [ region_to_location region ])
  | Virtual _ -> None
