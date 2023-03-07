open Ligo_interface
open Linol_lwt
open Linol_lwt.Jsonrpc2
module Loc = Simple_utils.Location

(* TODO: use Set, List & Hashtbl from Core *)
module LSet = Caml.Set.Make (Loc)
module Hashtbl = Caml.Hashtbl
module List = Caml.List
open Handler
open Utils

let get_references : DocumentUri.t -> Loc.t -> Scopes.def list -> Range.t list =
 fun uri location defs ->
  defs
  |> List.filter_map (fun def ->
         if Loc.equal (get_location def) location
         then Some (LSet.elements @@ get_references_of_file uri def)
         else None)
  |> List.flatten
  |> List.filter_map (function
         | Loc.File region -> Some (region_to_range region)
         | Loc.Virtual _ -> None)


let get_all_references
    :  Loc.t -> (DocumentUri.t, document_info) Hashtbl.t
    -> (DocumentUri.t * Range.t list) list Handler.t
  =
 fun location get_scope_buffers ->
  let go (file, document_info) =
    Lwt_mvar.using document_info.file_data
    @@ fun file_data ->
    let defs = file_data.get_scope_info.definitions in
    Lwt.return
    @@ ( file_data
       , match get_references file location defs with
         | [] -> None
         | l -> Some (file, l) )
  in
  get_scope_buffers
  |> Hashtbl.to_seq
  |> Lwt_seq.of_seq
  |> Lwt_seq.filter_map_s go
  |> Lwt_seq.to_list
  |> lift_IO


let on_req_references : Position.t -> DocumentUri.t -> Location.t list option Handler.t =
 fun pos uri ->
  let@ get_scope_buffers = ask_docs_cache in
  with_cached_doc uri None
  @@ fun { get_scope_info; _ } ->
  when_some (Definition.get_definition pos uri get_scope_info.definitions)
  @@ fun definition ->
  let@ references = get_all_references (get_location definition) get_scope_buffers in
  let@ () = send_debug_msg @@ "On references request on " ^ DocumentUri.to_path uri in
  let show_reference (uri, ranges) =
    DocumentUri.to_path uri
    ^ "\n"
    ^ String.concat ~sep:"\n"
    @@ List.map range_to_string ranges
  in
  let@ () =
    send_debug_msg @@ String.concat ~sep:"\n" @@ List.map show_reference references
  in
  references
  |> List.map (fun (file, ranges) ->
         List.map (fun range -> Location.create ~uri:file ~range) ranges)
  |> List.flatten
  |> return
