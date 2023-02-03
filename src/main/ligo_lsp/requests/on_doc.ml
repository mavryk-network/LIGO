module Make (Ligo_api : Ligo_interface.LIGO_API) = struct
  (* TODO: use String, Option, Set, List & Hashtbl from Core *)
  open Linol_lwt
  module Hashtbl = Caml.Hashtbl
  open Ligo_interface.Make (Ligo_api)
  module List = Caml.List
  open Handler
  module Diagnostics = Diagnostics

  (* We define here a helper that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
  let on_doc : DocumentUri.t -> string -> unit Handler.t =
   fun uri contents ->
    let@ get_scope_buffers = ask_docs_cache in
    let new_state = Ligo_interface.unfold_get_scope @@ get_scope uri contents in
    Hashtbl.replace get_scope_buffers uri new_state;
    let@ () = send_debug_msg ("Updating DOC :" ^ DocumentUri.to_string uri) in
    let simple_diags = Diagnostics.get_diagnostics new_state in
    let diags = List.map Diagnostics.from_simple_diagnostic simple_diags in
    send_diagnostic diags
end
