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

  let process uri contents =
    let@ syntax =
      match Utils.get_syntax uri with
      | None ->
        lift_IO
        @@ failwith
        @@ "Expected file with LIGO code, got: "
        ^ DocumentUri.to_path uri
      | Some s -> return s
    in
    let new_state = Ligo_interface.unfold_get_scope @@ get_scope uri contents in
    let@ docs_cache = ask_docs_cache in
    Hashtbl.replace
      docs_cache
      uri
      { get_scope_info = new_state; syntax; code = contents; outdated = false };
    let@ { max_number_of_problems; _ } = ask_config in
    let deprecation_warnings =
      match syntax with
      | PascaLIGO ->
        [ Diagnostics.
            { range = None
            ; message = "PascaLIGO is not officially supported in this LIGO version"
            ; severity = DiagnosticSeverity.Warning
            }
        ]
      | CameLIGO | JsLIGO -> []
    in
    let simple_diags = Diagnostics.get_diagnostics new_state in
    let diags =
      List.map
        Diagnostics.from_simple_diagnostic
        (Utils.take max_number_of_problems @@ simple_diags @ deprecation_warnings)
    in
    send_diagnostic diags


  let on_doc : DocumentUri.t -> string -> unit Handler.t =
   fun uri contents ->
    let@ () = send_debug_msg @@ "Updating DOC :" ^ DocumentUri.to_string uri in
    let@ docs_cache = ask_docs_cache in
    match Hashtbl.find_opt docs_cache uri with
    | Some file_data ->
      file_data.outdated <- true;
      file_data.code <- contents;
      return ()
    | None -> process uri contents


  let process_outdated_documents : unit Handler.t =
    let open Ligo_interface in
    let@ docs_cache = ask_docs_cache in
    let tuples = Hashtbl.to_seq docs_cache in
    with_run_in_IO
    @@ fun { unlift_IO } ->
    Lwt_seq.iter_s
      (fun (uri, { code; outdated; _ }) ->
        unlift_IO @@ if outdated then process uri code else return ())
      (Lwt_seq.of_seq tuples)
end
