module Make (Ligo_api : Ligo_interface.LIGO_API) = struct
  (* TODO: use String, Option, Set, List & Hashtbl from Core *)
  open Linol_lwt
  module Hashtbl = Caml.Hashtbl
  open Ligo_interface.Make (Ligo_api)
  module List = Caml.List
  open Handler
  module Diagnostics = Diagnostics

  let get_file_data : DocumentUri.t -> string -> Ligo_interface.file_data Handler.t =
   fun uri contents ->
    let open Ligo_interface in
    let@ syntax =
      match Utils.get_syntax uri with
      | None ->
        lift_IO
        @@ failwith
        @@ "Expected file with LIGO code, got: "
        ^ DocumentUri.to_path uri
      | Some s -> return s
    in
    let new_state = unfold_get_scope @@ get_scope uri contents in
    let file_data = { get_scope_info = new_state; syntax; code = contents } in
    return file_data


  let publish_diagnostics : Ligo_interface.file_data -> unit Handler.t =
   fun { get_scope_info; syntax; _ } ->
    let simple_diags = Diagnostics.get_diagnostics get_scope_info in
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
    let diags =
      List.map Diagnostics.from_simple_diagnostic (simple_diags @ deprecation_warnings)
    in
    send_diagnostic diags


  (* let update_threads_1 thread =
    match Lwt.state thread with
    | Lwt.Fail _ | Lwt.Return _ -> No_thread
    | Lwt.Sleep -> One_thread thread


  let update_threads_2 old_thread new_thread =
    match Lwt.state old_thread, Lwt.state new_thread with
    | Lwt.Sleep, Lwt.Sleep -> Two_threads { running = old_thread; on_hold = new_thread }
    | Lwt.Sleep, Lwt.Return _ ->
      let () = Lwt.cancel old_thread in
      No_thread
    | Lwt.Return _, Lwt.Return _ -> No_thread
    | Lwt.Return _, Lwt.Sleep -> One_thread new_thread
    | Lwt.Fail _, Lwt.Fail _ -> No_thread
    | (Lwt.Return _ | Lwt.Sleep), Lwt.Fail _ -> One_thread old_thread
    | Lwt.Fail _, (Lwt.Return _ | Lwt.Sleep) -> One_thread new_thread *)


  (*
    We define here a helper that will:
    - Process a document.
    - Store the state resulting from the processing.
    - Return the diagnostics from the new state.
  *)
  let on_doc : DocumentUri.t -> string -> unit Handler.t =
   fun uri contents ->
    let@ () = send_debug_msg @@ "Updating DOC :" ^ DocumentUri.to_string uri in
    let file_data_promise () = get_file_data uri contents in
    let@ get_scope_buffers = ask_docs_cache in
    match Hashtbl.find_opt get_scope_buffers uri with
    (* First time opening this document. We can proceed with our ordinary
       business synchronously. *)
    | None ->
      let@ file_data = file_data_promise () in
      let@ () = publish_diagnostics file_data in
      let@ () = send_debug_msg "No thread (None)" in
      let doc_info =
        { thread_info = Lwt_mvar.create No_thread; file_data = Lwt_mvar.create file_data }
      in
      return @@ Hashtbl.add get_scope_buffers uri doc_info
    | Some doc_info ->
      with_run_in_IO
      @@ fun { unlift_IO } ->
      let file_data_then_diags = Caml_threads.Thread.create (fun () ->
        (* Creating a new async thread *)
        Utils.Lwt_mvar.using_ doc_info.file_data
        @@ fun _old_file_data ->
        unlift_IO
          (let@ () = send_debug_msg "Creating new thread" in
           let@ file_data =  file_data_promise () in
           let@ () = publish_diagnostics file_data in
           return file_data)) ()
      in
      Utils.Lwt_mvar.using_ doc_info.thread_info
      @@ (function
      (* Nothing is running, continue as usual. *)
      | No_thread ->
        let* () = unlift_IO @@ send_debug_msg "No thread" in
        Lwt.return @@ One_thread file_data_then_diags
        (* Lwt.return @@ update_threads_1 file_data_then_diags *)
      (* There is one thread running. Let's wait until it's complete and then
         run the new one in the background. *)
      | One_thread running ->
        let* () = unlift_IO @@ send_debug_msg "One thread" in
        (* TODO: handle diagnostics *)
        Lwt.map (fun () -> update_threads_2 running file_data_then_diags) running
      (* One thread is running and another is waiting. Let's forget the old
         one and run this call instead. *)
      | Two_threads { running; on_hold } ->
        let* () = unlift_IO @@ send_debug_msg "Two threads" in
        let () = Lwt.cancel on_hold in
        (* TODO: handle diagnostics *)
        Lwt.map (fun () -> update_threads_2 running file_data_then_diags) running)
end
