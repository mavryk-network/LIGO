module LSet = Set.Make (Simple_utils.Location)
open Ligo_interface
open Utils
open Utils.Maybe

(* open Types *)
open Linol_lwt
open Linol_lwt.Jsonrpc2
open Lsp

(* This file is free software, part of linol. See file "LICENSE" for more information *)

(* Some user code

   The code here is just a placeholder to make this file compile, it is expected
   that users have an implementation of a processing function for input contents.

   Here we expect a few things:
   - a type to represent a state/environment that results from processing an
     input file
   - a function procdessing an input file (given the file contents as a string),
     which return a state/environment
   - a function to extract a list of diagnostics from a state/environment.
     Diagnostics includes all the warnings, errors and messages that the processing
     of a document are expected to be able to return.
*)

let with_get_scope_info
    : type r.
      (DocumentUri.t, get_scope_info) Hashtbl.t
      -> DocumentUri.t
      -> r
      -> (get_scope_info -> r IO.t)
      -> r IO.t
  =
 fun get_scope_buffers uri default f ->
  Hashtbl.find_opt get_scope_buffers uri
  |> Option.map f
  |> Option.value ~default:(IO.return default)


(* let position_to_string (position :Position.t) =
  "Position { line: " ^ Int.to_string position.line
  ^ ", character: " ^ Int.to_string position.character
  ^ " }"

let range_to_string (range :Range.t) =
  "Range { start: " ^ position_to_string range.start
  ^ ", end: " ^ position_to_string range.end_
  ^ " }"
 *)

(* Lsp server class

   This is the main point of interaction beetween the code checking documents
   (parsing, typing, etc...), and the code of linol.

   The [server] class defines a method for each of the action
   that the lsp server receives, such as opening of a document, when a document
   changes, etc.. By default, the method predefined does nothing (or errors out ?),
   so that users only need to override methods that they want the server to
   actually meaningfully interpret and respond to.
*)
class lsp_server =
  object (self)
    inherit server as super

    (* one env per document *)
    val get_scope_buffers : (DocumentUri.t, get_scope_info) Hashtbl.t = Hashtbl.create 32

    (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc : notify_back -> DocumentUri.t -> string -> unit IO.t =
      fun notify_back uri contents ->
        let new_state = get_scope uri contents in
        Hashtbl.replace get_scope_buffers uri new_state;
        let* () =
          notify_back#send_log_msg
            ~type_:Info
            ("Updating DOC :" ^ DocumentUri.to_string uri)
        in
        let* () =
          match new_state with
          | [], _, _ -> notify_back#send_log_msg ~type_:Info "No erorrs"
          | errors, _, _ ->
            let value = List.map Main_errors.Formatter.error_json errors |> List.concat in
            notify_back#send_log_msg
              ~type_:Info
              ("There are errors:\n *"
              ^ String.concat "\n *"
              @@ List.map (fun (x : Simple_utils.Error.t) -> x.content.message) value)
        in
        let* () =
          match new_state with
          | _, [], _ -> notify_back#send_log_msg ~type_:Info "No warnings"
          | _ -> notify_back#send_log_msg ~type_:Info "There are warnings"
        in
        let* () =
          match new_state with
          | _, _, None -> notify_back#send_log_msg ~type_:Info "No result"
          | _ -> notify_back#send_log_msg ~type_:Info "There is result"
        in
        let dummy_start = Position.create ~character:1 ~line:1 in
        let dummy_end = Position.create ~character:5 ~line:3 in
        let dummy_range = Range.create ~end_:dummy_end ~start:dummy_start in
        let dummy_diag =
          Diagnostic.create ~message:"Dummy diagnostics!" ~range:dummy_range ()
        in
        notify_back#send_diagnostic [ dummy_diag ]

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
      let message =
        ShowMessageParams.create ~message:"WELCOME NOTIFICATION!!!" ~type_:Info
      in
      let* () = notify_back#send_notification (ShowMessage message) in
      self#_on_doc notify_back d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old ~new_content =
      self#_on_doc notify_back d.uri new_content

    (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back _ : unit Linol_lwt.t =
      let* () = notify_back#send_log_msg ~type_:Info "Closed!!!" in
      Linol_lwt.return ()

    method on_req_hover_ : Position.t -> get_scope_info -> Hover.t option IO.t =
      fun pos _ ->
        let marked_string : MarkedString.t =
          { value =
              "Hover on line: "
              ^ Int.to_string pos.line
              ^ " character: "
              ^ Int.to_string pos.character
          ; language = None
          }
        in
        let dummy_content = `MarkedString marked_string in
        let dummy_hover = Hover.create ~contents:dummy_content () in
        IO.return (Some dummy_hover)

    method on_req_formatting_
        : notify_back -> DocumentUri.t -> TextEdit.t list option IO.t =
      fun notify_back uri ->
        let* () =
          notify_back#send_log_msg ~type_:MessageType.Info
          @@ "Formatting request on "
          ^ DocumentUri.to_path uri
        in
        let formatted_text = formatting uri in
        match formatted_text with
        | Ok (result, _) ->
          let formatted_text = TextEdit.create ~newText:result ~range:whole_file_range in
          IO.return (Some [ formatted_text ])
        | Error _ -> IO.return None

    method on_req_definition_ : Position.t -> DocumentUri.t -> get_scope_info -> Locations.t option IO.t =
      fun pos uri (_, _, defs) ->
        IO.return
          (let open Maybe in
          let@ defs, _ = defs in
          let@ region =
            Option.map get_location @@ Requests.Definition.get_definition pos (uri_location_cmp uri) defs
          in
          match region with
          | File region -> Some (`Location [ region_to_location region ])
          | Virtual _ -> None)

    method on_req_rename_ : string -> Position.t -> DocumentUri.t -> get_scope_info -> WorkspaceEdit.t IO.t
        =
      fun new_name pos uri (_, _, defs) ->
        IO.return (
        Option.value (
        let open Maybe in
        let@ (defs, _) = defs in
        let@ definition = Requests.Definition.get_definition pos (uri_location_cmp uri) defs in
        let references =
          Requests.References.get_all_references
            (get_location definition)
            get_scope_buffers
        in
        let changes =
          List.map
            (fun (file, ranges) ->
              file, List.map (Requests.Rename.rename_reference new_name) ranges)
            references
        in
        Some (WorkspaceEdit.create ~changes ()))
        ~default:(WorkspaceEdit.create ()))

    method on_req_references_
        : Position.t -> DocumentUri.t -> get_scope_info -> Location.t list option IO.t =
      fun pos uri (_, _, defs) ->
        IO.return
        (let open Maybe in
        let@ defs, _ = defs in
        let@ definition = Requests.Definition.get_definition pos (uri_location_cmp uri) defs in
        let references =
          Requests.References.get_all_references
            (get_location definition)
            get_scope_buffers
        in
        let locations =
          List.flatten
          @@ List.map
               (fun (file, ranges) ->
                 List.map (fun range -> Location.create ~uri:file ~range) ranges)
               references
        in
        Some locations)

    method! config_hover = Some (`Bool true)
    method config_formatting = Some (`Bool true)
    method! config_definition = Some (`Bool true)
    method config_rename = Some (`Bool true)
    method config_references = Some (`Bool true)

    method! config_modify_capabilities (c : ServerCapabilities.t) : ServerCapabilities.t =
      { c with
        hoverProvider = self#config_hover
      ; documentFormattingProvider = self#config_formatting
      ; definitionProvider = self#config_definition
      ; renameProvider = self#config_rename
      ; referencesProvider = self#config_references
      }

    method! on_request
        : type r.  notify_back:(Server_notification.t -> unit Lwt.t)
                  -> id:Req_id.t
                  -> r Client_request.t
                  -> r IO.t =
      fun ~notify_back ~id (r : _ Client_request.t) ->
        match r with
        | Client_request.TextDocumentFormatting { textDocument; _ } ->
          let uri = textDocument.uri in
          let notify_back = new notify_back ~uri ~notify_back () in
          self#on_req_formatting_ notify_back uri
        | Client_request.TextDocumentDefinition { textDocument; position; _ } ->
          let uri = textDocument.uri in
          (* let notify_back = new notify_back ~uri ~notify_back () in *)
          with_get_scope_info
            get_scope_buffers
            uri
            None
            (self#on_req_definition_ position uri)
        | Client_request.TextDocumentHover { textDocument; position; _ } ->
          let uri = textDocument.uri in
          (* let notify_back = new notify_back ~uri ~notify_back () in *)
          with_get_scope_info get_scope_buffers uri None (self#on_req_hover_ position)
        | Client_request.TextDocumentRename { newName; position; textDocument; _ } ->
          let uri = textDocument.uri in
          let _notify_back = new notify_back ~uri ~notify_back () in
          with_get_scope_info
            get_scope_buffers
            uri
            (WorkspaceEdit.create ())
            (self#on_req_rename_ newName position uri)
        | Client_request.TextDocumentReferences { position; textDocument; _ } ->
          let uri = textDocument.uri in
          (* let notify_back = new notify_back ~uri ~notify_back () in *)
          with_get_scope_info
            get_scope_buffers
            uri
            None
            (self#on_req_references_ position uri)
        | _ -> super#on_request ~notify_back ~id r
  end
