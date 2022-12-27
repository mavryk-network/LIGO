module Ligo = Ligo
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

type state_after_processing = unit

let process_some_input_file (_file_contents : string) : state_after_processing =
  ()

let diagnostics (_state : state_after_processing) : Lsp.Types.Diagnostic.t list =
  let dummy_start = Lsp.Types.Position.create ~character:1 ~line:1 in
  let dummy_end = Lsp.Types.Position.create ~character:5 ~line:3 in
  let dummy_range = Lsp.Types.Range.create ~end_:dummy_end ~start:dummy_start in
  let dummy_diag = Lsp.Types.Diagnostic.create ~message:"Dummy diagnostics!" ~range:dummy_range () in
  [dummy_diag]


(* Lsp server class

   This is the main point of interaction beetween the code checking documents
   (parsing, typing, etc...), and the code of linol.

   The [Linol_lwt.Jsonrpc2.server] class defines a method for each of the action
   that the lsp server receives, such as opening of a document, when a document
   changes, etc.. By default, the method predefined does nothing (or errors out ?),
   so that users only need to override methods that they want the server to
   actually meaningfully interpret and respond to.
*)
class lsp_server =
  object(self)
    inherit Linol_lwt.Jsonrpc2.server as super

    (* one env per document *)
    val buffers: (Lsp.Types.DocumentUri.t, state_after_processing) Hashtbl.t
      = Hashtbl.create 32

    (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc
        ~(notify_back:Linol_lwt.Jsonrpc2.notify_back)
        (uri:Lsp.Types.DocumentUri.t) (contents:string) =
      let new_state = process_some_input_file contents in
      Hashtbl.replace buffers uri new_state;
      let diags = diagnostics new_state in
      let open Linol_lwt in
      let* () = notify_back#send_log_msg ~type_:Info "Welcome!!!" in
      let message = ShowMessageParams.create ~message:"WELCOME NOTIFICATION!!!" ~type_:Info in
      let* () = notify_back#send_notification (ShowMessage message) in
      let* () = notify_back#send_diagnostic diags in
      Lwt.return ()

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
      self#_on_doc ~notify_back d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ d : unit Linol_lwt.t =
      Hashtbl.remove buffers d.uri;
      Linol_lwt.return ()

    method on_req_hove ~notify_back:_ ~id:_ ~uri:_ ~(pos : Lsp.Types.Position.t) ~workDoneToken:_
        : Lsp.Types.Hover.t option Linol_lwt.Jsonrpc2.IO.t =
      let marked_string : Lsp.Types.MarkedString.t = {
        value = "Hover on line: " ^ Int.to_string (pos.line) ^ " character: " ^ Int.to_string (pos.character)  ;
        language = None
      } in
      let dummy_content = `MarkedString (marked_string) in
      let dummy_hover = Lsp.Types.Hover.create ~contents:dummy_content () in
      Linol_lwt.Jsonrpc2.IO.return (Some dummy_hover)

    method! config_hover = Some (`Bool true)
    method config_formatting = Some (`Bool true)

    method! config_modify_capabilities (c:Lsp.Types.ServerCapabilities.t) : Lsp.Types.ServerCapabilities.t =
      {c with
        hoverProvider = self#config_hover;
        documentFormattingProvider = self#config_formatting
      }

    method! on_request
    : type r. notify_back:_ -> id:Linol_lwt.Jsonrpc2.Req_id.t -> r Lsp.Client_request.t -> r Linol_lwt.Jsonrpc2.IO.t
    = fun ~notify_back ~id (r:_ Lsp.Client_request.t) ->
      begin match r with
        | Lsp.Client_request.TextDocumentFormatting {textDocument; _} ->
          let open Linol_lwt in
          let uri = textDocument.uri in
          let notify_back = new Linol_lwt.Jsonrpc2.notify_back ~uri ~notify_back () in
          let* () = notify_back#send_log_msg ~type_:Info @@ "Formatting request on " ^ Lsp.Uri.to_path textDocument.uri in
          let compiler_options = Compiler_options.Raw_options.make () in
          let file_path = Lsp.Uri.to_path uri in
          let display_format = Simple_utils.Display.human_readable in
          let text = Ligo_api.Print.pretty_print compiler_options file_path display_format () in
          begin match text with
            | Ok (result, _) ->
              let file_start = Lsp.Types.Position.create ~character:0 ~line:0 in
              let file_end = Lsp.Types.Position.create ~character:0 ~line:1000000000 in
              let whole_file_range = Lsp.Types.Range.create ~end_:file_end ~start:file_start in
              let formatted_text = Lsp.Types.TextEdit.create ~newText:result ~range:whole_file_range in
              Linol_lwt.Jsonrpc2.IO.return (Some [formatted_text])
            | Error _ -> Linol_lwt.Jsonrpc2.IO.return None
          end
        | Lsp.Client_request.TextDocumentHover { textDocument; position; workDoneToken } ->
            let uri = textDocument.uri in
            let notify_back = new Linol_lwt.Jsonrpc2.notify_back ~uri ~notify_back () in
            self#on_req_hove ~notify_back ~id ~uri ~pos:position ~workDoneToken
        | _ -> super#on_request ~notify_back ~id r
    end
  end

(* Main code
   This is the code that creates an instance of the lsp server class
   and runs it as a task. *)
let run () =
  let s = new lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio (s :> Linol_lwt.Jsonrpc2.server) in
  let task = Linol_lwt.Jsonrpc2.run server in
  match Linol_lwt.run task with
  | () -> ()
  | exception e ->
    let e = Printexc.to_string e in
    Printf.eprintf "error: %s\n%!" e;
    exit 1

(* Finally, we actually run the server *)
let () = run ()
