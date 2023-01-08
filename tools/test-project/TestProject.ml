module Ligo = Ligo
module Scopes = Scopes

module LSet = Set.Make(Simple_utils.Location)
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

type get_scope_info = Main_errors.all list
                      * Main_warnings.all list
                      * (Scopes.def list * Scopes.scopes) option

let diagnostics (_state : get_scope_info) : Lsp.Types.Diagnostic.t list =
  let dummy_start = Lsp.Types.Position.create ~character:1 ~line:1 in
  let dummy_end = Lsp.Types.Position.create ~character:5 ~line:3 in
  let dummy_range = Lsp.Types.Range.create ~end_:dummy_end ~start:dummy_start in
  let dummy_diag = Lsp.Types.Diagnostic.create ~message:"Dummy diagnostics!" ~range:dummy_range () in
  [dummy_diag]

let get_scope ~uri:uri =
  let compiler_options = Compiler_options.Raw_options.make () in
  let file_path = Lsp.Uri.to_path uri in
  Ligo_api.Info.get_scope_trace compiler_options file_path ()


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
    val get_scope_buffers: (Lsp.Types.DocumentUri.t, get_scope_info) Hashtbl.t
      = Hashtbl.create 32

    (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc
        ~(notify_back:Linol_lwt.Jsonrpc2.notify_back)
        (uri:Lsp.Types.DocumentUri.t) (_contents:string) =
      let new_state = get_scope ~uri in
      Hashtbl.replace get_scope_buffers uri new_state;
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
      Hashtbl.remove get_scope_buffers d.uri;
      Linol_lwt.return ()

    method on_req_hover_ ~notify_back:_ ~id:_ ~uri:_ ~(pos : Lsp.Types.Position.t) ~workDoneToken:_
        ~get_scope_info:_
        : Lsp.Types.Hover.t option Linol_lwt.Jsonrpc2.IO.t =
      let marked_string : Lsp.Types.MarkedString.t = {
        value = "Hover on line: " ^ Int.to_string (pos.line) ^ " character: " ^ Int.to_string (pos.character)  ;
        language = None
      } in
      let dummy_content = `MarkedString (marked_string) in
      let dummy_hover = Lsp.Types.Hover.create ~contents:dummy_content () in
      Linol_lwt.Jsonrpc2.IO.return (Some dummy_hover)

    method on_req_formatting_ ~notify_back:notify_back ~id:_ ~uri:uri ~get_scope_info:_
        : Lsp.Types.TextEdit.t list option Linol_lwt.Jsonrpc2.IO.t =
        let open Linol_lwt in
        let* () = notify_back#send_log_msg ~type_:MessageType.Info @@ "Formatting request on " ^ Lsp.Uri.to_path uri in
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

    method on_req_definition_  ~notify_back:_ ~id:_ ~uri:uri ~(pos : Lsp.Types.Position.t)
        ~workDoneToken:_ ~partialResultToken:_
        ~get_scope_info:_ : Lsp.Types.Locations.t option Linol_lwt.Jsonrpc2.IO.t =
        let compiler_options = Compiler_options.Raw_options.make () in
        let file_path = Lsp.Uri.to_path uri in
        let _, _, defs = Ligo_api.Info.get_scope_trace compiler_options file_path () in
        let line_diff = 1 in
        let character_diff = 0 in
        let pos_to_position (pos: Simple_utils.Pos.t) =
          Lsp.Types.Position.create ~line:(pos#line - line_diff) ~character:(pos#point_num - pos#point_bol - character_diff)
        in
        let region_to_range (region: Simple_utils.Region.t) =
          Lsp.Types.Range.create ~start:(pos_to_position region#start) ~end_:(pos_to_position region#stop)
        in
        let position_le (position_l: Lsp.Types.Position.t) (position_r: Lsp.Types.Position.t) :Bool.t =
          position_l.line < position_r.line
          || (position_l.line = position_r.line
            && position_l.character <= position_r.character)
        in
        let is_position_in_range (position: Lsp.Types.Position.t) (range: Lsp.Types.Range.t) : Bool.t =
          position_le range.start position
          && position_le position range.end_
        in
        let check_pos (ref:Simple_utils.Location.t) = match ref with
          | File reg -> let range = region_to_range reg in
                        if is_position_in_range pos range
                        then true
                        else false
          | Virtual _ -> false
        in
        let find_def (def: Scopes.def) =
          begin match def with
            | Variable vdef ->
              if LSet.exists check_pos vdef.references
              then
                match vdef.range with
                  | File region -> Some region
                  | _ -> None
              else None
            | Type tdef ->
              if LSet.exists check_pos tdef.references
              then
                match tdef.range with
                  | File region -> Some region
                  | _ -> None
              else None
            | Module mdef ->
              if LSet.exists check_pos mdef.references
              then
                match mdef.range with
                  | File region -> Some region
                  | _ -> None
              else None
          end
        in
        (* let position_to_string (position :Lsp.Types.Position.t) =
          "Position { line: " ^ Int.to_string position.line
          ^ ", character: " ^ Int.to_string position.character
          ^ " }"
        in
        let range_to_string (range :Lsp.Types.Range.t) =
          "Range { start: " ^ position_to_string range.start
          ^ ", end: " ^ position_to_string range.end_
          ^ " }"
        in *)
        begin match defs with
          | Some (defs, _) ->
            begin match List.find_map find_def defs with
              | Some region ->
                let x = `Location [Lsp.Types.Location.create ~uri:(Lsp.Types.DocumentUri.of_path region#file) ~range:(region_to_range region)] in
                Linol_lwt.Jsonrpc2.IO.return @@ Some x
              | _ ->
                Linol_lwt.Jsonrpc2.IO.return None
            end
          | _ ->
            Linol_lwt.Jsonrpc2.IO.return None
        end

    method! config_hover = Some (`Bool true)
    method config_formatting = Some (`Bool true)
    method! config_definition = Some (`Bool true)

    method! config_modify_capabilities (c:Lsp.Types.ServerCapabilities.t) : Lsp.Types.ServerCapabilities.t =
      {c with
        hoverProvider = self#config_hover;
        documentFormattingProvider = self#config_formatting;
        definitionProvider = self#config_definition
      }

    method! on_request
    : type r. notify_back:_ -> id:Linol_lwt.Jsonrpc2.Req_id.t -> r Lsp.Client_request.t -> r Linol_lwt.Jsonrpc2.IO.t
    = fun ~notify_back ~id (r:_ Lsp.Client_request.t) ->
      begin match r with
        | Lsp.Client_request.TextDocumentFormatting {textDocument; _} ->
          let uri = textDocument.uri in
          let notify_back = new Linol_lwt.Jsonrpc2.notify_back ~uri ~notify_back () in
          begin match Hashtbl.find_opt get_scope_buffers uri with
            | None -> Linol_lwt.Jsonrpc2.IO.return None
            | Some get_scope_info ->
              self#on_req_formatting_ ~notify_back ~id
                    ~uri ~get_scope_info
          end
        | Lsp.Client_request.TextDocumentDefinition {
            textDocument; position; workDoneToken; partialResultToken;
          } ->
          let uri = textDocument.uri in
          let notify_back = new Linol_lwt.Jsonrpc2.notify_back ~uri ~notify_back () in
          begin match Hashtbl.find_opt get_scope_buffers uri with
            | None -> Linol_lwt.Jsonrpc2.IO.return None
            | Some get_scope_info ->
              self#on_req_definition_ ~notify_back ~id
                    ~workDoneToken ~partialResultToken
                    ~uri ~pos:position ~get_scope_info
          end
        | Lsp.Client_request.TextDocumentHover { textDocument; position; workDoneToken } ->
            let uri = textDocument.uri in
            let notify_back = new Linol_lwt.Jsonrpc2.notify_back ~uri ~notify_back () in
            begin match Hashtbl.find_opt get_scope_buffers uri with
            | None -> Linol_lwt.Jsonrpc2.IO.return None
            | Some get_scope_info ->
              self#on_req_hover_ ~notify_back ~id
                    ~workDoneToken ~uri ~pos:position
                    ~get_scope_info
            end
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
