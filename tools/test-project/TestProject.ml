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

type get_scope_info =
    Main_errors.all list
  * Main_warnings.all list
  * (Scopes.def list * Scopes.scopes) option

let get_scope : Lsp.Uri.t -> string -> get_scope_info =
fun uri source ->
  let compiler_options = Compiler_options.Raw_options.make () in
  let file_path = Lsp.Uri.to_path uri in
  Ligo_api.Info.get_scope_trace compiler_options (Raw { id = file_path; code = source }) ()

let with_get_scope_info : type r.
     (Lsp.Types.DocumentUri.t, get_scope_info) Hashtbl.t
  -> Lsp.Uri.t
  -> r
  -> (get_scope_info -> r Linol_lwt.Jsonrpc2.IO.t)
  -> r Linol_lwt.Jsonrpc2.IO.t =
fun get_scope_buffers uri default f ->
  begin match Hashtbl.find_opt get_scope_buffers uri with
    | None -> Linol_lwt.Jsonrpc2.IO.return default
    | Some x -> f x
  end

let pos_to_position (pos: Simple_utils.Pos.t) =
  let line_diff = 1 in
  let character_diff = 0 in
  Lsp.Types.Position.create ~line:(pos#line - line_diff) ~character:(pos#point_num - pos#point_bol - character_diff)

let region_to_range (region: Simple_utils.Region.t) =
  Lsp.Types.Range.create ~start:(pos_to_position region#start) ~end_:(pos_to_position region#stop)

let position_le (position_l: Lsp.Types.Position.t) (position_r: Lsp.Types.Position.t) :Bool.t =
  position_l.line < position_r.line
  || (position_l.line = position_r.line
    && position_l.character <= position_r.character)

let is_position_in_range (position: Lsp.Types.Position.t) (range: Lsp.Types.Range.t) : Bool.t =
  position_le range.start position
  && position_le position range.end_

(* let position_to_string (position :Lsp.Types.Position.t) =
  "Position { line: " ^ Int.to_string position.line
  ^ ", character: " ^ Int.to_string position.character
  ^ " }"

let range_to_string (range :Lsp.Types.Range.t) =
  "Range { start: " ^ position_to_string range.start
  ^ ", end: " ^ position_to_string range.end_
  ^ " }"
 *)

let get_references : Scopes.def -> LSet.t =
function
  | Variable vdef -> LSet.add vdef.range vdef.references
  | Type tdef ->  LSet.add tdef.range tdef.references
  | Module mdef ->  LSet.add mdef.range mdef.references

let get_range : Scopes.def -> Simple_utils.Location.t =
  function
    | Variable vdef -> vdef.range
    | Type tdef -> tdef.range
    | Module mdef -> mdef.range

let is_reference : Lsp.Types.Position.t -> Scopes.def -> bool =
fun pos defintion ->
  let check_pos : Simple_utils.Location.t -> bool =
  function
    | File reg -> is_position_in_range pos @@ region_to_range reg
    | Virtual _ -> false
  in
  LSet.exists check_pos @@ get_references defintion

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
    method private _on_doc :
      Linol_lwt.Jsonrpc2.notify_back
      -> Lsp.Types.DocumentUri.t
      -> string
      -> unit Linol_lwt.Jsonrpc2.IO.t =
    fun notify_back uri contents ->
      let open Linol_lwt in
      let* () = notify_back#send_log_msg ~type_:Info "Welcome!!!" in
      let message = ShowMessageParams.create ~message:"WELCOME NOTIFICATION!!!" ~type_:Info in
      let* () = notify_back#send_notification (ShowMessage message) in

      let new_state = get_scope uri contents in
      Hashtbl.replace get_scope_buffers uri new_state;

      let dummy_start = Lsp.Types.Position.create ~character:1 ~line:1 in
      let dummy_end = Lsp.Types.Position.create ~character:5 ~line:3 in
      let dummy_range = Lsp.Types.Range.create ~end_:dummy_end ~start:dummy_start in
      let dummy_diag = Lsp.Types.Diagnostic.create ~message:"Dummy diagnostics!" ~range:dummy_range () in
      notify_back#send_diagnostic [dummy_diag]

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
      self#_on_doc notify_back d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old ~new_content =
      self#_on_doc notify_back d.uri new_content

    (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back d : unit Linol_lwt.t =
      Hashtbl.remove get_scope_buffers d.uri;
      let open Linol_lwt in
      let* () = notify_back#send_log_msg ~type_:Info "Closed!!!" in
      Linol_lwt.return ()

    method on_req_hover_ :
         Lsp.Types.Position.t
      -> get_scope_info
      -> Lsp.Types.Hover.t option Linol_lwt.Jsonrpc2.IO.t =
    fun pos _ ->
      let marked_string : Lsp.Types.MarkedString.t = {
        value = "Hover on line: " ^ Int.to_string (pos.line) ^ " character: " ^ Int.to_string (pos.character)  ;
        language = None
      } in
      let dummy_content = `MarkedString (marked_string) in
      let dummy_hover = Lsp.Types.Hover.create ~contents:dummy_content () in
      Linol_lwt.Jsonrpc2.IO.return (Some dummy_hover)

    method on_req_formatting_ :
         Linol_lwt.Jsonrpc2.notify_back
      -> Lsp.Uri.t
      -> Lsp.Types.TextEdit.t list option Linol_lwt.Jsonrpc2.IO.t =
    fun notify_back uri ->
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

    method on_req_definition_ :
         Linol_lwt.Jsonrpc2.notify_back
      -> Jsonrpc.Id.t
      -> Lsp.Uri.t
      -> Lsp.Types.Position.t
      -> get_scope_info
      -> Lsp.Types.Locations.t option Linol_lwt.Jsonrpc2.IO.t =
    fun _ _ _ pos (_, _, defs) ->
      begin match defs with
        | Some (defs, _) ->
          begin match Option.map get_range @@ List.find_opt (is_reference pos) defs with
            | Some (File region) ->
              let x = `Location [
                Lsp.Types.Location.create
                  ~uri:(Lsp.Types.DocumentUri.of_path region#file)
                  ~range:(region_to_range region)
                ] in
              Linol_lwt.Jsonrpc2.IO.return @@ Some x
            | _ ->
              Linol_lwt.Jsonrpc2.IO.return None
          end
        | _ ->
          Linol_lwt.Jsonrpc2.IO.return None
      end

    method get_ranges :
      Lsp.Types.Position.t
      -> get_scope_info
      -> (Lsp.Uri.t * (Lsp.Types.Range.t list)) list option =
    fun pos (_, _, defs) ->
      begin match defs with
        | Some (defs, _) ->
          begin match Option.map get_range @@ List.find_opt (is_reference pos) defs with
            | Some location ->
              let go file (_, _, defs) res =
                begin match defs with
                  | Some (defs, _) ->
                    let regions = defs
                      |> List.filter_map (
                          fun def ->
                            if Simple_utils.Location.equal (get_range def) location
                            then Some (LSet.elements @@ get_references def)
                            else None
                        )
                      |> List.flatten
                      |> List.filter_map (function
                        | Simple_utils.Location.File region -> Some (region_to_range region)
                        | Simple_utils.Location.Virtual _ ->  None)
                    in
                    (file , regions) :: res
                    | None -> res
                end in
              Some (Hashtbl.fold go get_scope_buffers [])
            | None -> None
          end
        | None -> None
      end

    method on_req_rename_ :
       string
    -> Lsp.Types.Position.t
    -> get_scope_info
    -> Lsp.Types.WorkspaceEdit.t Linol_lwt.Jsonrpc2.IO.t =
    fun new_name pos get_scope_info ->
      match self#get_ranges pos get_scope_info with
        | Some ranges ->
          let changes = List.map (fun (file, ranges) -> (file, List.map (fun range -> Lsp.Types.TextEdit.create ~range ~newText:new_name) ranges)) ranges
          in
          Linol_lwt.Jsonrpc2.IO.return @@ Lsp.Types.WorkspaceEdit.create ~changes:changes ()
        | None -> Linol_lwt.Jsonrpc2.IO.return @@ Lsp.Types.WorkspaceEdit.create ()

    method on_req_references_ :
       Lsp.Types.Position.t
    -> get_scope_info
    -> Lsp.Types.Location.t list option Linol_lwt.Jsonrpc2.IO.t =
    fun pos get_scope_info ->
      match self#get_ranges pos get_scope_info with
        | Some ranges ->
          Linol_lwt.Jsonrpc2.IO.return @@ Some (List.flatten @@ List.map (fun (file, ranges) -> List.map (fun range -> Lsp.Types.Location.create ~uri:file ~range) ranges) ranges)
        | None -> Linol_lwt.Jsonrpc2.IO.return None

    method! config_hover = Some (`Bool true)
    method config_formatting = Some (`Bool true)
    method! config_definition = Some (`Bool true)
    method config_rename = Some (`Bool true)
    method config_references = Some (`Bool true)

    method! config_modify_capabilities (c:Lsp.Types.ServerCapabilities.t) : Lsp.Types.ServerCapabilities.t =
      {c with
        hoverProvider = self#config_hover;
        documentFormattingProvider = self#config_formatting;
        definitionProvider = self#config_definition;
        renameProvider = self#config_rename;
        referencesProvider = self#config_references
      }

    method! on_request : type r.
        notify_back:_
      -> id:Linol_lwt.Jsonrpc2.Req_id.t
      -> r Lsp.Client_request.t
      -> r Linol_lwt.Jsonrpc2.IO.t =
    fun ~notify_back ~id (r:_ Lsp.Client_request.t) ->
      begin match r with
        | Lsp.Client_request.TextDocumentFormatting {textDocument; _} ->
          let uri = textDocument.uri in
          let notify_back = new Linol_lwt.Jsonrpc2.notify_back ~uri ~notify_back () in
          self#on_req_formatting_ notify_back uri

        | Lsp.Client_request.TextDocumentDefinition { textDocument; position; _} ->
          let uri = textDocument.uri in
          let notify_back = new Linol_lwt.Jsonrpc2.notify_back ~uri ~notify_back () in
          with_get_scope_info get_scope_buffers uri None
            (self#on_req_definition_ notify_back id uri position)

        | Lsp.Client_request.TextDocumentHover { textDocument; position; _ } ->
          let uri = textDocument.uri in
          (* let notify_back = new Linol_lwt.Jsonrpc2.notify_back ~uri ~notify_back () in *)
          with_get_scope_info get_scope_buffers uri None (self#on_req_hover_ position)

        | Lsp.Client_request.TextDocumentRename {newName; position; textDocument; _} ->
          let uri = textDocument.uri in
          let _notify_back = new Linol_lwt.Jsonrpc2.notify_back ~uri ~notify_back () in
          with_get_scope_info get_scope_buffers uri
            (Lsp.Types.WorkspaceEdit.create ())
            (self#on_req_rename_ newName position)

        | Lsp.Client_request.TextDocumentReferences
          { position : Lsp.Types.Position.t
          ; textDocument : Lsp.Types.TextDocumentIdentifier.t
          ; _
          } ->
            let uri = textDocument.uri in
            (* let notify_back = new Linol_lwt.Jsonrpc2.notify_back ~uri ~notify_back () in *)
            with_get_scope_info get_scope_buffers uri
              None
              (self#on_req_references_ position)

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
