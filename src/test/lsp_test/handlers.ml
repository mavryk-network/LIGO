module Requests = Ligo_api.Lsp_server.Requests.Make (struct
  module Info = Ligo_api.Info
  module Print = Ligo_api.Print
end)

open Linol_lwt
open Requests.Handler
open Utils

let test_run_session (session : 'a Handler.t) : 'a * Jsonrpc2.Diagnostic.t list =
  let mocked_notify_back = ref [] in
  let result =
    run_handler
      { notify_back = Mock mocked_notify_back
      ; debug = false
      ; docs_cache = Hashtbl.create 32
      }
      session
  in
  Lwt_main.run result, !mocked_notify_back


(** File path is expected to be absolute *)
let open_file (file_path : string) : DocumentUri.t Handler.t =
  let uri = DocumentUri.of_path file_path in
  let@ () = Requests.on_doc uri (In_channel.read_all file_path) in
  return uri


let to_absolute : string -> string = Filename.concat @@ Ligo_unix.getcwd ()
let rel_path_to_uri : string -> DocumentUri.t = DocumentUri.of_path @. to_absolute
