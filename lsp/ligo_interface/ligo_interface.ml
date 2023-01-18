open Lsp.Types

type get_scope_info =
  Main_errors.all list * Main_warnings.all list * (Scopes.def list * Scopes.scopes) option

let get_scope : DocumentUri.t -> string -> get_scope_info =
 fun uri source ->
  (* packages - project_root [later] *)
  let file_path = DocumentUri.to_path uri in
  (* #include - Pass lib or dirs *)
  let dir_name = Filename.dirname file_path in
  let compiler_options = Compiler_options.Raw_options.make ~libraries:[ dir_name ] () in
  Ligo_api.Info.get_scope_trace
    compiler_options
    (Raw { id = file_path; code = source })
    ()


let formatting : DocumentUri.t -> (string * string, string * string) result =
 fun uri ->
  let compiler_options = Compiler_options.Raw_options.make () in
  let file_path = DocumentUri.to_path uri in
  let display_format = Simple_utils.Display.human_readable in
  Ligo_api.Print.pretty_print compiler_options file_path display_format ()
