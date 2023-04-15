module Get_scope = Get_scope
open Get_scope
open Utils
open Linol_lwt

type nonrec get_scope_info = get_scope_info

(** To support dirty files, we store some data about files in memory *)
type file_data =
  { syntax : Syntax_types.t
  ; code : string
  ; get_scope_info : get_scope_info
  }

let get_scope : deprecated:bool -> DocumentUri.t -> string -> get_scope_info =
 fun ~deprecated uri source ->
  (* packages - project_root [later] *)
  let file = DocumentUri.to_path uri in
  (* #include - Pass lib or dirs *)
  let dir_name = Filename.dirname file in
  let compiler_options =
    Compiler_options.Raw_options.make
      ~with_types:true
      ~libraries:[ dir_name ]
      ~deprecated
      ()
  in
  unfold_get_scope
  @@ get_scope_trace compiler_options (Raw_input_lsp { file; code = source }) ()


let doc_to_string ~(width : int) (doc : PPrint.document) : string =
  let buffer = Buffer.create 131 in
  PPrint.ToBuffer.pretty 1.0 width buffer doc;
  Buffer.contents buffer


let pretty_print_cst ~(width : int) ~(dialect_cst : dialect_cst) : string =
  let doc =
    match dialect_cst with
    | CameLIGO_cst cst ->
      Parsing.Cameligo.Pretty.print Parsing.Cameligo.Pretty.default_environment cst
    | JsLIGO_cst cst ->
      Parsing.Jsligo.Pretty.print Parsing.Jsligo.Pretty.default_environment cst
    | PascaLIGO_cst cst ->
      Parsing.Pascaligo.Pretty.print Parsing.Pascaligo.Pretty.default_environment cst
  in
  doc_to_string ~width doc
