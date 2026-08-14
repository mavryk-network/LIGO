module Trace = Simple_utils.Trace
open Trace
open Main_errors
open Syntax_types

let file_name_to_variant ~raise sf : t =
  match Filename.split_extension sf with
  | _, Some "mligo" -> CameLIGO
  | _, Some "jsligo" -> JsLIGO
  | _, Some ("ligo" | "pligo") -> PascaLIGO (* MAVRYK: PascaLIGO *)
  | _ -> raise.error (main_invalid_extension sf)


let of_ext_opt = function
  | Some "mligo" -> Some CameLIGO
  | Some "jsligo" -> Some JsLIGO
  | Some ("ligo" | "pligo") -> Some PascaLIGO (* MAVRYK: PascaLIGO *)
  | _ -> None


let of_string_opt ~raise (Syntax_name syntax) source =
  match syntax, source with
  | "auto", Some sf -> file_name_to_variant ~raise sf
  | ("cameligo" | "CameLIGO"), _ -> CameLIGO
  | ("jsligo" | "JsLIGO"), _ -> JsLIGO
  | ("pascaligo" | "PascaLIGO"), _ -> PascaLIGO (* MAVRYK: PascaLIGO *)
  | _ -> raise.error (main_invalid_syntax_name syntax)


let to_string = function
  | CameLIGO -> "cameligo"
  | JsLIGO -> "jsligo"
  | PascaLIGO -> "pascaligo" (* MAVRYK: PascaLIGO *)


let to_ext = function
  | CameLIGO -> ".mligo"
  | JsLIGO -> ".jsligo"
  | PascaLIGO -> ".ligo" (* MAVRYK: PascaLIGO *)


let is_cameligo = Fn.flip Filename.check_suffix ".mligo"
let is_jsligo = Fn.flip Filename.check_suffix ".jsligo"

(* MAVRYK: PascaLIGO. [".ligo"] is not a suffix of ["*.jsligo"], so this does not
   misclassify JsLIGO files. *)
let is_pascaligo file_path =
  Filename.check_suffix file_path ".ligo"
  || Filename.check_suffix file_path ".pligo"


let is_ligo file_path =
  is_cameligo file_path || is_jsligo file_path || is_pascaligo file_path


let cameligo_glob = "**/*.mligo"
let jsligo_glob = "**/*.jsligo"
let pascaligo_glob = "**/*.ligo" (* MAVRYK: PascaLIGO *)
