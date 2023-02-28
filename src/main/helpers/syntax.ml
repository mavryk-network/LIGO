module Trace = Simple_utils.Trace
open Trace
open Main_errors
open Syntax_types

let file_extension_to_variant ~raise ~deprecated sf : t option =
  ignore raise;
  match sf with
  | ".mligo" -> Some CameLIGO
  | ".jsligo" -> Some JsLIGO
  | (".ligo" | ".pligo") when deprecated -> Some PascaLIGO
  | _ -> None


let of_ext_opt ~deprecated = function
  | None -> None
  | Some ("mligo" | ".mligo") -> Some CameLIGO
  | Some ("jsligo" | ".jsligo") -> Some JsLIGO
  | Some ("ligo" | ".ligo" | "pligo" | ".pligo") when deprecated -> Some PascaLIGO
  | Some _ -> None


let of_string_opt ~raise ~deprecated (Syntax_name syntax) source =
  match syntax, source with
  | "auto", Some sf ->
    let ext = Caml.Filename.extension sf in
    trace_option
      ~raise
      (main_invalid_extension ext)
      (file_extension_to_variant ~deprecated ~raise ext)
  | ("cameligo" | "CameLIGO"), _ -> CameLIGO
  | ("jsligo" | "JsLIGO"), _ -> JsLIGO
  | ("pascaligo" | "PascaLIGO"), _ when deprecated -> PascaLIGO
  | _ -> raise.error (main_invalid_syntax_name syntax)


let to_string = function
  | CameLIGO -> "cameligo"
  | JsLIGO -> "jsligo"
  | PascaLIGO -> "pascaligo"


let to_ext = function
  | CameLIGO -> ".mligo"
  | JsLIGO -> ".jsligo"
  | PascaLIGO -> ".ligo"
