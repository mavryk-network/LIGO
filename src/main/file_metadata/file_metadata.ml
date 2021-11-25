open Trace
module Errors = Errors
open Errors

type s_syntax = Syntax_name of string
type s_dialect = Dialect_name of string

type dialect = Terse | Verbose
type v_syntax =
  | PascaLIGO of dialect option
  | CameLIGO
  | ReasonLIGO
  | JsLIGO

type t = {
  syntax : v_syntax;
  curry  : bool;
}

let dialect_to_variant ~raise dialect =
  match dialect with
  | None -> None
  | Some (Dialect_name dialect) ->
     match dialect with
     | "terse" -> (Some Terse)
     | "verbose" -> (Some Verbose)
     | _ -> raise.raise (invalid_dialect dialect)


let file_extension_to_variant ~raise ?dialect sf : v_syntax =
  match sf with
  | ".ligo" | ".pligo" -> 
    let dialect = dialect_to_variant ~raise dialect in
    (PascaLIGO dialect)
  | ".mligo"           -> CameLIGO
  | ".religo"          -> ReasonLIGO
  | ".jsligo"          -> JsLIGO
  | ext                -> raise.raise (syntax_auto_detection ext)

let syntax_to_variant ~raise ?dialect (Syntax_name syntax) source =
  match syntax, source with
  | "auto", Some sf ->
    let sf = Filename.extension sf in
      file_extension_to_variant ~raise ?dialect sf
  | ("pascaligo" | "PascaLIGO"),   _ -> 
     let dialect = dialect_to_variant ~raise dialect in
     (PascaLIGO dialect)
  | ("cameligo" | "CameLIGO"),     _ -> CameLIGO
  | ("reasonligo" | "ReasonLIGO"), _ -> ReasonLIGO
  | ("jsligo" | "JsLIGO"),         _ -> JsLIGO
  | _ -> raise.raise (invalid_syntax syntax)

let variant_to_syntax (v: v_syntax) =
  match v with
  | PascaLIGO _ -> "pascaligo"
  | CameLIGO -> "cameligo"
  | ReasonLIGO -> "reasonligo"
  | JsLIGO -> "jsligo"

let is_syntax_curried (v : v_syntax) =
  match v with
  | PascaLIGO _  -> false
  | CameLIGO   -> true
  | ReasonLIGO -> false
  | JsLIGO     -> false
(* we should have on for filename with syntax_opt and one in case of no file *)
let make  ~raise syntax file_name_opt : t =
  let syntax   = syntax_to_variant ~raise (Syntax_name syntax) file_name_opt in
  let curry    = is_syntax_curried syntax in
  {syntax;curry}

let extract ~raise syntax file_name : t =
  make ~raise syntax (Some file_name)

let make_from_syntax (syntax : v_syntax) : t =
  let curry    = is_syntax_curried syntax in
  {syntax;curry}