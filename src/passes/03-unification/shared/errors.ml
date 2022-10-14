
open Simple_utils.Display

module Snippet  = Simple_utils.Snippet
module Location = Simple_utils.Location
module Region   = Simple_utils.Region

let stage = "unification"

type unification_error = [
  | `Unification_unsupported_syntax of string
  | `Unification_other_error of string
] [@@deriving poly_constructor {prefix = "unification_" }]

let error_ppformat
  : display_format:string display_format
  -> Format.formatter
  -> unification_error
  -> unit
  = fun ~display_format f err ->
    match display_format with
    | Human_readable | Dev -> (
      match err with
      | `Unification_unsupported_syntax msg -> Format.fprintf f "@[<hv>Syntax %s is not supported by the syntax unification stage.@]" msg
      | `Unification_other_error msg -> Format.fprintf f "@[<hv>Error during unification stage: %s.@]" msg
      )


let error_jsonformat : unification_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~message =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("message",  `String message )]
  in
  match a with
  | `Unification_unsupported_syntax s ->
    json_error ~stage ~message:(sprintf "Syntax %s is not supported by the syntax unification stage." s)
  | `Unification_other_error message ->
    json_error ~stage ~message