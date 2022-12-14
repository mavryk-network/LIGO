open Simple_utils.Display
module Snippet = Simple_utils.Snippet
module Location = Simple_utils.Location
module Region = Simple_utils.Region

let stage = "unification"

type t =
  [ `Unification_unsupported_syntax of string
  | `Unification_other_error of string
  ]
[@@deriving poly_constructor { prefix = "unification_" }]

let error_ppformat
    :  display_format:string display_format -> Format.formatter -> t
    -> unit
  =
 fun ~display_format f err ->
  match display_format with
  | Human_readable | Dev ->
    (match err with
    | `Unification_unsupported_syntax msg ->
      Format.fprintf
        f
        "@[<hv>Syntax %s is not supported by the syntax unification stage.@]"
        msg
    | `Unification_other_error msg ->
      Format.fprintf f "@[<hv>Error during unification stage: %s.@]" msg)


let error_json : t -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  match e with
  | `Unification_unsupported_syntax s ->
    let message =
      Format.sprintf "Syntax %s is not supported by the syntax unification stage." s
    in
    let content = make_content ~message () in
    make ~stage ~content
  | `Unification_other_error s ->
    let message = Format.sprintf "Error during unification stage : %s" s in
    let content = make_content ~message () in
    make ~stage ~content
