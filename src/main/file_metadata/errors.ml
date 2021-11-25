open Display

type t = 
[
  | `Meta_invalid_extension of string
  | `Meta_invalid_syntax_name of string
  | `Meta_invalid_dialect_name of string
]

let syntax_auto_detection extension : t = `Meta_invalid_extension extension
let invalid_syntax syntax : t = `Meta_invalid_syntax_name syntax
let invalid_dialect dialect : t = `Meta_invalid_dialect_name dialect

let error_ppformat : display_format:string display_format ->
  Format.formatter -> t -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Meta_invalid_extension extension ->
      Format.fprintf f
        "@[<hv>Invalid file extension '%s'. @.Use '.ligo' for PascaLIGO, '.mligo' for CameLIGO, '.religo' for ReasonLIGO, or the --syntax option.@]"
        extension
    | `Meta_invalid_syntax_name syntax ->
      Format.fprintf f
        "@[<hv>Invalid syntax option: '%s'. @.Use 'pascaligo', 'cameligo', or 'reasonligo'. @]"
          syntax
    | `Meta_invalid_dialect_name syntax ->
      Format.fprintf f
        "@[<hv>Invalid dialect option: '%s'. @.Use 'verbose' or 'terse'. @]"
          syntax
  )


let error_jsonformat : t -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Meta_invalid_extension _ ->
    json_error ~stage:"command line interpreter" ~content:(`String "bad file extension")
  | `Meta_invalid_syntax_name _ ->
    json_error ~stage:"command line interpreter" ~content:(`String "bad syntax name")
  | `Meta_invalid_dialect_name _ ->
    json_error ~stage:"command line interpreter" ~content:(`String "bad dialect name")