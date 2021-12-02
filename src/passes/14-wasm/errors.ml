open Simple_utils.Display

type wasm_error = [
  | `Wasm_pass_michelson_insertion of Location.t
  ] [@@deriving poly_constructor { prefix = "wasm_pass_" }]

let stage = "wasm_pass"

let error_ppformat : display_format:string display_format ->
  Format.formatter -> wasm_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Wasm_pass_michelson_insertion l ->
      ignore l;
      let s = Format.asprintf "Michelson insertion can't be used when targeting WebAssembly." in
      Format.pp_print_string f s
  )

let error_jsonformat : wasm_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Wasm_pass_michelson_insertion l ->
    ignore l;
    let msg = Format.asprintf "impossible michelson insertion"in
    let content = `Assoc [
      ("message", `String msg); ]
    in
    json_error ~stage ~content