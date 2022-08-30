open Simple_utils.Display

type wasm_error = [
| `Wasm_pass_michelson_insertion of Location.t
| `Wasm_pass_invalid_utf8 of Location.t
| `Wasm_pass_not_supported of Mini_c.Types.expression

(* | `Wasm_pass_not_implemented of string * Location.T
| `Wasm_pass_invalid_utf8 of string
| `Wasm_pass_local_type of string
| `Wasm_pass_import of string
| `Wasm_pass_import of string
| `Wasm_pass_operand_stack_unexpected_type of string
| `Wasm_pass_operand_stack_empty of Location.t
| `Wasm_pass_unknown_function of string * Location.t
| `Wasm_pass_unknown_type of string * Location.t *)
]

[@@deriving poly_constructor {prefix = "wasm_pass_"}]

let stage = "wasm_pass"

let error_ppformat :
    display_format:string display_format ->
    Format.formatter ->
    wasm_error ->
    unit =
 fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Wasm_pass_michelson_insertion l ->
      Format.fprintf f
        "@[<v>%a@.Michelson insertion can't be used when targeting WebAssembly. @]"
        Snippet.pp l
    | `Wasm_pass_invalid_utf8 l ->
      Format.fprintf f
        "@[<v>%a@.Invalid utf8. @]"
        Snippet.pp l
    | `Wasm_pass_not_supported e ->
      Format.fprintf f
        "@[<v>%a@.Not supported/implemented at the moment. @]" 
        Mini_c.PP.expression e
    )

let error_jsonformat : wasm_error -> Yojson.Safe.t =
 fun a ->
  let json_error ~stage ~content =
    `Assoc
      [
        ("status", `String "error");
        ("stage", `String stage);
        ("content", content);
      ]
  in
  match a with
  | `Wasm_pass_michelson_insertion loc ->
    let message = `String (Format.asprintf "impossible michelson insertion") in
    let loc = Location.to_yojson loc in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_error ~stage ~content
  | `Wasm_pass_invalid_utf8 loc ->
    let message = `String (Format.asprintf "impossible michelson insertion") in
    let loc = Location.to_yojson loc in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_error ~stage ~content
  | `Wasm_pass_not_supported e ->
    let message = `String (Format.asprintf "not supported") in
    let expression = `String (Format.asprintf "%a" Mini_c.PP.expression e) in
    let content = `Assoc [
      ("message", message);
      ("expression", expression);
    ] in
    json_error ~stage ~content
