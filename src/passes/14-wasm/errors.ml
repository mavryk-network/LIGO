open Simple_utils.Display

type wasm_error =
  [ `Wasm_pass_michelson_insertion of Location.t
  | `Wasm_pass_invalid_utf8 of Location.t
  | `Wasm_pass_not_supported of
    Mini_c.Types.expression
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
[@@deriving poly_constructor { prefix = "wasm_pass_" }]

let stage = "wasm_pass"

let error_ppformat
    : display_format:string display_format -> Format.formatter -> wasm_error -> unit
  =
 fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Wasm_pass_michelson_insertion l ->
      Format.fprintf
        f
        "@[<v>%a@.Michelson insertion can't be used when targeting WebAssembly. @]"
        Snippet.pp
        l
    | `Wasm_pass_invalid_utf8 l ->
      Format.fprintf f "@[<v>%a@.Invalid utf8. @]" Snippet.pp l
    | `Wasm_pass_not_supported e ->
      Format.fprintf
        f
        "@[<v>%a@.Not supported/implemented at the moment. @]"
        Mini_c.PP.expression
        e)


let error_json : wasm_error -> Simple_utils.Error.t =
 fun a ->
  let open Simple_utils.Error in
  match a with
  | `Wasm_pass_michelson_insertion location ->
    let message = Format.asprintf "impossible michelson insertion" in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Wasm_pass_invalid_utf8 location ->
    let message = Format.asprintf "invalid utf8" in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Wasm_pass_not_supported e ->
    let message = Format.asprintf "not supported %a" Mini_c.PP.expression e in
    let content = make_content ~message () in
    make ~stage ~content
