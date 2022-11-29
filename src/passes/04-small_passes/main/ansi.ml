type style =
  | Normal
  | Bold
  | Underline
  | Red
  | Green

let style_of_stag = function
  | Format.String_tag s ->
    (match s with
    | "normal" -> Normal
    | "bold" -> Bold
    | "underline" -> Underline
    | "red" -> Red
    | "green" -> Green
    | _ -> failwith "Unknown ANSI style" (* TODO NP : How to report errors properly ? *))
  | _ ->
    failwith "Unknown ANSI semantic tag" (* TODO NP : How to report errors properly ? *)


let closing_style = function
  | Red | Green | Bold | Underline | Normal -> Normal


let code_of_style = function
  | Normal -> 0
  | Bold -> 1
  | Underline -> 4
  | Red -> 31
  | Green -> 32


let ansi_of_code code = Format.sprintf "\027[%dm" code
let ansi_of_style style = ansi_of_code @@ code_of_style style
let mark_open_stag t = ansi_of_style @@ style_of_stag t
let mark_close_stag t = ansi_of_style @@ closing_style @@ style_of_stag t

let add_ansi_marking ppf =
  let open Format in
  pp_set_mark_tags ppf true;
  let old_fs = pp_get_formatter_stag_functions ppf () in
  pp_set_formatter_stag_functions ppf { old_fs with mark_open_stag; mark_close_stag }
