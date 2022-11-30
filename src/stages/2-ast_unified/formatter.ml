open Simple_utils.Display

let module_ppformat ~display_format f p =
  match display_format with
  | Human_readable | Dev -> PP.program f p

let f_todo _ = failwith "TODO"
let module_jsonformat p : json = Types.program__to_yojson f_todo f_todo f_todo p
let module_format : 'a format = { pp = module_ppformat; to_json = module_jsonformat }
