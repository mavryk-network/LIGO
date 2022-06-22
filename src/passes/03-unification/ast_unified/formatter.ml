open Simple_utils.Display

let module_ppformat ~display_format f p =
  match display_format with
  | Human_readable | Dev -> PP.program f p

let module_jsonformat p : json =
  To_yojson.program p

let module_format : 'a format = {
  pp = module_ppformat;
  to_json = module_jsonformat;
}
