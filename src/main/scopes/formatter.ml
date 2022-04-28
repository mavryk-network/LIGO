open Simple_utils.Display

let scope_ppformat ~display_format f (d,s) =
  match display_format with
  | Human_readable ->
    Format.fprintf f "there is to human-readable pretty printer for you, use --format=json"
  | Dev -> Format.fprintf f "@[<v>%a@ %a@]" PP.scopes s PP.definitions d

let scope_jsonformat defscopes : json = PP.to_json defscopes

let scope_format : 'a format = {
  pp = scope_ppformat;
  to_json = scope_jsonformat;
}

type get_scope_output =
  { errors : Main_errors.all list;
    warns  : Main_warnings.all list;
    info  : (Types.def_map * Types.scopes) option;
  }

let error_format = Main_errors.Formatter.error_format
let warn_format  = Main_warnings.format

let pp_get_scope_output: get_scope_output pp =
  fun ~display_format f {errors; warns; info } ->
  begin match info with
  | Some info -> scope_ppformat ~display_format f info
  | None -> ()
  end;
  List.iter errors ~f:(fun err ->
    error_format.pp ~display_format f err;
    Format.fprintf f "\n");
  List.iter warns ~f:(fun warn ->
    warn_format.pp ~display_format f warn;
    Format.fprintf f "\n")

let json_list (list : 'a list) (format : 'a format) =
  `List (List.map list
             ~f:(fun value -> convert ~display_format:Json (Displayable {value; format})))

let get_scope_output_to_json: get_scope_output -> json =
  fun {errors; warns; info} ->
  let content = [
    ("errors",      json_list errors error_format);
    ("warnings",    json_list warns warn_format); ] in
  let info_json =
    match info with
    | Some (d, s) ->
       [("definitions", (PP.defs_json d));
        ("scopes",      (PP.scopes_json s)); ]
    | None -> []
  in `Assoc (content @ info_json)

let get_scope_format : get_scope_output format =
  { pp      = pp_get_scope_output;
    to_json = get_scope_output_to_json; }
