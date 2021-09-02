open Simple_utils.Display

let stage = "abstracter"

type abs_warning = [
  `Deprecated_constant_used of string * Location.t * string
]

let deprecated_constant_used ~deprecated ~latest loc  : abs_warning = `Deprecated_constant_used (deprecated, loc, latest)

let warning_ppformat :
  display_format:string display_format ->
  Format.formatter ->
  abs_warning ->
  unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Deprecated_constant_used (deprecated,loc,latest) ->
      Format.fprintf f
        "@[<hv>%a@.Warning: The use of %S has been deprecated user %S instead.@]"
        Snippet.pp loc
        deprecated
        latest
  )

let mk_warning (s: string) (loc: Location.t) (msg: string) =
  let content =
    `Assoc [("message",  `String msg);
            ("variable", `String s);
            ("location", Location.to_yojson loc)]
  in `Assoc [("status",  `String "error");
             ("stage",   `String stage);
             ("content",  content )]

let warning_jsonformat : abs_warning -> Yojson.Safe.t =
  function
  | `Deprecated_constant_used (s,loc,_) ->
      mk_warning s loc "Deprecated constant used."
