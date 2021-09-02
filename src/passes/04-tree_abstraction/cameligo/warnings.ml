open Simple_utils.Display

let stage = "abstracter"

type abs_warning = [
  `Deprecated_constant_used of string * Location.t
]

let deprecated_constant_used s loc : abs_warning = `Deprecated_constant_used (s, loc)

let warning_ppformat :
  display_format:string display_format ->
  Format.formatter ->
  abs_warning ->
  unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Deprecated_constant_used (s,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Warning: The use of %S has been deprecated.@]"
        Snippet.pp loc
        s
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
  | `Deprecated_constant_used (s,loc) ->
      mk_warning s loc "Deprecated constant used."
