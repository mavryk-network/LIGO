open Simple_utils.Display

let stage = "small_passes"

type t = [ `Small_passes_wrong_reduction of string * string ]
[@@deriving poly_constructor { prefix = "small_passes_" }]

let error_ppformat
    : display_format:string display_format -> Format.formatter -> t -> unit
  =
 fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Small_passes_wrong_reduction (pass,msg) -> Format.fprintf f "@[<hv>Pass %s did not reduce: %s @]" pass msg)


let error_json : t -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  match e with
  | `Small_passes_wrong_reduction (pass,msg) ->
    let message = Format.asprintf "@[<hv>Pass %s did not reduce: %s @]" pass msg in
    let content = make_content ~message () in
    make ~stage ~content
