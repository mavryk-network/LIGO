open Simple_utils.Display
open Cst.Cameligo

let stage = "self_cst_cameligo"

type self_cst_cameligo_warning = [
  `Shadow_reserved_name of variable
]

let shadow_reserved_name var : self_cst_cameligo_warning = `Shadow_reserved_name var

let error_ppformat :
  display_format:string display_format ->
  Format.formatter ->
  self_cst_cameligo_warning ->
  unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Shadow_reserved_name var ->
      Format.fprintf f
        "@[<hv>%a@.Reserved name shadowed %S by this binding.@]"
        Snippet.pp_lift var.region
        var.value
  )

let mk_error (var: string Region.reg) (msg: string) =
  let loc = Location.lift @@ var.region in
  let content =
    `Assoc [("message",  `String msg);
            ("variable", `String var.value);
            ("location", Location.to_yojson loc)]
  in `Assoc [("status",  `String "error");
             ("stage",   `String stage);
             ("content",  content )]

let error_jsonformat : self_cst_cameligo_warning -> Yojson.Safe.t =
  function
  | `Shadow_reserved_name var ->
       mk_error var "Reserved name shadowed by this binding."
