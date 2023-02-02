open Simple_utils.Display
open Types

let program_ppformat ~display_format ~no_colour f p =
  (* The [no_colour] option is provided to all [_ppformat] functions by default,
     but not needed by all of them. Remove the [ignore] if you need it. *)
  let () = ignore no_colour in
  match display_format with
  | Human_readable | Dev -> Format.fprintf f "%a" Sexp.pp (S_exp.sexp_of_program p)


let program_jsonformat p : json = `String (Format.asprintf "%a" Sexp.pp (S_exp.sexp_of_program p))
let program_format : 'a format = { pp = program_ppformat; to_json = program_jsonformat }
