open Simple_utils.Display
module Location = Simple_utils.Location
module Snippet = Simple_utils.Snippet
open Ast_unified
open S_exp

let stage = "small_passes"

type t =
  [ `Small_passes_wrong_reduction of string
  | `Small_passes_expected_variable of ty_expr
  ]
[@@deriving poly_constructor { prefix = "small_passes_" }, sexp]

let error_ppformat : display_format:string display_format -> Format.formatter -> t -> unit
  =
 fun ~display_format f a ->
  match display_format with
  (* For unit tests of small passes, we print the sexp of the full value
     because we don't have location. *)
  | Dev -> Format.fprintf f "%a" (Sexp.pp_hum_indent 4) (sexp_of_t a)
  | Human_readable ->
    (match a with
    | `Small_passes_wrong_reduction pass ->
      Format.fprintf f "@[<hv>Pass %s did not reduce.@]" pass
    | `Small_passes_expected_variable t ->
      Format.fprintf f "@[<hv>%a@.Expected a declaration name@]" Snippet.pp (get_t_loc t))


let error_json : t -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  match e with
  | `Small_passes_wrong_reduction pass ->
    let message = Format.asprintf "@[<hv>Pass %s did not reduce.@]" pass in
    let content = make_content ~message () in
    make ~stage ~content
  | `Small_passes_expected_variable t ->
    let message = Format.asprintf "Expected a declaration name." in
    let content = make_content ~message ~location:(get_t_loc t) () in
    make ~stage ~content
