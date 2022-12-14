open Simple_utils.Display
module Location = Simple_utils.Location
module Snippet = Simple_utils.Snippet
open Ast_unified
open S_exp

let stage = "small_passes"

type t =
  [ `Small_passes_wrong_reduction of string
  | `Small_passes_expected_variable of ty_expr
  | `Small_passes_array_rest_not_supported of expr
  | `Small_passes_invalid_case of expr
  | `Small_passes_unsupported_match_object_property of expr
  | `Small_passes_invalid_list_pattern_match of Location.t
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
      Format.fprintf f "@[<hv>%a@.Expected a declaration name@]" Snippet.pp (get_t_loc t)
    | `Small_passes_array_rest_not_supported e ->
      Format.fprintf
        f
        "@[<hv>%a@.Rest property not supported here.@]"
        Snippet.pp (get_e_loc e)
    | `Small_passes_invalid_case e ->
      Format.fprintf
      f
      "@[<hv>%a@.Invalid field value. An anonymous arrow function was expected, \
       eg. `None: () => foo`.@]"
      Snippet.pp (get_e_loc e)
    | `Small_passes_unsupported_match_object_property e ->
      Format.fprintf
      f
      "@[<hv>%a@.Unsupported pattern match object property.@]"
      Snippet.pp (get_e_loc e)
    | `Small_passes_invalid_list_pattern_match loc ->
      Format.fprintf
      f
      "@[<hv>%a@.Invalid list pattern matching.@]"
      Snippet.pp loc
      )


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
  | `Small_passes_array_rest_not_supported e ->
    let message = Format.asprintf "Rest property not supported here." in
    let content = make_content ~message ~location:(get_e_loc e) () in
    make ~stage ~content
  | `Small_passes_invalid_case e ->
    let message =
        "Invalid field value. An anonymous arrow function was expected, eg. `None: \
          () => foo`."
    in
    let location = get_e_loc e in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_unsupported_match_object_property e ->
    let message = "Unsupported pattern match object property" in
    let location = get_e_loc e in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_invalid_list_pattern_match loc ->
    let message = "Invalid list pattern matching" in
    let content = make_content ~message ~location:loc () in
    make ~stage ~content
