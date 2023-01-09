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
  | `Small_passes_michelson_type_wrong_arity of string * ty_expr
  | `Small_passes_michelson_type_wrong of string * ty_expr
  | `Small_passes_wrong_lvalue of expr
  | `Small_passes_statement_after_break of statement list
  ]
[@@deriving poly_constructor { prefix = "small_passes_" }, sexp]

let error_ppformat : display_format:string display_format -> no_colour:bool -> Format.formatter -> t -> unit
  =
 fun ~display_format ~no_colour f a ->
  let snippet_pp = Snippet.pp ~no_colour in
  match display_format with
  (* For unit tests of small passes, we print the sexp of the full value
     because we don't have location. *)
  | Dev -> Format.fprintf f "%a" (Sexp.pp_hum_indent 4) (sexp_of_t a)
  | Human_readable ->
    (match a with
    | `Small_passes_wrong_reduction pass ->
      Format.fprintf f "@[<hv>Pass %s did not reduce.@]" pass
    | `Small_passes_expected_variable t ->
      Format.fprintf f "@[<hv>%a@.Expected a declaration name@]" snippet_pp (get_t_loc t)
    | `Small_passes_array_rest_not_supported e ->
      Format.fprintf
        f
        "@[<hv>%a@.Rest property not supported here.@]"
        snippet_pp
        (get_e_loc e)
    | `Small_passes_invalid_case e ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid field value. An anonymous arrow function was expected, eg. \
         `None: () => foo`.@]"
        snippet_pp
        (get_e_loc e)
    | `Small_passes_unsupported_match_object_property e ->
      Format.fprintf
        f
        "@[<hv>%a@.Unsupported pattern match object property.@]"
        snippet_pp
        (get_e_loc e)
    | `Small_passes_invalid_list_pattern_match loc ->
      Format.fprintf f "@[<hv>%a@.Invalid list pattern matching.@]" snippet_pp loc
    | `Small_passes_michelson_type_wrong_arity (name, t) ->
      Format.fprintf
        f
        "[@<hv>%a@.Invalid \"%s\" type.@.An even number of 2 or more arguments is \
         expected, where each odd item is a type annotated by the following string.@]"
        snippet_pp
        (get_t_loc t)
        name
    | `Small_passes_michelson_type_wrong (name, t) ->
      Format.fprintf
        f
        "[@<hv>%a@.Invalid \"%s\" type.@.At this point, an annotation, in the form of a \
         string, is expected for the preceding type.@]"
        snippet_pp
        (get_t_loc t)
        name
    | `Small_passes_wrong_lvalue e ->
      Format.fprintf
        f
        "@[<hv>%a@.Expected a field name or an accessor@]"
        snippet_pp
        (get_e_loc e)
    | `Small_passes_statement_after_break slst ->
      let loc =
        List.fold slst ~init:Location.generated ~f:(fun acc el ->
            Location.cover acc (get_s_loc el))
      in
      Format.fprintf f "@[<hv>%a@.Illegal statements after break@]" snippet_pp loc)


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
      "Invalid field value. An anonymous arrow function was expected, eg. `None: () => \
       foo`."
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
  | `Small_passes_michelson_type_wrong_arity (name, t) ->
    let message =
      Format.sprintf
        "Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where \
         each odd item is a type annotated by the following string."
        name
    in
    let location = get_t_loc t in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_michelson_type_wrong (name, t) ->
    let message =
      Format.sprintf
        "Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is \
         expected for the preceding type."
        name
    in
    let location = get_t_loc t in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_wrong_lvalue e ->
    let message = "Expected a field name or an accessor" in
    let location = get_e_loc e in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Small_passes_statement_after_break slst ->
    let location =
      List.fold slst ~init:Location.generated ~f:(fun acc el ->
          Location.cover acc (get_s_loc el))
    in
    let message = "Illegal statements after break" in
    let content = make_content ~message ~location () in
    make ~stage ~content