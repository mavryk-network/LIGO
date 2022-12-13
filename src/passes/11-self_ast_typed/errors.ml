module Snippet = Simple_utils.Snippet
module Location = Simple_utils.Location
module PP = Ast_typed.PP
open Simple_utils.Display
open Ligo_prim

let stage = "self_ast_typed"

type self_ast_typed_error =
  [ `Self_ast_typed_recursive_call_is_only_allowed_as_the_last_operation of
    Value_var.t * Location.t
  | `Self_ast_typed_bad_self_type of
    Ast_typed.type_expression * Ast_typed.type_expression * Location.t
  | `Self_ast_typed_bad_format_entrypoint_ann of string * Location.t
  | `Self_ast_typed_entrypoint_ann_not_literal of Location.t
    [@name "entrypoint_annotation_not_literal"]
  | `Self_ast_typed_unmatched_entrypoint of Location.t
  | `Self_ast_typed_nested_bigmap of Location.t
  | `Self_ast_typed_corner_case of string
  | `Self_ast_typed_bad_contract_io of Value_var.t * Ast_typed.expression * Location.t
  | `Self_ast_typed_bad_view_io of Value_var.t * Location.t
  | `Self_ast_typed_expected_list_operation of
    Value_var.t * Ast_typed.type_expression * Ast_typed.expression
  | `Self_ast_typed_expected_same_entry of
    Value_var.t
    * Ast_typed.type_expression
    * Ast_typed.type_expression
    * Ast_typed.expression
  | `Self_ast_typed_expected_non_vars_in_storage of
    Value_var.t * Ast_typed.type_expression * Ast_typed.expression
  | `Self_ast_typed_expected_non_vars_in_parameter of
    Value_var.t * Ast_typed.type_expression * Ast_typed.expression
  | `Self_ast_typed_expected_pair_in of Location.t * [ `View | `Contract ]
  | `Self_ast_typed_expected_pair_out of Location.t
  | `Self_ast_typed_storage_view_contract of
    Location.t
    * Value_var.t
    * Value_var.t
    * Ast_typed.type_expression
    * Ast_typed.type_expression
  | `Self_ast_typed_view_io of Location.t * Ast_typed.type_expression * [ `In | `Out ]
  | `Self_ast_typed_annotated_declaration_shadowed of Location.t
  ]
[@@deriving poly_constructor { prefix = "self_ast_typed_" }]

let expected_pair_in_contract loc = expected_pair_in loc `Contract
let expected_pair_in_view loc = expected_pair_in loc `View
let type_view_io_in loc got = view_io loc got `In
let type_view_io_out loc got = view_io loc got `Out

let error_ppformat
    :  display_format:string display_format -> Format.formatter -> self_ast_typed_error
    -> unit
  =
 fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev ->
    (match a with
    | `Self_ast_typed_annotated_declaration_shadowed loc ->
      Format.fprintf
        f
        "@[<hv>%a@.This declaration holds an annotation and is later shadowed.@]"
        Snippet.pp
        loc
    | `Self_ast_typed_storage_view_contract (loc, main_name, view_name, ct, vt) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid view argument.@.View '%a' has storage type '%a' and contract \
         '%a' has storage type '%a'.@]"
        Snippet.pp
        loc
        Value_var.pp
        view_name
        Ast_typed.PP.type_expression
        vt
        Value_var.pp
        main_name
        Ast_typed.PP.type_expression
        ct
    | `Self_ast_typed_view_io (loc, got, arg) ->
      let s =
        match arg with
        | `In -> "input"
        | `Out -> "output"
      in
      Format.fprintf
        f
        "@[<hv>%a@.Invalid view.@.Type '%a' is forbidden as %s argument.@]"
        Snippet.pp
        loc
        Ast_typed.PP.type_expression
        got
        s
    | `Self_ast_typed_recursive_call_is_only_allowed_as_the_last_operation (_name, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Recursive call not in tail position. @.The value of a recursive call \
         must be immediately returned by the defined function. @]"
        Snippet.pp
        loc
    | `Self_ast_typed_bad_self_type (expected, got, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type annotation.@.\"%a\" was given, but \"%a\" was \
         expected.@.Note that \"Tezos.self\" refers to this contract, so the parameters \
         should be the same. @]"
        Snippet.pp
        loc
        Ast_typed.PP.type_expression
        got
        Ast_typed.PP.type_expression
        expected
    | `Self_ast_typed_bad_format_entrypoint_ann (ep, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid entrypoint \"%s\". One of the following patterns is \
         expected:@.* \"%%bar\" is expected for entrypoint \"Bar\"@.* \"%%default\" when \
         no entrypoint is used."
        Snippet.pp
        loc
        ep
    | `Self_ast_typed_entrypoint_ann_not_literal loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid entrypoint value.@.The entrypoint value must be a string \
         literal. @]"
        Snippet.pp
        loc
    | `Self_ast_typed_unmatched_entrypoint loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid entrypoint value.@.The entrypoint value does not match a \
         constructor of the contract parameter. @]"
        Snippet.pp
        loc
    | `Self_ast_typed_nested_bigmap loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid big map nesting.@.A big map cannot be nested inside another \
         big map. @]"
        Snippet.pp
        loc
    | `Self_ast_typed_corner_case desc ->
      Format.fprintf f "@[<hv>Internal error: %s @]" desc
    | `Self_ast_typed_bad_contract_io (entrypoint, _, location) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type for entrypoint \"%a\".@.An entrypoint must of type \
         \"parameter * storage -> operation list * storage\". @]"
        Snippet.pp
        location
        Value_var.pp
        entrypoint
    | `Self_ast_typed_bad_view_io (entrypoint, loc) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type for view \"%a\".@.An view must be a function. @]"
        Snippet.pp
        loc
        Value_var.pp
        entrypoint
    | `Self_ast_typed_expected_list_operation (entrypoint, got, e) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type for entrypoint \"%a\".@.An entrypoint must of type \
         \"parameter * storage -> operation list * storage\".@.We expected a list of \
         operations but we got %a@]"
        Snippet.pp
        e.location
        Value_var.pp
        entrypoint
        Ast_typed.PP.type_expression
        got
    | `Self_ast_typed_expected_same_entry (entrypoint, t1, t2, e) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type for entrypoint \"%a\".@.The storage type \"%a\" of the \
         function parameter must be the same as the storage type \"%a\" of the return \
         value.@]"
        Snippet.pp
        e.location
        Value_var.pp
        entrypoint
        Ast_typed.PP.type_expression
        t1
        Ast_typed.PP.type_expression
        t2
    | `Self_ast_typed_expected_non_vars_in_storage (entrypoint, t, e) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type for entrypoint \"%a\".@.The storage type \"%a\" of the \
         entrypoint function must not contain polymorphic variables.@]"
        Snippet.pp
        e.location
        Value_var.pp
        entrypoint
        Ast_typed.PP.type_expression
        t
    | `Self_ast_typed_expected_non_vars_in_parameter (entrypoint, t, e) ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid type for entrypoint \"%a\".@.The parameter type \"%a\" of the \
         entrypoint function must not contain polymorphic variables.@]"
        Snippet.pp
        e.location
        Value_var.pp
        entrypoint
        Ast_typed.PP.type_expression
        t
    | `Self_ast_typed_expected_pair_in (loc, t) ->
      let ep =
        match t with
        | `View -> "view"
        | `Contract -> "contract"
      in
      Format.fprintf
        f
        "@[<hv>%a@.Invalid %s.@.Expected a tuple as argument.@]"
        Snippet.pp
        loc
        ep
    | `Self_ast_typed_expected_pair_out loc ->
      Format.fprintf
        f
        "@[<hv>%a@.Invalid entrypoint.@.Expected a tuple of operations and storage as \
         return value.@]"
        Snippet.pp
        loc)


let error_json : self_ast_typed_error -> Simple_utils.Error.t =
 fun e ->
  let open Simple_utils.Error in
  match e with
  | `Self_ast_typed_annotated_declaration_shadowed location ->
    let message = "This declaration holds an annotation and is later shadowed." in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_storage_view_contract (location, main_name, view_name, ct, vt) ->
    let message =
      Format.asprintf
        "Invalid view argument.@.View '%a' has storage type '%a' and contract '%a' has \
         storage type '%a'."
        Value_var.pp
        view_name
        Ast_typed.PP.type_expression
        vt
        Value_var.pp
        main_name
        Ast_typed.PP.type_expression
        ct
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_view_io (location, got, arg) ->
    let s =
      match arg with
      | `In -> "input"
      | `Out -> "output"
    in
    let message =
      Format.asprintf
        "Invalid view.@.Type '%a' is forbidden as %s argument."
        Ast_typed.PP.type_expression
        got
        s
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_recursive_call_is_only_allowed_as_the_last_operation (_name, location)
    ->
    let message =
      Format.sprintf
        "Recursive call not in tail position. @.The value of a recursive call must be \
         immediately returned by the defined function."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_bad_self_type (expected, got, location) ->
    let message =
      Format.asprintf
        "Invalid type annotation.@.\"%a\" was given, but \"%a\" was expected.@.Note that \
         \"Tezos.self\" refers to this contract, so the parameters should be the same."
        Ast_typed.PP.type_expression
        got
        Ast_typed.PP.type_expression
        expected
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_bad_format_entrypoint_ann (ep, location) ->
    let message =
      Format.sprintf
        "Invalid entrypoint \"%s\". One of the following patterns is expected:@.* \
         \"%%bar\" is expected for entrypoint \"Bar\"@.* \"%%default\" when no \
         entrypoint is used."
        ep
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_entrypoint_ann_not_literal location ->
    let message =
      Format.sprintf
        "Invalid entrypoint value.@.The entrypoint value must be a string literal."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_unmatched_entrypoint location ->
    let message =
      Format.sprintf
        "Invalid entrypoint value.@.The entrypoint value does not match a constructor of \
         the contract parameter."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_nested_bigmap location ->
    let message =
      Format.sprintf
        "Invalid big map nesting.@.A big map cannot be nested inside another big map."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_corner_case desc ->
    let message = Format.sprintf "Internal error: %s" desc in
    let content = make_content ~message () in
    make ~stage ~content
  | `Self_ast_typed_bad_contract_io (entrypoint, _, location) ->
    let message =
      Format.asprintf
        "Invalid type for entrypoint \"%a\".@.An entrypoint must of type \"parameter * \
         storage -> operation list * storage\"."
        Value_var.pp
        entrypoint
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_bad_view_io (entrypoint, location) ->
    let message =
      Format.asprintf
        "Invalid type for view \"%a\".@.An view must be a function."
        Value_var.pp
        entrypoint
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_expected_list_operation (entrypoint, got, e) ->
    let location = e.location in
    let message =
      Format.asprintf
        "Invalid type for entrypoint \"%a\".@.An entrypoint must of type \"parameter * \
         storage -> operation list * storage\".@.We expected a list of operations but we \
         got %a"
        Value_var.pp
        entrypoint
        Ast_typed.PP.type_expression
        got
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_expected_same_entry (entrypoint, t1, t2, e) ->
    let location = e.location in
    let message =
      Format.asprintf
        "Invalid type for entrypoint \"%a\".@.The storage type \"%a\" of the function \
         parameter must be the same as the storage type \"%a\" of the return value."
        Value_var.pp
        entrypoint
        Ast_typed.PP.type_expression
        t1
        Ast_typed.PP.type_expression
        t2
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_expected_non_vars_in_storage (entrypoint, t, e) ->
    let location = e.location in
    let message =
      Format.asprintf
        "Invalid type for entrypoint \"%a\".@.The storage type \"%a\" of the entrypoint \
         function must not contain polymorphic variables."
        Value_var.pp
        entrypoint
        Ast_typed.PP.type_expression
        t
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_expected_non_vars_in_parameter (entrypoint, t, e) ->
    let location = e.location in
    let message =
      Format.asprintf
        "Invalid type for entrypoint \"%a\".@.The parameter type \"%a\" of the \
         entrypoint function must not contain polymorphic variables."
        Value_var.pp
        entrypoint
        Ast_typed.PP.type_expression
        t
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_expected_pair_in (location, t) ->
    let ep =
      match t with
      | `View -> "view"
      | `Contract -> "contract"
    in
    let message = Format.sprintf "Invalid %s.@.Expected a tuple as argument." ep in
    let content = make_content ~message ~location () in
    make ~stage ~content
  | `Self_ast_typed_expected_pair_out location ->
    let message =
      Format.sprintf
        "Invalid entrypoint.@.Expected a tuple of operations and storage as return value."
    in
    let content = make_content ~message ~location () in
    make ~stage ~content
