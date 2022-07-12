open Simple_utils.Display

module Raw      = Cst.Pascaligo
module Region   = Simple_utils.Region
module Snippet  = Simple_utils.Snippet
module Location = Simple_utils.Location

let stage = "abstracter"

type abs_error = [
  | `Concrete_pascaligo_unknown_constant of string * Location.t
  | `Concrete_pascaligo_unsupported_pattern_type of Raw.pattern
  | `Concrete_pascaligo_unsupported_string_singleton of Raw.type_expr
  | `Concrete_pascaligo_michelson_type_wrong of Raw.type_expr * string
  | `Concrete_pascaligo_michelson_type_wrong_arity of Location.t * string
  | `Concrete_pascaligo_untyped_recursive_fun of Location.t
  | `Concrete_pascaligo_block_start_with_attribute of Raw.block Region.reg
  | `Concrete_pascaligo_unsupported_top_level_destructuring of Region.t
  | `Concrete_pascaligo_unsupported_type_ann_on_patterns of Region.t
  | `Concrete_pascaligo_ignored_attribute of Location.t
  | `Concrete_pascaligo_expected_variable of Location.t
  | `Concrete_pascaligo_expected_field_name of Region.t
  | `Concrete_pascaligo_expected_field_or_access of Region.t
  | `Concrete_pascaligo_wrong_functional_lens of Region.t
  | `Concrete_pascaligo_unexpected_wildcard of Region.t
  | `Concrete_pascaligo_wrong_functional_updator of Region.t
  | `Concrete_pascaligo_unsuported_pattern_in_function of Region.t
  | `Concrete_pascaligo_wrong_lvalue of Region.t
  ] [@@deriving poly_constructor { prefix = "concrete_pascaligo_" }]

let error_ppformat : display_format:string display_format ->
  Format.formatter -> abs_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Concrete_pascaligo_wrong_lvalue reg ->
      Format.fprintf f
        "@[<hv>%a@.Effectful updates must be performed on identified objects that are not accessed through a module@]"
        Snippet.pp_lift reg 
    | `Concrete_pascaligo_unsupported_type_ann_on_patterns reg ->
      Format.fprintf f
        "@[<hv>%a@.Type annotations on this kind of patterns are not supported yet@]"
        Snippet.pp_lift reg 
    | `Concrete_pascaligo_unsuported_pattern_in_function reg ->
      Format.fprintf f
        "@[<hv>%a@.These kind of patterns are not supported in function parameters@]"
        Snippet.pp_lift reg 
    | `Concrete_pascaligo_unexpected_wildcard reg ->
      Format.fprintf f
        "@[<hv>%a@.Wildcards ('_') are not supported yet@]"
        Snippet.pp_lift reg 
    | `Concrete_pascaligo_expected_field_name reg ->
      Format.fprintf f
        "@[<hv>%a@.Expected a field name@]"
        Snippet.pp_lift reg 
    | `Concrete_pascaligo_expected_field_or_access reg ->
      Format.fprintf f
        "@[<hv>%a@.Expected a field name or an accessor@]"
        Snippet.pp_lift reg
    | `Concrete_pascaligo_wrong_functional_lens reg ->
      Format.fprintf f
        "@[<hv>%a@.Functional lenses can't be used in record expressions@]"
        Snippet.pp_lift reg
    | `Concrete_pascaligo_ignored_attribute loc ->
      Format.fprintf f
        "@[<hv>%a@.Attribute being ignored@]"
        Snippet.pp loc
    | `Concrete_pascaligo_expected_variable loc ->
      Format.fprintf f
        "@[<hv>%a@.Expected a declaration name@]"
        Snippet.pp loc
    | `Concrete_pascaligo_wrong_functional_updator reg ->
      Format.fprintf f
        "@[<hv>%a@.Functional update only work on records@]"
        Snippet.pp_lift reg
    | `Concrete_pascaligo_unknown_constant (s,loc) ->
      Format.fprintf f
      "@[<hv>%a@.Unknown constant: %s"
        Snippet.pp loc s
    | `Concrete_pascaligo_unsupported_pattern_type pl ->
      Format.fprintf f
        "@[<hv>%a@.Invalid case pattern.\
        @.Can't match on values. @]"
        Snippet.pp_lift @@ Raw.pattern_to_region pl
    | `Concrete_pascaligo_unsupported_string_singleton te ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type. @.It's not possible to assign a string to a type. @]"
        Snippet.pp_lift (Raw.type_expr_to_region te)
    | `Concrete_pascaligo_michelson_type_wrong (texpr,name) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid \"%s\" type.@.At this point, an annotation, in the form of a string, is expected for the preceding type. @]"
          Snippet.pp_lift (Raw.type_expr_to_region texpr)
          name
    | `Concrete_pascaligo_michelson_type_wrong_arity (loc,name) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid \"%s\" type.@.An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string. @]"
        Snippet.pp loc
        name
    | `Concrete_pascaligo_untyped_recursive_fun loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid function declaration.@.Recursive functions are required to have a type annotation (for now). @]"
        Snippet.pp loc
    | `Concrete_pascaligo_block_start_with_attribute block ->
      Format.fprintf f
        "@[<hv>%a@.Invalid attribute declaration.@.Attributes have to follow the declaration it is attached to. @]"
        Snippet.pp_lift @@ block.region
    | `Concrete_pascaligo_unsupported_top_level_destructuring loc ->
      Format.fprintf f
        "@[<hv>%a@.Unsupported destructuring at top-level. @]"
        Snippet.pp_lift @@ loc
  )


let error_jsonformat : abs_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Concrete_pascaligo_wrong_lvalue _
  | `Concrete_pascaligo_unsupported_type_ann_on_patterns _
  | `Concrete_pascaligo_unsuported_pattern_in_function _
  | `Concrete_pascaligo_expected_field_or_access _
  | `Concrete_pascaligo_unexpected_wildcard _
  | `Concrete_pascaligo_wrong_functional_lens _
  | `Concrete_pascaligo_expected_variable _
  | `Concrete_pascaligo_expected_field_name _
  | `Concrete_pascaligo_ignored_attribute _
  | `Concrete_pascaligo_wrong_functional_updator _
    -> failwith "WAIT"
  | `Concrete_pascaligo_unsupported_top_level_destructuring loc ->
    let message = `String "Unsupported destructuring at top-level" in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson (Snippet.lift loc));] in
    json_error ~stage ~content
  | `Concrete_pascaligo_unknown_constant (s,loc) ->
    let message = `String ("Unknow constant: " ^ s) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
    ] in
    json_error ~stage ~content
  | `Concrete_pascaligo_untyped_recursive_fun loc ->
    let message = `String "Untyped recursive functions are not supported yet" in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_pascaligo_unsupported_pattern_type pl ->
    let loc = Location.lift @@ Raw.pattern_to_region pl in
    let message = `String "Currently, only booleans, lists, options, and constructors are supported in patterns" in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_pascaligo_unsupported_string_singleton te ->
    let message = `String "Unsupported singleton string type" in
    let loc = Location.lift (Raw.type_expr_to_region te) in
    let content = `Assoc [
      ("message", message );
      ("location", Location.to_yojson loc);] in
    json_error ~stage ~content
  | `Concrete_pascaligo_michelson_type_wrong (texpr,name) ->
    let message = Format.asprintf "Argument must be a string singleton" in
    let loc = Location.lift (Raw.type_expr_to_region texpr) in
    let content = `Assoc [
      ("message", `String message );
      ("name"   , `String name );
      ("location", Location.to_yojson loc); ] in
    json_error ~stage ~content
  | `Concrete_pascaligo_michelson_type_wrong_arity (loc,name) ->
    let message = Format.asprintf "%s does not have the right number of argument" name in
    let content = `Assoc [
      ("message", `String message );
      ("location", Location.to_yojson loc); ] in
    json_error ~stage ~content
  | `Concrete_pascaligo_block_start_with_attribute block ->
    let message = Format.asprintf "Attributes have to follow the declaration it is attached" in
    let loc = Location.lift block.region in
    let content = `Assoc [
      ("message", `String message );
      ("location", Location.to_yojson loc); ] in
    json_error ~stage ~content
