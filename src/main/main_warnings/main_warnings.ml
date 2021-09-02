open Simple_utils
open Display

type all = 
[
  | `Self_ast_typed_warning_unused of Location.t * string
  | `Self_ast_typed_warning_muchused of Location.t * string
  | `Self_ast_imperative_warning_layout of (Location.t * Ast_imperative.label)
  | `Main_self_cst_pascaligo of Self_cst.Pascaligo.Warnings.self_cst_pascaligo_warning
  | `Main_self_cst_cameligo of Self_cst.Cameligo.Warnings.self_cst_cameligo_warning
  | `Main_self_cst_reasonligo of Self_cst.Reasonligo.Warnings.self_cst_reasonligo_warning
  | `Main_self_cst_jsligo of Self_cst.Jsligo.Warnings.self_cst_jsligo_warning
  | `Main_cit_cameligo_warning of Tree_abstraction.Cameligo.Warnings.abs_warning
]

let self_cst_pascaligo_warning_tracer (e:Self_cst.Pascaligo.Warnings.self_cst_pascaligo_warning) : all = `Main_self_cst_pascaligo e
let self_cst_cameligo_warning_tracer (e:Self_cst.Cameligo.Warnings.self_cst_cameligo_warning) : all = `Main_self_cst_cameligo e
let self_cst_reasonligo_warning_tracer (e:Self_cst.Reasonligo.Warnings.self_cst_reasonligo_warning) : all = `Main_self_cst_reasonligo e
let self_cst_jsligo_warning_tracer (e:Self_cst.Jsligo.Warnings.self_cst_jsligo_warning) : all = `Main_self_cst_jsligo e
let cit_cameligo_warning_tracer (w:Tree_abstraction.Cameligo.Warnings.abs_warning) : all = `Main_cit_cameligo_warning w

let warn_layout loc lab = `Self_ast_imperative_warning_layout (loc,lab)

let pp : display_format:string display_format ->
  Format.formatter -> all -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Self_ast_typed_warning_unused (loc, s) ->
        Format.fprintf f
          "@[<hv>%a:@.Warning: unused variable \"%s\".@.Hint: replace it by \"_%s\" to prevent this warning.\n@]"
          Snippet.pp loc s s
    | `Self_ast_typed_warning_muchused (loc, s) ->
        Format.fprintf f
          "@[<hv>%a:@.Warning: variable \"%s\" cannot be used more than once.\n@]"
          Snippet.pp loc s
    | `Self_ast_imperative_warning_layout (loc,Label s) ->
        Format.fprintf f
          "@[<hv>%a@ Warning: layout attribute only applying to %s, probably ignored.@.@]"
          Snippet.pp loc s
    | `Main_self_cst_pascaligo e -> Self_cst.Pascaligo.Warnings.error_ppformat ~display_format f e
    | `Main_self_cst_cameligo e -> Self_cst.Cameligo.Warnings.error_ppformat ~display_format f e
    | `Main_self_cst_reasonligo e -> Self_cst.Reasonligo.Warnings.error_ppformat ~display_format f e
    | `Main_self_cst_jsligo e -> Self_cst.Pascaligo.Warnings.error_ppformat ~display_format f e
    | `Main_cit_cameligo_warning w -> Tree_abstraction.Cameligo.Warnings.warning_ppformat ~display_format f w
  )
let to_json : all -> Yojson.Safe.t = fun a ->
  let json_warning ~stage ~content =
    `Assoc [
      ("status", `String "warning") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Self_ast_typed_warning_unused (loc, s) ->
     let message = `String "unused variable" in
     let stage   = "self_ast_typed" in
     let description = `String s in
     let loc = `String (Format.asprintf "%a" Location.pp loc) in
     let content = `Assoc [
                       ("message", message);
                       ("location", loc);
                       ("variable", description)
                     ] in
     json_warning ~stage ~content
  | `Self_ast_typed_warning_muchused (loc, s) ->
     let message = `String "much used variable" in
     let stage   = "self_ast_typed" in
     let description = `String s in
     let loc = `String (Format.asprintf "%a" Location.pp loc) in
     let content = `Assoc [
                       ("message", message);
                       ("location", loc);
                       ("variable", description)
                     ] in
     json_warning ~stage ~content
  | `Self_ast_imperative_warning_layout (loc, s) ->
    let message = `String (Format.asprintf "Layout attribute on constructor %a" Ast_imperative.PP.label s) in
     let stage   = "self_ast_imperative" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_warning ~stage ~content
  | `Main_self_cst_pascaligo e -> Self_cst.Pascaligo.Warnings.error_jsonformat e
  | `Main_self_cst_cameligo e -> Self_cst.Cameligo.Warnings.error_jsonformat e
  | `Main_self_cst_reasonligo e -> Self_cst.Reasonligo.Warnings.error_jsonformat e
  | `Main_self_cst_jsligo e -> Self_cst.Jsligo.Warnings.error_jsonformat e
  | `Main_cit_cameligo_warning w -> Tree_abstraction.Cameligo.Warnings.warning_jsonformat w

let format = {pp;to_json}
