open Simple_utils
open Display

type all =
[
  | `Self_ast_typed_warning_unused of Location.t * string
  | `Self_ast_typed_warning_muchused of Location.t * string
  | `Checking_ambiguous_contructor of Location.t * Stage_common.Types.type_variable * Stage_common.Types.type_variable
  | `Self_ast_imperative_warning_layout of (Location.t * Ast_imperative.label)
  | `Self_ast_imperative_warning_deprecated_polymorphic_variable of Location.t * Stage_common.Types.TypeVar.t
  | `Main_view_ignored of Location.t
  | `Pascaligo_deprecated_case of Location.t
  | `Pascaligo_deprecated_semi_before_else of Location.t
  | `Michelson_typecheck_failed_with_different_protocol of (Environment.Protocols.t * Tezos_error_monad.Error_monad.error list)
  | `Jsligo_deprecated_failwith_no_return of Location.t
]

let warn_layout loc lab = `Self_ast_imperative_warning_layout (loc,lab)

let pascaligo_deprecated_case loc = `Pascaligo_deprecated_case loc

let pascaligo_deprecated_semi_before_else loc = `Pascaligo_deprecated_semi_before_else loc

let pp : display_format:string display_format ->
  Format.formatter -> all -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Michelson_typecheck_failed_with_different_protocol (user_proto,errs) -> (
      let open Environment.Protocols in
      Format.fprintf f
        "@[<hv>Warning: Error(s) occurred while type checking the produced michelson contract:@.%a@. 
        Note: You compiled your contract with protocol %s although we internally use protocol %s to typecheck the produced Michelson contract 
        so you might want to ignore this error if related to a breaking change in protocol %s@.@]"
          (Tezos_client_012_Psithaca.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errs
          (variant_to_string user_proto)
          (variant_to_string in_use)
          (variant_to_string in_use)
    )
    | `Checking_ambiguous_contructor (loc,tv_chosen,tv_possible) ->
      Format.fprintf f "@[<hv>%a@ Warning: The type of this value is ambiguous: Inferred type is %a but could be of type %a.@ Hint: You might want to add a type annotation. @.@]"
      Snippet.pp loc
      Stage_common.PP.type_variable tv_chosen
      Stage_common.PP.type_variable tv_possible
    | `Main_view_ignored loc ->
      Format.fprintf f "@[<hv>%a@ Warning: This view will be ignored, command line option override [@ view] annotation@.@]"
      Snippet.pp loc
    | `Pascaligo_deprecated_case loc ->
      Format.fprintf f "@[<hv>%a@ Warning: Deprecated syntax behind `of`. An opening bracket `[` is expected behind `of` and `end` is expected to be replaced with a closing bracket `]`.@.@]"
      Snippet.pp loc
    | `Pascaligo_deprecated_semi_before_else loc ->
      Format.fprintf f "@[<hv>%a@ Warning: Deprecated semicolon `;` before the `else` keyword. Please remove the semicolon.@.@]"
      Snippet.pp loc
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
    | `Self_ast_imperative_warning_deprecated_polymorphic_variable (loc, name) ->
        Format.fprintf f
          "@[<hv>%a@ Warning: %a is not recognize as a polymorphic variable anymore. If you want to make a polymorphic function, please consult the online documentation @.@]"
          Snippet.pp loc Stage_common.Types.TypeVar.pp name
    | `Jsligo_deprecated_failwith_no_return loc ->
      Format.fprintf f "@[<hv>%a@.Deprecated `failwith` without `return`: `failwith` is just a function.@.Please add an explicit `return` before `failwith` if you meant the built-in `failwith`.@.For now, compilation proceeds adding such `return` automatically.@.@]"
      Snippet.pp loc
  )
let to_json : all -> Yojson.Safe.t = fun a ->
  let json_warning ~stage ~content =
    `Assoc [
      ("status", `String "warning") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Michelson_typecheck_failed_with_different_protocol (user_proto,_) ->
    let open Environment.Protocols in
    let message = `String "Typechecking the produced Michelson contract against the next protocol failed" in
    let stage   = "Main" in
    let content = `Assoc [
                      ("message", message);
                      ("used", `String (variant_to_string in_use));
                      ("compiled_for", `String (variant_to_string user_proto))
                    ] in
    json_warning ~stage ~content
  | `Checking_ambiguous_contructor (loc,_,_) ->
    let message = `String "the type of this value is ambiguous, you might want to add a type annotation" in
    let stage   = "Main" in
    let loc = Location.to_yojson loc in
    let content = `Assoc [
                      ("message", message);
                      ("location", loc);
                    ] in
    json_warning ~stage ~content
  | `Main_view_ignored loc ->
    let message = `String "command line option overwrites annotated views" in
    let stage   = "Main" in
    let loc = Location.to_yojson loc in
    let content = `Assoc [
                      ("message", message);
                      ("location", loc);
                    ] in
    json_warning ~stage ~content
  | `Pascaligo_deprecated_case loc ->
    let message = `String "deprecated case syntax" in
    let stage   = "lexer" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
                      ("message", message);
                      ("location", loc);
                    ] in
    json_warning ~stage ~content
  | `Pascaligo_deprecated_semi_before_else loc ->
    let message = `String "deprecated semicolon before else" in
    let stage   = "lexer" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
                      ("message", message);
                      ("location", loc);
                    ] in
    json_warning ~stage ~content
  | `Self_ast_typed_warning_unused (loc, s) ->
     let message = `String "unused variable" in
     let stage   = "self_ast_typed" in
     let description = `String s in
     let loc = Location.to_yojson loc in
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
     let loc = Location.to_yojson loc in
     let content = `Assoc [
                       ("message", message);
                       ("location", loc);
                       ("variable", description)
                     ] in
     json_warning ~stage ~content
  | `Self_ast_imperative_warning_layout (loc, s) ->
    let message = `String (Format.asprintf "Layout attribute on constructor %a" Ast_imperative.PP.label s) in
     let stage   = "self_ast_imperative" in
    let loc = Location.to_yojson loc in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_warning ~stage ~content
  | `Self_ast_imperative_warning_deprecated_polymorphic_variable (loc, name) ->
    let message = `String (Format.asprintf "Deprecated polymorphic var %a" Stage_common.Types.TypeVar.pp name) in
     let stage   = "self_ast_imperative" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_warning ~stage ~content
  | `Jsligo_deprecated_failwith_no_return loc ->
    let message = `String "deprecated use of failwith without return" in
    let stage   = "lexer" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
                      ("message", message);
                      ("location", loc);
                    ] in
    json_warning ~stage ~content

let format = {pp;to_json}
