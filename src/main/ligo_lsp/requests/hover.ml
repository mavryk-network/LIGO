open Handler
open Lsp_helpers

let hovers_pp_mode : Ligo_interface.pp_mode =
  { width = Helpers_pretty.default_line_width_for_hovers; indent = 2 }


let hover_string : Syntax_types.t -> Scopes.def -> string Handler.t =
 fun syntax ->
  let print_type_with_prefix ~prefix t =
    match
      Ligo_interface.pretty_print_type_expression hovers_pp_mode ~syntax ~prefix t
    with
    | `Ok str -> return str
    | `Nonpretty (err, nonpretty_type) ->
      let@ () =
        send_log_msg ~type_:Error
        @@
        match err with
        | `Exn exn -> "pretty_print_type_expression: exception: " ^ Exn.to_string exn
        | `PassesError e ->
          "pretty_print_type_expression: error: "
          ^ Helpers_pretty.passes_error_to_string e
      in
      return nonpretty_type
  in
  function
  | Variable vdef ->
    let prefix = PPrint.(string vdef.name ^//^ string ":") in
    let type_info = Type_definition.get_type vdef in
    (* XXX use var_name where appropriate *)
    Option.map ~f:(fun x -> x.contents) type_info
    |> Option.map ~f:(print_type_with_prefix ~prefix)
    |> Option.value
         ~default:
           (let opening_comment, closing_comment = Helpers_pretty.get_comment syntax in
            return @@ opening_comment ^ " Unresolved " ^ closing_comment)
  | Type tdef ->
    let prefix = PPrint.(string "type" ^//^ string tdef.name ^//^ string "=") in
    print_type_with_prefix ~prefix tdef.content
  | Module mdef -> return @@ Helpers_pretty.print_module syntax mdef


let on_req_hover : Position.t -> Path.t -> Hover.t option Handler.t =
 fun pos file ->
  with_cached_doc file None
  @@ fun { get_scope_info; _ } ->
  when_some' (Go_to_definition.get_definition pos file get_scope_info.definitions)
  @@ fun definition ->
  when_some' (Path.get_syntax file)
  @@ fun syntax ->
  let syntax_highlight = Syntax.to_string syntax in
  let@ hover_string = hover_string syntax definition in
  let markdown = Format.sprintf "```%s\n" syntax_highlight ^ hover_string ^ "\n```" in
  let contents = `MarkupContent MarkupContent.{ kind = Markdown; value = markdown } in
  let hover = Hover.create ~contents () in
  return (Some hover)
