open Handler
open Lsp_helpers

let hover_string : Syntax_types.t -> Scopes.def -> string =
 fun syntax ->
  let opening_comment, closing_comment = Helpers_pretty.get_comment syntax in
  let type_to_string t =
    Trace.try_with
      (fun ~raise ~catch:_ ->
        Some (Helpers_pretty.pp_type_expression ~raise ~syntax (`Core t)))
      (fun ~catch:_ _ -> None)
  in
  let unwrap type_ =
    Option.value ~default:(opening_comment ^ " Unresolved " ^ closing_comment) type_
  in
  function
  | Variable vdef ->
    Type_definition.get_type vdef |> Option.bind ~f:type_to_string |> unwrap
  | Type tdef -> type_to_string tdef.content |> unwrap
  | Module mdef -> Helpers_pretty.print_module syntax mdef


let on_req_hover : Position.t -> Path.t -> Hover.t option Handler.t =
 fun pos file ->
  with_cached_doc file None
  @@ fun { get_scope_info; _ } ->
  when_some' (Go_to_definition.get_definition pos file get_scope_info.definitions)
  @@ fun definition ->
  when_some' (Path.get_syntax file)
  @@ fun syntax ->
  let syntax_highlight = Syntax.to_string syntax in
  let hover_string = hover_string syntax definition in
  let marked_string : MarkedString.t =
    { value = Format.sprintf "```%s\n" syntax_highlight ^ hover_string ^ "\n```"
    ; language = None
    }
  in
  let contents = `MarkedString marked_string in
  let hover = Hover.create ~contents () in
  return (Some hover)
