open Handler
open Lsp_helpers

let hovers_pp_mode : Pretty.pp_mode =
  { width = Helpers_pretty.default_line_width_for_hovers; indent = 2 }


let hover_string : Syntax_types.t -> Scopes.def -> string Handler.t =
 fun syntax ->
  let print_type_with_prefix ?prefix t =
    match Pretty.pretty_print_type_expression hovers_pp_mode ~syntax ?prefix t with
    | `Ok str -> return str
    | `Nonpretty (err, nonpretty_type) ->
      let@ () =
        send_log_msg ~type_:Error
        @@
        match err with
        | `Exn exn -> "pretty_print_type_expression: exception: " ^ Exn.to_string exn
        | `PassesError e ->
          "pretty_print_type_expression: passes error: "
          ^ Helpers_pretty.passes_error_to_string e
      in
      return nonpretty_type
  in
  function
  | Variable vdef ->
    let prefix = PPrint.(string vdef.name ^//^ colon) in
    let type_info = Type_definition.get_type vdef in
    Option.value_map
      ~default:(return @@ Helpers_pretty.unresolved_type_as_comment syntax)
      ~f:(print_type_with_prefix ~prefix <@ Type_definition.use_var_name_if_availiable)
      type_info
  | Type tdef ->
    let rec get_params (t : Ast_core.type_content) =
      match t with
      | T_abstraction Ligo_prim.Abstraction.{ ty_binder; kind = _; type_ } ->
        ty_binder :: get_params type_.type_content
      | _ -> []
    in
    let params = get_params tdef.content.type_content in
    (* Like ['a x] or [x<a>] if there are params, or just [x] otherwise *)
    let@ name_with_params =
      match params with
      | [] -> return tdef.name
      | _ ->
        print_type_with_prefix
        @@ Ast_core.Combinators.t_app
             ~loc:Loc.dummy
             { type_operator =
                 Ligo_prim.Module_access.make_el
                 @@ Ligo_prim.Type_var.of_input_var ~loc:Loc.dummy tdef.name
             ; arguments =
                 List.map
                   ~f:(fun var -> Ast_core.Combinators.t_variable var ~loc:Loc.dummy ())
                   params
             }
             ()
    in
    let prefix = PPrint.(string "type" ^//^ PPrint.string name_with_params ^//^ equals) in
    print_type_with_prefix ~prefix tdef.content
  | Module mdef ->
    let core_sig =
      match mdef.signature with
      | Core_sig core_sig -> Some core_sig
      | Resolved_sig signature ->
        Some (Checking.untype_signature ~use_orig_var:true signature)
      | Unresolved_sig -> None
    in
    let print_signature sig_ =
      match Pretty.pretty_print_signature ~syntax sig_ with
      | `Ok str -> return str
      | `Nonpretty (err, nonpretty_sig) ->
        let@ () =
          send_log_msg ~type_:Error
          @@
          match err with
          | `Exn exn -> "pretty_print_signature: exception: " ^ Exn.to_string exn
          | `PassesError e ->
            "pretty_print_signature: passes error: "
            ^ Helpers_pretty.passes_error_to_string e
        in
        return nonpretty_sig
    in
    let@ sig_str =
      Option.value_map
        ~default:(return @@ Helpers_pretty.unresolved_type_as_comment syntax)
        ~f:print_signature
        core_sig
    in
    return @@ Helpers_pretty.print_module syntax sig_str mdef


let on_req_hover : Position.t -> Path.t -> Hover.t option Handler.t =
 fun pos file ->
  with_cached_doc file None
  @@ fun { definitions; syntax; _ } ->
  when_some' (Go_to_definition.get_definition pos file definitions)
  @@ fun definition ->
  let@ hover_string = hover_string syntax definition in
  (* Since a hover contents is a Markdown code block (e.g. starts with "```cameligo"), the text editor
     will apply nice highlight (i.e. highlight for corresponding LIGO syntax) to hover message *)
  let markdown = Format.sprintf "```%s\n%s\n```" (Syntax.to_string syntax) hover_string in
  let contents = `MarkupContent MarkupContent.{ kind = Markdown; value = markdown } in
  let hover = Hover.create ~contents () in
  return (Some hover)
