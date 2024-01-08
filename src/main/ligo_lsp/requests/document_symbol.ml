open Handler
open Lsp_helpers

let create_hierarchy : Def.t list -> Def.t Rose.forest =
  Rose.map_forest ~f:snd
  <@ Rose.forest_of_list
     (* See the expect test in [Rose] on why we compare and check for intersection like this. *)
       ~compare:(fun ((range1 : Range.t), _def1) (range2, _def2) ->
         let ord = Position.compare range1.start range2.start in
         if ord = 0 then Position.compare range2.end_ range1.end_ else ord)
       ~intersects:(fun (range1, _def1) (range2, _def2) ->
         Position.compare range1.end_ range2.start > 0)
  <@ List.filter_map ~f:(fun def ->
         let%map.Option range = Range.of_loc @@ Scopes.Types.get_decl_range def in
         range, def)


let guard_ghost (input : string) : string option =
  Option.some_if (not @@ Parsing.Errors.ErrorWrapper.is_wrapped input) input


let make_def_info (syntax : Syntax_types.t) (def : Def.t)
    : (string option * SymbolKind.t * string * Range.t * Range.t) option
  =
  let detail, kind, name, decl_range, range =
    match def with
    | Variable ({ name; range; decl_range; def_type; _ } as vdef) ->
      let type_info = Def.get_type vdef in
      let detail =
        Option.map type_info ~f:(Pretty.show_type ~syntax <@ Def.use_var_name_if_available)
      in
      let is_field =
        match def_type with
        | Module_field -> true
        | Local | Parameter | Global -> false
      in
      let kind =
        match type_info with
        | None -> if is_field then SymbolKind.Field else Variable
        | Some { var_name = _; contents = { type_content; location = _ } } ->
          (match type_content with
          | T_sum _ -> EnumMember
          | T_record _ -> Field
          | T_arrow _ -> if is_field then Method else Function
          | _ -> Variable)
      in
      detail, kind, name, decl_range, range
    | Type { name; range; decl_range; content; _ } ->
      let detail = Option.map content ~f:(Pretty.show_type ~syntax) in
      let kind =
        match content with
        | None -> SymbolKind.TypeParameter
        | Some { type_content; location = _ } ->
          (match type_content with
          | T_sum _ -> Enum
          | _ -> Struct)
      in
      detail, kind, name, decl_range, range
    | Module { name; range; decl_range; mdef_type; signature; _ } ->
      let detail =
        match%bind.Option
          match signature with
          | Unresolved -> None
          | Core signature -> Some (Pretty.pretty_print_signature ~syntax signature)
          | Resolved signature ->
            Some
              (Pretty.pretty_print_signature ~syntax
              @@ Checking.untype_signature signature)
        with
        | `Ok pretty -> Some pretty
        | `Nonpretty _ -> None
      in
      let kind =
        match syntax, mdef_type with
        | CameLIGO, Module -> SymbolKind.Module
        | CameLIGO, Signature -> Interface
        | JsLIGO, Module -> Namespace
        | JsLIGO, Signature -> Interface
      in
      detail, kind, name, decl_range, range
  in
  let%bind.Option name = guard_ghost name in
  let%bind.Option selectionRange = Range.of_loc range in
  let%bind.Option range = Range.of_loc decl_range in
  let%map.Option () = Option.some_if (Range.inside ~small:selectionRange ~big:range) () in
  let detail = Option.bind ~f:guard_ghost detail in
  detail, kind, name, selectionRange, range


let rec get_all_symbols_hierarchy
    : Syntax_types.t -> Def.t Rose.tree -> DocumentSymbol.t option
  =
 fun syntax (Tree ((hd, tl), defs)) ->
  if List.is_empty tl
  then (
    let%map.Option detail, kind, name, selectionRange, range = make_def_info syntax hd in
    let children = get_all_symbols_hierarchies syntax defs in
    DocumentSymbol.create ?children ?detail ~kind ~name ~range ~selectionRange ())
  else (
    (* TODO: we should use constructor info for this, but we don't have them in scopes
       yet, so we use a placeholder for now. Note that this also assumes a single
       constructor being destructured, but there may be many of them. *)
    let%bind.Option selectionRange =
      Range.of_loc
      @@ List.fold tl ~init:(Scopes.Types.get_range hd) ~f:(fun acc def ->
             Loc.cover acc @@ Scopes.Types.get_range def)
    in
    let%bind.Option range = Range.of_loc @@ Scopes.Types.get_decl_range hd in
    let%map.Option () =
      Option.some_if (Range.inside ~small:selectionRange ~big:range) ()
    in
    let ctor_children = hd :: tl in
    let detail = None in
    let kind = SymbolKind.Constructor in
    let name =
      String.concat ~sep:", " @@ List.map ctor_children ~f:Scopes.Types.get_def_name
    in
    let children = get_all_symbols_hierarchies syntax defs in
    let ctor_children =
      match
        List.filter_map ctor_children ~f:(fun def ->
            let%map.Option detail, kind, name, selectionRange, range =
              make_def_info syntax def
            in
            DocumentSymbol.create ?children ?detail ~kind ~name ~range ~selectionRange ())
      with
      | [] -> None
      | _ :: _ as children -> Some children
    in
    let children =
      match ctor_children, children with
      | None, None -> None
      | None, Some nested_children -> Some nested_children
      | Some ctor_children, (* already nested in ctor_children *) _ -> Some ctor_children
    in
    DocumentSymbol.create ?children ?detail ~kind ~name ~range ~selectionRange ())


and get_all_symbols_hierarchies
    : Syntax_types.t -> Def.t Rose.forest -> DocumentSymbol.t list option
  =
 fun syntax defs ->
  match List.filter_map defs ~f:(get_all_symbols_hierarchy syntax) with
  | [] -> None
  | _ :: _ as defs -> Some defs


let on_req_document_symbol (path : Path.t)
    : [> `DocumentSymbol of DocumentSymbol.t list ] option Handler.t
  =
  with_cached_doc_pure path ~default:None
  @@ fun { syntax; code = _; definitions } ->
  let definitions =
    List.filter definitions ~f:(fun def ->
        match Scopes.Types.get_range def with
        | File reg -> Path.equal path (Path.from_absolute reg#file)
        | Virtual _ -> false)
  in
  let hierarchy = create_hierarchy definitions in
  Option.map (get_all_symbols_hierarchies syntax hierarchy) ~f:(fun symbols ->
      `DocumentSymbol symbols)
