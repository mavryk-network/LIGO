module PP_helpers = Simple_utils.PP_helpers
open Types

let scopes : Format.formatter -> scopes -> unit =
 fun f s ->
  let pp_scope f (scope : scope) =
    let loc, defs = scope in
    let defs = List.sort defs ~compare:compare_def in
    let pp_bindings f defs =
      List.iter defs ~f:(fun def -> Format.fprintf f "%s " (get_def_uid def))
    in
    Format.fprintf f "[ %a ] %a" pp_bindings defs Location.pp loc
  in
  let pp_scopes f = List.iter ~f:(Format.fprintf f "@[<v>%a@ @]" pp_scope) in
  let s =
    List.sort s ~compare:(fun (l1, _) (l2, _) ->
        Simple_utils.Location_ordered.compare l1 l2)
  in
  Format.fprintf f "@[<v>Scopes:@ %a@]" pp_scopes s


let rec definitions : Format.formatter -> def list -> unit =
 fun f defs ->
  let defs = List.sort defs ~compare:compare_def in
  let refs ppf locs =
    let locs = LSet.elements locs in
    match locs with
    | [] -> Format.fprintf ppf "references: []"
    | locs ->
      let locs = List.sort locs ~compare:Location.compare in
      Format.fprintf
        ppf
        "@[<hv 2>references:@ %a@]"
        PP_helpers.(list_sep Location.pp (tag " ,@ "))
        locs
  in
  let pp_def_type ppf = function
    | Local -> Format.fprintf ppf "Local"
    | Global -> Format.fprintf ppf "Global"
    | Parameter -> Format.fprintf ppf "Parameter"
    | Module_field -> Format.fprintf ppf "Module_field"
  in
  let pp_content ppf = function
    | Variable v ->
      let typ ppf t =
        match t with
        | Core t -> Format.fprintf ppf "core: %a" Ast_core.PP.type_expression t
        | Resolved t -> Format.fprintf ppf "resolved: %a" Ast_typed.PP.type_expression t
        | Unresolved -> Format.fprintf ppf "unresolved"
      in
      Format.fprintf
        ppf
        "|%a|@ %a @ Mod Path = %a @ Def Type = %a"
        typ
        v.t
        refs
        v.references
        (PP_helpers.list String.pp)
        v.mod_path
        pp_def_type
        v.def_type
    | Type t ->
      Format.fprintf
        ppf
        ": |%a|@ %a"
        Ast_core.PP.type_expression
        t.content
        refs
        t.references
    | Module { mod_case = Alias (a, _resolved); references; _ } ->
      Format.fprintf ppf "Alias: %s @ %a @ " (String.concat ~sep:"." a) refs references
    | Module { mod_case = Def d; references; _ } ->
      Format.fprintf ppf "Members: %a @ %a @ " definitions d refs references
    | Constructor c ->
      Format.fprintf
        ppf
        "|member_type = %a|@ parent_type = %a @%a @ Mod Path = %a @ Def Type = %a"
        Ast_core.PP.type_expression
        c.t
        Ast_core.PP.type_expression
        c.parent_type
        refs
        c.references
        (PP_helpers.list String.pp)
        c.mod_path
        pp_def_type
        c.def_type
  in
  let variables, types_modules_constructors =
    List.partition_tf
      ~f:(function
        | Type _ | Module _ | Constructor _ -> false
        | Variable _ -> true)
      defs
  in
  let types, modules_constrconstructors =
    List.partition_tf
      ~f:(function
        | Type _ -> true
        | _ -> false)
      types_modules_constructors
  in
  let modules, constructors =
    List.partition_tf
      ~f:(function
        | Module _ -> true
        | _ -> false)
      modules_constrconstructors
  in
  let pp_def f =
    List.iter ~f:(fun def ->
        Format.fprintf
          f
          "(%s -> %s) @ Range: %a @ Body Range: %a @ Content: %a@ "
          (get_def_uid def)
          (get_def_name def)
          Location.pp
          (get_def_range def)
          Location.pp
          (get_def_range def)
          pp_content
          def)
  in
  Format.fprintf
    f
    "@[<v>Variable definitions:@ %a @ Constructor definition: %a @ Type definitions:@ \
     %aModule definitions:@ %a@]"
    pp_def
    variables
    pp_def
    constructors
    pp_def
    types
    pp_def
    modules


let rec def_to_yojson : def -> string * Yojson.Safe.t =
 fun def ->
  let type_case_to_yojson = function
    | Core t -> `Assoc [ "core", Ast_core.type_expression_to_yojson t ]
    | Resolved t -> `Assoc [ "resolved", Ast_typed.type_expression_to_yojson t ]
    | Unresolved -> `Assoc [ "unresolved", `Null ]
  in
  let definition ~name ~range ~body_range ~t ~references =
    let references = LSet.elements references in
    `Assoc
      [ "name", `String name
      ; "range", Location.to_yojson range
      ; "body_range", Location.to_yojson body_range
      ; "t", type_case_to_yojson t
      ; "references", `List (List.map ~f:Location.to_yojson references)
      ]
  in
  let type_definition ~name ~range ~body_range ~content ~references =
    let references = LSet.elements references in
    `Assoc
      [ "name", `String name
      ; "range", Location.to_yojson range
      ; "body_range", Location.to_yojson body_range
      ; "content", Ast_core.type_expression_to_yojson content
      ; "references", `List (List.map ~f:Location.to_yojson references)
      ]
  in
  let aux = function
    | Variable { name; range; body_range; t; references; uid; def_type = _; mod_path = _ }
      -> uid, definition ~name ~range ~body_range ~t ~references
    | Type
        { name; range; body_range; content; uid; def_type = _; references; mod_path = _ }
      -> uid, type_definition ~name ~range ~body_range ~content ~references
    | Module
        { name
        ; range
        ; body_range
        ; mod_case = Def d
        ; references
        ; uid
        ; def_type = _
        ; mod_path = _
        } ->
      ( uid
      , `Assoc
          [ "definition", definition ~name ~range ~body_range ~references ~t:Unresolved
          ; "members", defs_json d
          ] )
    | Module
        { name
        ; range
        ; body_range
        ; mod_case = Alias (a, _resolved)
        ; references
        ; uid
        ; def_type = _
        ; mod_path = _
        } ->
      let alias = `List (List.map a ~f:(fun s -> `String s)) in
      ( uid
      , `Assoc
          [ "definition", definition ~name ~range ~body_range ~references ~t:Unresolved
          ; "alias", alias
          ] )
    | Constructor _ -> failwith "DUNNO WAT 2 DO"
  in
  aux def


and defs_json (defs : def list) : Yojson.Safe.t =
  let get_defs defs =
    let variables, types_modules =
      List.partition_tf defs ~f:(function
          | Variable _ -> true
          | Type _ | Module _ | Constructor _ -> false)
    in
    let types, modules =
      List.partition_tf types_modules ~f:(function
          | Type _ -> true
          | Variable _ | Module _ | Constructor _ -> false)
    in
    let modules, module_aliases =
      List.partition_tf modules ~f:(function
          | Module { mod_case = Def _; _ } -> true
          | Variable _ | Type _ | Module { mod_case = Alias _; _ } | Constructor _ ->
            false)
    in
    (* TODO: construtors as seprate entry or put it in variables *)
    [ "variables", `Assoc (List.map ~f:(fun def -> def_to_yojson def) variables)
    ; "types", `Assoc (List.map ~f:(fun def -> def_to_yojson def) types)
    ; "modules", `Assoc (List.map ~f:(fun def -> def_to_yojson def) modules)
    ; "module_aliases", `Assoc (List.map ~f:(fun def -> def_to_yojson def) module_aliases)
    ]
  in
  `Assoc (get_defs defs)


let scopes_json (scopes : scopes) : Yojson.Safe.t =
  `List
    (List.map
       ~f:(fun scope ->
         let loc, defs = scope in
         let variables, types_modules =
           List.partition_tf defs ~f:(function
               | Type _ | Module _ | Constructor _ -> false
               | Variable _ -> true)
         in
         let types, modules =
           List.partition_tf types_modules ~f:(function
               | Type _ | Constructor _ -> true
               | Module _ | Variable _ -> false)
         in
         (* TODO: construtors as seprate entry or put it in variables *)
         let vs = List.map ~f:(fun def -> `String (get_def_uid def)) variables in
         let ts = List.map ~f:(fun def -> `String (get_def_uid def)) types in
         let ms = List.map ~f:(fun def -> `String (get_def_uid def)) modules in
         `Assoc
           [ "range", Location.to_yojson loc
           ; "expression_environment", `List vs
           ; "type_environment", `List ts
           ; "module_environment", `List ms
           ])
       scopes)
