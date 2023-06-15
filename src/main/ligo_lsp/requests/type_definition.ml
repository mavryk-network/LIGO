open Lsp_helpers
open Handler

type type_info =
  { var_name : Ast_core.type_expression option
  ; contents : Ast_core.type_expression
  }

let get_type (vdef : Scopes.Types.vdef) : type_info option =
  match vdef.t with
  | Core ty -> Some { var_name = None; contents = ty }
  | Resolved ({ location; _ } as ty) ->
    let orig_var =
      Option.map
        ~f:(fun x -> Ast_core.{ type_content = T_variable x; location })
        ty.orig_var (* This is non-empty in case there is a name for our type *)
    in
    let raw_options = Compiler_options.Raw_options.make ~use_orig_var:true () in
    let options = (Compiler_options.make ~raw_options ()).middle_end in
    Some { var_name = orig_var; contents = Checking.untype_type_expression ~options ty }
  | Unresolved -> None


let on_req_type_definition : Position.t -> Path.t -> Locations.t option Handler.t =
 fun pos file ->
  with_cached_doc file None
  @@ fun { get_scope_info; _ } ->
  when_some' (Go_to_definition.get_definition pos file get_scope_info.definitions)
  @@ fun def ->
  when_some'
    (let from_def_location : Def.t -> Def.Loc_in_file.t option =
      fun def ->
       match Def.get_location def with
       | StdLib _ | Virtual _ -> None
       | File { range; path } -> Some { range; path }
     in
     match def with
     | Type _ -> from_def_location def
     | Variable vdef ->
       let open Option.Monad_infix in
       get_type vdef
       >>= fun type_expression ->
       let location = Def.Def_location.of_loc type_expression.contents.location in
       (match location with
       | StdLib _ | Virtual _ ->
         None (* We can't return any position to user: type of this vdef is inferred *)
       | File { range; path } ->
         Option.some
         @@ Option.value ~default:Def.Loc_in_file.{ range; path }
         @@ (Go_to_definition.get_definition range.start file get_scope_info.definitions
            >>= from_def_location))
     | Module _mdef -> None)
  @@ fun { range; path } ->
  return @@ Some (`Location [ Location.create ~range ~uri:(DocumentUri.of_path path) ])
