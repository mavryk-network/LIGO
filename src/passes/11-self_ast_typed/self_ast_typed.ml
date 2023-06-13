module Errors = Errors
module Helpers = Helpers
open Ligo_prim

let all_program ~raise init =
  Make_entry_point.make_main_module ~raise init

let all_contract ~raise ~(options : Compiler_options.t) entrypoints module_path (prg : Ast_typed.program) =
  let module_ = Helpers.get_module module_path prg in
  let module_ =
    match entrypoints with
    | [] -> module_
    | _ ->
      Helpers.strip_entry_annotations module_
      |> Helpers.annotate_with_entry ~raise entrypoints
  in
  let main_name, module_ = Make_entry_point.program ~raise module_ in
  let prg, () = Helpers.update_module module_path (fun _ -> module_, ()) prg in
  let prg, main_name, contract_type =
    Helpers.fetch_contract_type ~raise main_name module_path prg
  in
  let () =
    if not options.middle_end.no_metadata_check
    then (
      (* Check storage type TZIP-16 compliance *)
      let open Check_metadata in
      match find_storage_metadata_opt contract_type.storage with
      | Some metadata ->
        check_metadata_tzip16_type_compliance ~raise ?syntax:options.middle_end.syntax_for_errors metadata
      | None -> ())
  in
  (main_name, contract_type), prg


let all_view ~raise command_line_views main_name module_path contract_type prg =
  let module_ = Helpers.get_module module_path prg in
  let () =
    (* detects whether a declared view (passed with --views command line option) overwrites an annotated view ([@view] let ..) *)
    let user_views = Ast_typed.Helpers.get_views module_ in
    match command_line_views with
    | None -> ()
    | Some command_line_views ->
      List.iter user_views ~f:(fun (x, loc) ->
          if Option.is_none (List.find ~f:(Value_var.is_name x) command_line_views)
          then Simple_utils.Trace.(raise.warning (`Main_view_ignored loc)))
  in
  let () =
    match
      Helpers.get_shadowed_decl module_ (fun ({ view; _ } : Ast_typed.ValueAttr.t) ->
          view)
    with
    | Some loc -> raise.error (Errors.annotated_declaration_shadowed loc)
    | None -> ()
  in
  let module_ =
    (* in case of command_line views, strip the existing user views and decorate the AST *)
    match command_line_views with
    | None -> module_
    | Some command_line_views ->
      Helpers.strip_view_annotations module_
      |> Helpers.annotate_with_view ~raise command_line_views
  in
  let f decl =
    match Ast_typed.Helpers.fetch_view_type decl with
    | None -> ()
    | Some (view_type, view_binder) ->
      View_passes.check_view_type
        ~raise
        ~err_data:(main_name, view_binder)
        contract_type
        view_type
  in
  let () = List.iter ~f module_ in
  let prg, () = Helpers.update_module module_path (fun _ -> module_, ()) prg in
  prg


module Check_metadata = Check_metadata
