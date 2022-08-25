module Errors = Errors
module Helpers = Helpers

let all_module_passes ~raise ~warn_unused_rec = [
  Unused.unused_map_module ~raise;
  Muchused.muchused_map_module ~raise;
  Helpers.map_module @@ Recursion.check_tail_expression ~raise ;
  Helpers.map_module @@ Recursion.remove_rec_expression ~raise ~warn_unused_rec ;
  Helpers.map_module @@ Pattern_matching_simpl.peephole_expression ;
]

let all_expression_passes ~raise ~warn_unused_rec = [
  Helpers.map_expression @@ Recursion.check_tail_expression ~raise ;
  Helpers.map_expression @@ Recursion.remove_rec_expression ~raise ~warn_unused_rec ;
  Pattern_matching_simpl.peephole_expression ;
]

let contract_passes ~raise = [
  (* REMITODO: Move old self_mini_c.ml "self in lambda" check *)
  No_nested_big_map.self_typing ~raise ;
]

let all_module ~raise ~warn_unused_rec init =
  List.fold ~f:(|>) (all_module_passes ~raise ~warn_unused_rec) ~init

let all_expression ~raise ~warn_unused_rec init =
  List.fold ~f:(|>) (all_expression_passes ~raise ~warn_unused_rec) ~init

let all_contract ~raise main_name prg =
  let contract_type = Helpers.fetch_contract_type ~raise main_name prg in
  let data : Contract_passes.contract_pass_data = {
    contract_type = contract_type ;
    main_name = main_name ;
    } in
  let all_p = List.map ~f:(fun pass -> Ast_typed.Helpers.fold_map_module pass data) @@ contract_passes ~raise in
  let prg = List.fold ~f:(fun x f -> snd @@ f x) all_p ~init:prg in
  let prg = Contract_passes.remove_unused ~raise data prg in
  prg

let all_view ~raise command_line_views main_name prg =
  let () =
    (* detects whether a declared view (passed with --views command line option) overwrites an annotated view ([@view] let ..) *)
    let user_views = Ast_typed.Helpers.get_views prg in
    match command_line_views with
    | None -> ()
    | Some command_line_views -> (
      List.iter user_views
        ~f:(fun (x,loc) ->
          if Option.is_none (List.find ~f:(fun s -> Ast_typed.ValueVar.is_name x s) command_line_views) then
            Simple_utils.Trace.(raise.warning (`Main_view_ignored loc))
        )
    )
  in
  let () =
    match Helpers.get_shadowed_decl prg (fun ({ view ; _ } : Ast_typed.known_attributes) -> view) with
    | Some loc -> raise.error (Errors.annotated_declaration_shadowed loc)
    | None -> ()
  in
  let prg =
    (* in case of command_line views, strip the existing user views and decorate the AST *)
    match command_line_views with
    | None -> prg
    | Some command_line_views ->
      Helpers.strip_view_annotations prg |> Helpers.annotate_with_view ~raise command_line_views
  in
  let contract_type = Helpers.fetch_contract_type ~raise main_name prg in
  let f decl =
    match Ast_typed.Helpers.fetch_view_type decl with
    | None -> ()
    | Some (view_type,view_binder) ->
      (* ValueVar.get_location binder.var *)
      View_passes.check_view_type ~raise ~err_data:(main_name,view_binder) contract_type view_type
  in
  let () = List.iter ~f prg in
  prg

let all = [
  Recursion.check_tail_expression
]

let remove_unused_expression = Contract_passes.remove_unused_expression
let remove_unused_for_views = Contract_passes.remove_unused_for_views