open Handler
open Lsp_helpers

let get_scope_new : Position.t -> Path.t -> string list Handler.t =
  fun pos path ->
  with_cst path []
  @@ fun cst ->
  let open Cst_shared.Fold in
    match cst with
  | CameLIGO cst ->
    let open Cst_cameligo.Fold in
    let collect (Some_node (node, sing)) =
      match sing with
      | S_reg S_let_in when Range.(contains_position pos (of_region node.region)) ->
        Continue node.value.binding.value.binders
      | S_reg _ when Range.(not @@ contains_position pos (of_region node.region))
        -> Stop
      | _ -> Skip
    in
    let pattern_to_string : Cst_cameligo.CST.pattern -> string = function
      | P_Var var -> var#payload
      | _ -> "Unknown pattern" in
    return (fold_cst [] (Fn.flip List.cons) collect cst
    |> List.concat_map ~f:(fun seq -> Simple_utils.Utils.nseq_to_list seq)
    |> List.map ~f:pattern_to_string)
  | JsLIGO cst ->
    let open Cst_jsligo.Fold in
    let collect (Some_node (node, sing)) =
      match sing with
      | S_reg S_value_decl when Range.(contains_position pos (of_region node.region)) ->
        Continue node.value.bindings
      | S_reg _ when Range.(not @@ contains_position pos (of_region node.region))
        -> Stop
      | _ -> Skip
    in
    let pattern_to_string : Cst_jsligo.CST.val_binding Region.reg -> string =
      fun val_binding_region ->
      match val_binding_region.value.pattern with
      | P_Var var -> var#payload
      | _ -> "Unknown pattern" in
    return (fold_cst [] (Fn.flip List.cons) collect cst
    |> List.concat_map ~f:(fun seq -> Simple_utils.Utils.nsepseq_to_list seq)
    |> List.map ~f:pattern_to_string)
