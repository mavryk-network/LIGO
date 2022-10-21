open Simple_utils.Trace
module Errors = Errors
open Errors
module Signature = Context.Signature
module Elaboration = Context.Elaboration
module O = Ast_typed
open O.Combinators
open Ligo_prim

let parameter_from_entry_points ~raise : (e_variable * O.type_expression) List.Ne.t -> O.type_expression * O.type_expression =
  fun ((entrypoint, entrypoint_type), rest) ->
  let parameter, storage = trace_option ~raise (corner_case "Entrypoint has not entrypoint type") @@ O.Misc.get_entry_form entrypoint_type in
  let parameter_list = List.fold ~init:[Value_var.to_name_exn entrypoint,parameter] ~f:(fun parameters (ep, ep_type) ->
      let parameter_, storage_ = trace_option ~raise (corner_case "Entrypoint has not entrypoint type") @@ O.Misc.get_entry_form ep_type in
      let () = trace_option ~raise (corner_case "Storage types do not match") @@
        Ast_typed.assert_type_expression_eq (storage_,storage) in
      (Value_var.to_name_exn ep, parameter_)::parameters) rest in
  Ast_typed.t_sum_ez ~layout:O.default_layout parameter_list, storage

let default_entrypoint = "main"
let default_entrypoint_var = Value_var.of_input_var default_entrypoint

let create_entrypoint_function_expr entrypoints parameter_type storage_type =
  let open Ast_typed in
  let p_var = (Value_var.of_input_var "p") in
  let s_var = (Value_var.of_input_var "s") in
  let p = e_a_variable p_var parameter_type in
  let s = e_a_variable s_var storage_type in
  let params = Value_var.fresh ~name:"param" () in
  let fields = Record.LMap.of_list
    [(Label "0", Location.wrap @@ Pattern.(P_var (Binder.make p_var parameter_type)));
     (Label "1", Location.wrap @@ Pattern.(P_var (Binder.make s_var storage_type)))] in
  let param_storage = e_a_pair p s in
  let fun_type = O.Misc.build_entry_type parameter_type storage_type in
  let oplst_storage = t_pair (t_list @@ t_operation ()) storage_type in
  let cases = List.map entrypoints ~f:(fun entrypoint ->
    let constructor = Label.of_string (Value_var.to_name_exn entrypoint) in
    let pattern = Value_var.fresh ~name:"pattern" () in
    let body = e_a_application
      (e_a_variable entrypoint fun_type)
        (e_a_pair (e_variable pattern (t_unit ())) s)
        oplst_storage in
    let pattern = Location.wrap @@ Pattern.(P_variant (constructor, Location.wrap @@ P_var (Binder.make pattern param_storage.type_expression))) in
    ({pattern;body} : _ Match_expr.match_case)) in
  let body = e_a_matching p cases oplst_storage in
  let pattern = Location.wrap @@ Pattern.(P_record fields) in
  let result = e_a_matching (e_a_variable params param_storage.type_expression) [{pattern;body}] oplst_storage  in
  e_lambda {binder=Param.(make params param_storage.type_expression);result;output_type=oplst_storage} fun_type

let program ~raise : Ast_typed.module_ -> (Ast_typed.declaration * Ast_typed.declaration) option =
  fun module_ ->
  let f d = match Location.unwrap d with
    | Ast_typed.D_value {binder;attr;_} when attr.entry && Option.is_some (Binder.get_ascr binder) ->
      Some (Binder.get_var binder, Option.value_exn @@ Binder.get_ascr binder)
    | _ -> None in
  let open Simple_utils.Option in
  let* entries = List.Ne.of_list_opt @@ List.filter_map ~f module_ in
  let parameter_type, storage_type = parameter_from_entry_points ~raise entries in
  let type_binder = Type_var.fresh ~name:"parameter" () in
  let entrypoint_type_decl = Location.wrap @@ Ast_typed.D_type {type_binder;type_expr=parameter_type;type_attr={public=true;hidden=false}} in
  let entrypoint_function_decl =
    let expr = create_entrypoint_function_expr (List.Ne.to_list @@ List.Ne.map fst entries) parameter_type storage_type in
    let binder = Binder.make default_entrypoint_var (Some expr.type_expression) in
    Location.wrap @@ Ast_typed.D_value {binder;expr;attr={inline=false;no_mutation=false;entry=false;view=false;public=true;thunk=false;hidden=false}}
  in
  return (entrypoint_type_decl, entrypoint_function_decl)

let make_main_module ~raise (module_ : O.module_expr) =
  match Location.unwrap module_ with
  | M_struct ds -> (
    match program ~raise ds with
    | None -> module_
    | Some (type_decl, main_decl) ->
      Location.wrap @@ Module_expr.M_struct (ds @ [type_decl; main_decl])
  )
  | _ -> module_

let program_sig_ ~raise : Signature.t -> (Signature.item * Signature.item) option =
  fun sig_ ->
  let f s = match s with
    | Signature.S_value (var, ty, entry) when entry -> Some (var, ty)
    | _ -> None in
  let open Simple_utils.Option in
  let* entries = List.Ne.of_list_opt @@ List.filter_map ~f sig_ in
  let parameter_type, storage_type = parameter_from_entry_points ~raise entries in
  let type_binder = Type_var.fresh ~name:"parameter" () in
  let parameter_type_decl = Signature.S_type (type_binder, parameter_type) in
  let contract_type = O.Misc.build_entry_type parameter_type storage_type in
  let contract_decl = Signature.S_value (default_entrypoint_var, contract_type, false) in
  return (parameter_type_decl, contract_decl)

let make_main_sig_ ~raise (sig_ : Signature.t) =
  sig_ @ match program_sig_ ~raise sig_ with
  | None -> []
  | Some (type_decl, main_decl) -> [type_decl; main_decl]

let make_main ~raise ~options ~ctx sig_ (module_ : (O.module_expr, typer_error, Main_warnings.all) Elaboration.t) =
  ignore options; ignore ctx;
  let sig_ = make_main_sig_ ~raise sig_ in
  ctx, sig_,
  let open Elaboration.Let_syntax in
  let%bind module_ = module_ in
  let module_ = make_main_module ~raise module_ in
  return @@ module_
