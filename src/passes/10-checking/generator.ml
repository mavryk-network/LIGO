open Simple_utils.Trace
module Errors = Errors
open Errors
module Signature = Context.Signature
module Elaboration = Context.Elaboration
module O = Ast_typed
open O.Combinators
open Ligo_prim


let default_entrypoint = "main"
let default_entrypoint_var = Value_var.of_input_var default_entrypoint

let get_entry_point_from_annotation : Ast_typed.module_ -> Value_var.t list =
  fun prg ->
  List.fold_right
    ~f:(fun el prev ->
      match el.wrap_content with
      | Ast_typed.D_value {binder;attr;_} when attr.entry -> (Binder.get_var binder)::prev
      | _ -> prev)
    ~init:[] prg

let get_entry_point_from_annotation_sig_ : Signature.t -> Value_var.t list =
  fun prg ->
  List.fold_right
    ~f:(fun el prev ->
      match el with
      | Signature.S_value (var, _, entry) when entry -> var::prev
      | _ -> prev)
    ~init:[] prg

let create_entrypoint_function_expr entrypoints entrypoint_type storage =
  let open Ast_typed in
  let p_var = (Value_var.of_input_var "p") in
  let s_var = (Value_var.of_input_var "s") in
  let p = e_a_variable p_var entrypoint_type in
  let s = e_a_variable s_var storage in
  let params = Value_var.fresh ~name:"param" () in
  let fields = Record.LMap.of_list
    [(Label "0", Location.wrap @@ Pattern.(P_var (Binder.make p_var entrypoint_type)));
     (Label "1", Location.wrap @@ Pattern.(P_var (Binder.make s_var storage)))] in
  let param_storage = e_a_pair p s in
  let oplst_storage = t_pair (t_list @@ t_operation ()) storage in
  let fun_type = t_arrow param_storage.type_expression oplst_storage () in
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

let program ~raise : Signature.t -> Ast_typed.module_ -> (Ast_typed.declaration * Ast_typed.declaration) option =
  fun sig_ module_ ->
  ignore raise;
  let annoted_entry_points = get_entry_point_from_annotation module_ in
  let get_entry_point_type e =
    let e_type, _ =
        trace_option ~raise (unbound_variable e Location.generated)
          @@ Signature.get_value sig_ e in
    match e_type.type_content with
    | T_arrow {type1 ; type2} -> (
        match Ast_typed.Combinators.(get_t_pair type1 , get_t_pair type2) with
        | Some (parameter,storage) , Some (listop, storage') ->
          let () = trace_option ~raise (corner_case "foo") @@
            Ast_typed.assert_t_list_operation listop in
          let () = trace_option ~raise (corner_case "bar") @@
            Ast_typed.assert_type_expression_eq (storage,storage') in
          (* TODO: on storage/parameter : asert_storable, assert_passable ? *)
          parameter, storage
        |  _ -> raise.error @@ corner_case "jeje"
      )
    | _ -> raise.error @@ corner_case "jojo" in
  match annoted_entry_points with
  | [] -> None
  | (entrypoint :: rest) ->
    let parameter, storage = get_entry_point_type entrypoint in
    let parameter_list = List.fold ~init:[Value_var.to_name_exn entrypoint,parameter] ~f:(fun parameters ep ->
       let parameter_, storage_ = get_entry_point_type ep in
       let () = trace_option ~raise (corner_case "Storage types do not match") @@
         Ast_typed.assert_type_expression_eq (storage_,storage) in
       (Value_var.to_name_exn ep, parameter_)::parameters) rest in
    let entrypoint_type = Ast_typed.t_sum_ez ~layout:O.default_layout parameter_list in
    let type_binder = Type_var.fresh ~name:"parameter" () in
    let entrypoint_type_decl = Location.wrap @@ Ast_typed.D_type {type_binder;type_expr=entrypoint_type;type_attr={public=true;hidden=false}} in
    let entrypoint_function_decl =
      let expr = create_entrypoint_function_expr (entrypoint::rest) entrypoint_type storage in
      let binder = Binder.make default_entrypoint_var (Some expr.type_expression) in
      Location.wrap @@ Ast_typed.D_value {binder;expr;attr={inline=false;no_mutation=false;entry=false;view=false;public=true;thunk=false;hidden=false}}
    in
    Some (entrypoint_type_decl, entrypoint_function_decl)

let program_sig_ ~raise : Signature.t -> (Signature.item * Signature.item) option =
  fun sig_ ->
  ignore raise;
  let annoted_entry_points = get_entry_point_from_annotation_sig_ sig_ in
  let get_entry_point_type e =
    let e_type, _ =
        trace_option ~raise (unbound_variable e Location.generated)
          @@ Signature.get_value sig_ e in
    match e_type.type_content with
    | T_arrow {type1 ; type2} -> (
        match Ast_typed.Combinators.(get_t_pair type1 , get_t_pair type2) with
        | Some (parameter,storage) , Some (listop, storage') ->
          let () = trace_option ~raise (corner_case "Expected (operation list)") @@
            Ast_typed.assert_t_list_operation listop in
          let () = trace_option ~raise (corner_case "Expected equal store") @@
            Ast_typed.assert_type_expression_eq (storage,storage') in
          (* TODO: on storage/parameter : asert_storable, assert_passable ? *)
          parameter, storage
        |  _ -> raise.error @@ corner_case "Expected pairs"
      )
    | _ -> raise.error @@ corner_case "Expected arrow" in
  match annoted_entry_points with
  | [] -> None
  | (entrypoint :: rest) ->
    let parameter, storage = get_entry_point_type entrypoint in
    let parameter_list = List.fold ~init:[Value_var.to_name_exn entrypoint,parameter] ~f:(fun parameters ep ->
       let parameter_, storage_ = get_entry_point_type ep in
       let () = trace_option ~raise (corner_case "Storage types do not match") @@
         Ast_typed.assert_type_expression_eq (storage_,storage) in
       (Value_var.to_name_exn ep, parameter_)::parameters) rest in
    let entrypoint_type = Ast_typed.t_sum_ez ~layout:O.default_layout parameter_list in
    let type_binder = Type_var.fresh ~name:"parameter" () in
    let entrypoint_type_decl = Signature.S_type (type_binder, entrypoint_type) in
    let expr = create_entrypoint_function_expr (entrypoint::rest) entrypoint_type storage in
    let entrypoint_function_decl = Signature.S_value (default_entrypoint_var, expr.type_expression, false) in
    Some (entrypoint_type_decl, entrypoint_function_decl)

let make_main_module ~raise sig_ (module_ : O.module_expr) =
  match Location.unwrap module_ with
  | M_struct ds -> (
    match program ~raise sig_ ds with
    | None -> module_
    | Some (type_decl, main_decl) ->
      Location.wrap @@ Module_expr.M_struct (ds @ [type_decl; main_decl])
  )
  | _ -> module_

let make_main_sig_ ~raise (sig_ : Signature.t) =
  match program_sig_ ~raise sig_ with
  | None -> sig_
  | Some (type_decl, main_decl) -> sig_ @ [type_decl; main_decl]

let make_main ~raise ~options ~ctx sig_ (module_ : (O.module_expr, typer_error, Main_warnings.all) Elaboration.t) =
  let open Elaboration.Let_syntax in
  ignore options; ignore raise; ignore ctx;
  let sig_ = make_main_sig_ ~raise sig_ in
  ctx, sig_,
  let%bind module_ = module_ in
  let module_ = make_main_module ~raise sig_ module_ in
  return @@ module_
