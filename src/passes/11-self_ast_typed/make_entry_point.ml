open Simple_utils.Trace
open Errors
open Ligo_prim

let default_entrypoint = "main"
let default_entrypoint_var = Value_var.of_input_var default_entrypoint

let get_entry_point_from_annotation : Ast_typed.program -> Value_var.t list =
  fun prg ->
  List.fold_right
    ~f:(fun el prev ->
      let open Simple_utils.Location in
      match el.wrap_content with
      | Ast_typed.D_value {binder;attr;_} when attr.entry -> (Binder.get_var binder)::prev
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

let make_main_entrypoint ?(layout=Ast_typed.default_layout) ~raise :  Ast_typed.expression_variable Simple_utils.List.Ne.t -> Ast_typed.program -> Ast_typed.expression_variable * Ast_typed.program = fun entrypoints prg ->
  match entrypoints with
    (entrypoint,[]) -> entrypoint, prg
  | (entrypoint,rest) ->
    let Helpers.{parameter;storage} = Helpers.fetch_contract_type ~raise entrypoint prg in
    let parameter_list = List.fold ~init:[Value_var.to_name_exn entrypoint,parameter] ~f:(fun parameters ep ->
      let Helpers.{parameter;storage=str} = Helpers.fetch_contract_type ~raise ep prg in
      let () = trace_option ~raise (storage_entrypoint_contract (Value_var.get_location ep) ep str entrypoint storage ) @@
        Ast_typed.assert_type_expression_eq (str,storage) in
      (Value_var.to_name_exn ep, parameter)::parameters) rest in
    let entrypoint_type = Ast_typed.t_sum_ez ~layout parameter_list in
    let type_binder = Type_var.fresh ~name:"parameter" () in
    let entrypoint_type_decl = Location.wrap @@ Ast_typed.D_type {type_binder;type_expr=entrypoint_type;type_attr={public=true;hidden=false}} in
    let entrypoint_function_decl =
      let expr = create_entrypoint_function_expr (entrypoint::rest) entrypoint_type storage in
      let binder = Binder.make default_entrypoint_var (Some expr.type_expression) in
      Location.wrap @@ Ast_typed.D_value {binder;expr;attr={inline=false;no_mutation=false;entry=false;view=false;public=true;thunk=false;hidden=false}}
    in
    let prg = prg @ [entrypoint_type_decl;entrypoint_function_decl] in
    (default_entrypoint_var, prg)

let program ~raise : Ast_typed.expression_variable list -> Ast_typed.program -> Ast_typed.expression_variable * Ast_typed.program =
  fun entry_points prg ->
    let annoted_entry_points = get_entry_point_from_annotation prg in
    match entry_points,annoted_entry_points with
    (* First make the entrypoint from the provided list *)
    | hd::tl,_ ->
      let () = List.iter ~f:(fun var ->
        if Option.is_none (List.find ~f:(fun s -> Value_var.equal var s) entry_points) then
          raise.warning (`Main_entry_ignored (Value_var.get_location var))) annoted_entry_points  in
      make_main_entrypoint ~raise (hd,tl) prg
    (* Second from annotations *)
    | [], hd::tl -> make_main_entrypoint ~raise (hd,tl) prg
    | [], [] -> default_entrypoint_var, prg

let get_final_entrypoint_name ~raise : Ast_typed.expression_variable list -> Ast_typed.program -> Ast_typed.expression_variable =
  fun entry_points prg ->
    let annoted_entry_points = get_entry_point_from_annotation prg in
    match entry_points,annoted_entry_points with
    | hd::[],lst ->
      let () = List.iter ~f:(fun var ->
        if not (Value_var.equal var hd) then
          raise.warning (`Main_entry_ignored (Value_var.get_location var))) lst in
      hd
    | [], hd::[] -> hd
    | [], _ -> default_entrypoint_var
    | lst, _ ->
      let () = List.iter ~f:(fun var ->
        if Option.is_none (List.find ~f:(fun s -> Value_var.equal var s) lst) then
          raise.warning (`Main_entry_ignored (Value_var.get_location var))) annoted_entry_points  in
      default_entrypoint_var
