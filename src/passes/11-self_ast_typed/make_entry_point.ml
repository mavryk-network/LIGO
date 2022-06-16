open Simple_utils.Trace
open Errors
let default_entrypoint = "main"
let default_entrypoint_var = Ast_typed.ValueVar.of_input_var default_entrypoint

let get_entry_point_from_annotation : Ast_typed.program -> Ast_typed.expression_variable list =
  fun prg ->
  List.fold_left
    ~f:(fun prev el ->
      let open Simple_utils.Location in
      match el.wrap_content with
      | Ast_typed.Declaration_constant {binder;attr;_} when attr.entrypoint -> binder.var::prev
      | _ -> prev)
    ~init:[] prg

let create_entrypoint_function_expr entrypoints entrypoint_type storage =
  let open Ast_typed in
  let p_var = (ValueVar.of_input_var "p") in
  let s_var = (ValueVar.of_input_var "s") in
  let p = e_a_variable p_var entrypoint_type in
  let s = e_a_variable s_var storage in
  let params = ValueVar.fresh ~name:"param" () in
  let fields = LMap.of_list
    [(Label "0", {var=p_var;ascr=Some entrypoint_type;attributes={const_or_var=None}});
     (Label "1", {var=s_var;ascr=Some storage;attributes={const_or_var=None}})] in
  let param_storage = e_a_pair p s in
  let oplst_storage = t_pair (t_list @@ t_operation ()) storage in
  let fun_type = t_arrow param_storage.type_expression oplst_storage () in
  let cases = List.map entrypoints ~f:(fun entrypoint ->
    let constructor = Label (ValueVar.to_name_exn entrypoint) in
    let pattern = ValueVar.fresh ~name:"pattern" () in
    let p = e_a_variable pattern param_storage.type_expression in
    let body = e_a_application
      (e_a_variable entrypoint fun_type)
        (e_a_pair p s)
        oplst_storage in
    {constructor;pattern;body}) in
  let body = e_a_matching p (Match_variant {cases;tv=entrypoint_type}) oplst_storage in
  let result = e_a_matching (e_a_variable params param_storage.type_expression) (Match_record {fields;body;tv=oplst_storage}) oplst_storage  in
  e_lambda {binder={var=params;ascr=Some param_storage.type_expression;attributes={const_or_var=None}};result} fun_type

let make_main_entrypoint ~raise :  Ast_typed.expression_variable Simple_utils.List.Ne.t -> Ast_typed.program -> Ast_typed.expression_variable * Ast_typed.program = fun entrypoints prg ->
  match entrypoints with
    (entrypoint,[]) -> entrypoint, prg
  | (entrypoint,rest) ->
    let Helpers.{parameter;storage} = Helpers.fetch_contract_type ~raise entrypoint prg in
    let parameter_list = List.fold ~init:[Ast_typed.ValueVar.to_name_exn entrypoint,parameter] ~f:(fun parameters ep ->
      let Helpers.{parameter;storage=str} = Helpers.fetch_contract_type ~raise ep prg in
      let () = trace_option ~raise (storage_entrypoint_contract (Ast_typed.ValueVar.get_location ep) ep str entrypoint storage ) @@
        Ast_typed.assert_type_expression_eq (str,storage) in
      (Ast_typed.ValueVar.to_name_exn ep, parameter)::parameters) rest in
    let entrypoint_type = Ast_typed.t_sum_ez ~layout:Ast_typed.L_comb parameter_list in
    let type_binder = Ast_typed.TypeVar.fresh ~name:"parameter" () in
    let entrypoint_type_decl = Location.wrap @@ Ast_typed.Declaration_type {type_binder;type_expr=entrypoint_type;type_attr=Ast_typed.{public=true;hidden=false}} in
    let entrypoint_function_decl =
      let expr = create_entrypoint_function_expr (entrypoint::rest) entrypoint_type storage in
      let binder = Ast_typed.{var=default_entrypoint_var;ascr=Some expr.type_expression;attributes={const_or_var=Some `Const}} in
      Location.wrap @@ Ast_typed.Declaration_constant {binder;expr;attr=Ast_typed.{inline=false;no_mutation=false;entrypoint=false;view=false;public=true;thunk=false;hidden=false}}
    in
    let prg = prg @ [entrypoint_type_decl;entrypoint_function_decl] in
    (default_entrypoint_var, prg)

let program ~raise : Ast_typed.expression_variable list -> Ast_typed.program -> Ast_typed.expression_variable * Ast_typed.program =
  fun entry_points prg ->
    match entry_points with
    (* First make the entrypoint from the provided list *)
    | hd::tl -> make_main_entrypoint ~raise (hd,tl) prg
    (* Second from annotations *)
    | [] -> let entry_points = get_entry_point_from_annotation prg in
      match entry_points with
      | hd::tl -> make_main_entrypoint ~raise (hd,tl) prg
      (* Lastly default to "main" *)
      | [] ->
        default_entrypoint_var, prg

let get_final_entrypoint_name : Ast_typed.expression_variable list -> Ast_typed.program -> Ast_typed.expression_variable =
  fun entry_points prg ->
    match entry_points with
    | hd::[] -> hd
    | [] -> let entry_points = get_entry_point_from_annotation prg in
    (match entry_points with
      | hd::[] -> hd
      (* Lastly default to "main" *)
      | _ -> default_entrypoint_var
    )
    | _ -> default_entrypoint_var
