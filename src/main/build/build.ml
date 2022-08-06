open Simple_utils
open Trace
open Main_errors

module Stdlib = Stdlib
module Source_input = BuildSystem.Source_input

module type Params = sig
  val raise : (all, Main_warnings.all) raise
  val options : Compiler_options.t
end

module M (Params : Params) =
  struct
    let raise = Params.raise
    let options = Params.options
    type file_name = Source_input.file_name
    type raw_input = Source_input.raw_input
    type code_input = Source_input.code_input
    type module_name = string
    type compilation_unit = Buffer.t
    type meta_data = Ligo_compile.Helpers.meta

    let preprocess : code_input -> compilation_unit * meta_data * (file_name * module_name) list =
      fun code_input ->
      let syntax = Syntax.of_string_opt ~raise (Syntax_name "auto") (match code_input with From_file file_name -> Some file_name | Raw {id ; _} -> Some id) in
      let meta = Ligo_compile.Of_source.extract_meta syntax in
      let c_unit, deps = match code_input with
        | From_file file_name -> Ligo_compile.Helpers.preprocess_file ~raise ~meta ~options:options.frontend file_name
        | Raw {id = _ ; code} -> Ligo_compile.Helpers.preprocess_string ~raise ~meta ~options:options.frontend code
      in
      c_unit,meta,deps
    module AST = struct
      type declaration = Ast_typed.declaration
      type t = declaration list
      type environment = Environment.t
      let add_ast_to_env : t -> environment -> environment = fun ast env ->
        Environment.append ast env
      let add_module_to_env : module_name -> environment -> environment -> environment =
        fun module_name ast_typed_env env ->
          let module_name = Ast_typed.ModuleVar.of_input_var module_name in
          Environment.add_module ~public:() module_name (Environment.to_program ast_typed_env) env
      let init_env : environment = options.middle_end.init_env
      let make_module_declaration : module_name -> t -> declaration =
        fun module_binder ast_typed ->
        let module_ = Location.wrap (Ast_typed.M_struct ast_typed) in
        let module_binder = Ast_typed.ModuleVar.of_input_var module_binder in
        Location.wrap Ast_typed.(Declaration_module {module_binder;module_;module_attr={public=true;hidden=true}})
    end
    let compile : AST.environment -> file_name -> meta_data -> compilation_unit -> AST.t =
      fun env file_name meta c_unit ->
      let options = Compiler_options.set_init_env options env in
      let stdlib = Stdlib.typed ~options meta.syntax in
      let options = Compiler_options.set_init_env options (Environment.append stdlib env) in
      let ast_core = Ligo_compile.Utils.to_core ~raise ~options ~meta c_unit file_name in
      let ast_core =
        let syntax = Syntax.of_string_opt ~raise (Syntax_name "auto") (Some file_name) in
        Helpers.inject_declaration ~options ~raise syntax ast_core
      in
      Ligo_compile.Of_core.typecheck ~raise ~options Ligo_compile.Of_core.Env ast_core

  end

module Infer (Params : Params) = struct
  include M(Params)
  module AST = struct
    include AST
    type declaration = Ast_core.declaration
    type t = declaration list
      type environment = Environment.core
      let add_ast_to_env : t -> environment -> environment = fun ast env ->
        Environment.append_core ast env
      let add_module_to_env : module_name -> environment -> environment -> environment =
        fun module_name ast_typed_env env ->
          let module_name = Ast_core.ModuleVar.of_input_var module_name in
          Environment.add_core_module ~public:() module_name (Environment.to_core_program ast_typed_env) env
      let init_env : environment = Environment.init_core @@ Checking.untype_program @@ Environment.to_program @@ options.middle_end.init_env
      let make_module_declaration : module_name -> t -> declaration =
        fun module_binder ast_typed ->
        let module_ = Location.wrap (Ast_core.M_struct ast_typed) in
        let module_binder = Ast_core.ModuleVar.of_input_var module_binder in
        Location.wrap Ast_core.(Declaration_module {module_binder;module_;module_attr={public=true;hidden=true}})
  end

  let compile : AST.environment -> file_name -> meta_data -> compilation_unit -> AST.t =
    fun _ file_name meta c_unit ->
    let stdlib =  Stdlib.core ~options meta.syntax in
    let module_ = Ligo_compile.Utils.to_core ~raise ~options ~meta c_unit file_name in
    let module_ =
      let syntax = Syntax.of_string_opt ~raise (Syntax_name "auto") (Some file_name) in
      Helpers.inject_declaration ~options ~raise syntax module_
    in
    stdlib @ module_

end

module Build(Params : Params) = BuildSystem.Make(M(Params))


let dependency_graph ~raise : options:Compiler_options.t -> Ligo_compile.Of_core.form -> Source_input.file_name -> _ =
  fun ~options _form file_name ->
    let open Build(struct
      let raise = raise
      let options = options
    end) in
    dependency_graph (Source_input.From_file file_name)

let infer_contract ~raise : options:Compiler_options.t -> Source_input.file_name -> Ast_core.module_ =
  fun ~options main_file_name ->
    let open BuildSystem.Make(Infer(struct
      let raise = raise
      let options = options
    end)) in
    trace ~raise build_error_tracer @@ from_result (compile_separate (Source_input.From_file main_file_name))

let type_contract ~raise : options:Compiler_options.t -> Source_input.file_name -> _ =
  fun ~options file_name ->
    let open Build(struct
      let raise = raise
      let options = options
    end) in
    trace ~raise build_error_tracer @@ from_result (compile_separate (Source_input.From_file file_name))

let merge_and_type_libraries ~raise : options:Compiler_options.t -> Source_input.file_name -> Ast_typed.program =
  fun ~options file_name ->
    let open BuildSystem.Make(Infer(struct
      let raise = raise
      let options = options
    end)) in
    let contract = trace ~raise build_error_tracer @@ from_result (compile_combined (Source_input.From_file file_name)) in
    let contract = Ligo_compile.Of_core.typecheck ~raise ~options Env contract in
    contract

let merge_and_type_libraries_str ~raise : options:Compiler_options.t -> string -> Ast_typed.program =
  fun ~options code ->
    let open BuildSystem.Make(Infer(struct
      let raise = raise
      let options = options
    end)) in
    let id = match options.frontend.syntax with Some s -> "from_build"^(Syntax.to_ext s) | None -> "from_build" in
    let s = Source_input.Raw { code = code ; id } in
    let contract = trace ~raise build_error_tracer @@ from_result (compile_combined s) in
    let contract = Ligo_compile.Of_core.typecheck ~raise ~options Env contract in
    contract
    
let build_typed ~raise :
  options:Compiler_options.t -> Ligo_compile.Of_core.form -> Source_input.file_name -> Ast_typed.program =
    fun ~options form file_name ->
      let open Build(struct
        let raise = raise
        let options = options
      end) in
      let contract = merge_and_type_libraries ~raise ~options file_name in
      trace ~raise self_ast_typed_tracer @@ Ligo_compile.Of_core.specific_passes form contract

let build_expression ~raise : options:Compiler_options.t -> Syntax_types.t -> string -> Source_input.file_name option -> _ =
  fun ~options syntax expression file_name ->
    let contract, aggregated_prg =
      match file_name with
      | Some init_file_name ->
         let module_ = merge_and_type_libraries ~raise ~options init_file_name in
         let contract = Ligo_compile.Of_typed.compile_program ~raise module_ in
         (module_, contract)
      | None ->
         let stdlib   = Stdlib.typed ~options syntax in
         let contract = Ligo_compile.Of_typed.compile_program ~raise stdlib in
         (stdlib, contract)
    in
    let typed_exp       = Ligo_compile.Utils.type_expression ~raise ~options syntax expression contract in
    let aggregated      = Ligo_compile.Of_typed.compile_expression_in_context ~raise ~options:options.middle_end typed_exp aggregated_prg in
    let mini_c_exp      = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    (mini_c_exp ,aggregated)

(* TODO: this function could be called build_michelson_code since it does not really reflect a "contract" (no views, parameter/storage types) *)
let build_contract ~raise : options:Compiler_options.t -> string -> Source_input.file_name -> Stacking.compiled_expression =
  fun ~options entry_point file_name ->
    let entry_point = Ast_typed.ValueVar.of_input_var entry_point in
    let typed_prg = build_typed ~raise ~options (Ligo_compile.Of_core.Contract entry_point) file_name in
    let aggregated = Ligo_compile.Of_typed.apply_to_entrypoint_contract ~raise ~options:options.middle_end typed_prg entry_point in
    let (parameter_ty, storage_ty) =
    trace_option ~raise (`Self_ast_aggregated_tracer (Self_ast_aggregated.Errors.corner_case "Could not recover types from contract")) (
      let open Option in
      let open Ast_aggregated in
      let* { type1 = input_ty ; _ }= Ast_aggregated.get_t_arrow aggregated.type_expression in
      Ast_aggregated.get_t_pair input_ty
    )
    in
    let aggregated = trace ~raise self_ast_aggregated_tracer @@ Self_ast_aggregated.all_contract parameter_ty storage_ty aggregated in
    let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c

module ValueVar = Stage_common.Types.ValueVar
module LMap = Stage_common.Types.LMap
module DepGraph = Caml.Map.Make(ValueVar)
module DepSet = Caml.Set.Make(ValueVar)
type dep_graph = DepSet.t DepGraph.t
let rec build_dep_graph (ast : Ast_aggregated.expression) (g : dep_graph) : dep_graph =
  match ast.expression_content with
    E_let_in { let_binder ; rhs ; let_result ; _ } ->
      let vars = DepSet.of_list (find_vars rhs) in
      let g = DepGraph.add let_binder.var vars g in
      build_dep_graph let_result g
  | E_literal _ -> g
  | E_variable _ -> g
  | E_constant { arguments ; _ } -> List.fold_left arguments ~init:g ~f:(fun g e -> build_dep_graph e g)
  | E_application { lamb ; args } -> 
    let g = build_dep_graph lamb g in
    let g = build_dep_graph args g in
    g
  | E_lambda { result ; _ } -> build_dep_graph result g
  | E_recursive { lambda = { result ; _ } ; _ } -> build_dep_graph result g
  | E_raw_code { code ; _ } -> build_dep_graph code g
  | E_type_inst { forall ; _ } -> build_dep_graph forall g
  | E_constructor { element ; _ } -> build_dep_graph element g
  | E_matching { matchee ; cases } -> let g = build_dep_graph matchee g in
    let g = (match cases with
      Match_variant { cases ; _ } -> List.fold_left cases ~init:g ~f:(fun g e -> build_dep_graph e.body g)
    | Match_record { body } -> build_dep_graph body g
    ) in
    g
  | E_record m -> let vs = LMap.to_list m in List.fold_left vs ~init:g ~f:(fun g e -> build_dep_graph e g)
  | E_record_accessor { record ; _ } -> build_dep_graph record g
  | E_record_update { record ; update ; _ } -> 
    let g = build_dep_graph record g in
    let g = build_dep_graph update g in
    g
  | E_assign { expression ; _ } -> build_dep_graph expression g
  | E_type_abstraction { result ; _ } -> build_dep_graph result g
and find_vars (e : Ast_aggregated.expression) =
  match e.expression_content with
    E_literal _ -> []
  | E_variable e -> [e]
  | E_constant { arguments ; _ } -> List.fold_left arguments ~init:[] ~f:(fun xs e -> xs @ find_vars e)
  | E_application { lamb ; args } -> find_vars lamb @ find_vars args
  | E_lambda { result ; _ } -> find_vars result
  | E_recursive { fun_name ; lambda = { binder ; result } ; _ } -> [fun_name] @ find_vars result
  | E_let_in { let_binder ; rhs ; let_result } -> [let_binder.var] @ find_vars rhs @ find_vars let_result
  | E_raw_code { code ; _ } -> find_vars code
  | E_type_inst { forall ; _ } -> find_vars forall
  | E_constructor { element ; _ } -> find_vars element
  | E_matching { matchee ; cases } -> let vs = find_vars matchee in
    let ms = (match cases with
      Match_variant { cases ; _ } -> List.fold_left cases ~init:[] ~f:(fun xs e -> xs @ find_vars e.body)
    | Match_record { body } -> find_vars body
    ) in
    vs @ ms
  | E_record m -> let vs = LMap.to_list m in List.fold_left vs ~init:[] ~f:(fun xs e -> xs @ find_vars e)
  | E_record_accessor { record ; _ } -> find_vars record
  | E_record_update { record ; update ; _ } -> find_vars record @ find_vars update
  | E_assign { binder ; expression } -> find_vars expression @ [binder.var]
  | E_type_abstraction { result ; _ } -> find_vars result
and pp_dep_grph g =
    DepGraph.iter (fun v s -> Format.printf "%a *** " ValueVar.pp v; pp_dep_set s; Format.printf "\n---------------\n") g 
and pp_dep_set s =
    DepSet.iter (fun v -> Format.printf "%a, " ValueVar.pp v) s
and find_reachable g v : DepSet.t =
    let s = DepGraph.find_opt v g in
    match s with
    | None -> DepSet.singleton v
    | Some s ->
    let ss = DepSet.fold (fun v ss -> DepSet.union ss (find_reachable g v)) s (DepSet.singleton v) in
    DepSet.union s ss
and tree_shake (e : Ast_aggregated.expression) s =
  match e.expression_content with
    E_literal _ -> e
  | E_variable _ -> e
  | E_constant _ -> e
  | E_application _ -> e
  | E_lambda _ -> e
  | E_recursive _ -> e
  | E_let_in { let_binder ; rhs ; let_result } ->
    (* TODO this is not proper ... *)
    if DepSet.mem let_binder.var s then e else tree_shake let_result s
  | E_raw_code _ -> e
  | E_type_inst _ -> e
  | E_constructor _ -> e
  | E_matching _ -> e
  | E_record _ -> e
  | E_record_accessor _ -> e
  | E_record_update _ -> e
  | E_assign _ -> e
  | E_type_abstraction _ -> e

let build_views ~raise :
  options:Compiler_options.t -> string -> string list -> Source_input.file_name -> (Ast_typed.ValueVar.t * Stacking.compiled_expression) list =
  fun ~options main_name cli_views source_file ->
    let form =
      let contract_entry = Ast_typed.ValueVar.of_input_var main_name in
      let command_line_views = match cli_views with [] -> None | x -> Some x in
      Ligo_compile.Of_core.View { command_line_views ; contract_entry }
    in
    let contract =
      let warning : Main_warnings.all -> unit = fun x -> match x with `Main_view_ignored _ -> raise.warning x | _ -> () in
      build_typed ~raise:{raise with warning} ~options form source_file
    in
    let view_names = List.map ~f:fst (Ast_typed.Helpers.get_views contract) in
    match view_names with
    | [] -> []
    | _ ->
      (* print_endline "AAAAAAAAAAA"; *)
      let aggregated = Ligo_compile.Of_typed.apply_to_entrypoint_view ~raise:{raise with warning = fun _ -> ()} ~options:options.middle_end contract in
      let g = build_dep_graph aggregated DepGraph.empty in
      (* pp_dep_grph g; *)
      let nb_of_views = List.length view_names in
      let aux i view =
        let ss = find_reachable g view in
        let aggregated = tree_shake aggregated ss in
        (* Format.printf "%a" Ast_aggregated.PP.expression aggregated; *)
        let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
        let mini_c = trace ~raise self_mini_c_tracer @@ Self_mini_c.all_expression options mini_c in
        let mini_c_tys = trace_option ~raise (`Self_mini_c_tracer (Self_mini_c.Errors.corner_case "Error reconstructing type of views")) @@
                          Mini_c.get_t_tuple mini_c.type_expression in
        let idx_ty = trace_option ~raise (`Self_mini_c_tracer (Self_mini_c.Errors.corner_case "Error reconstructing type of view")) @@
                      List.nth mini_c_tys i in
        let idx = Mini_c.e_proj mini_c idx_ty i nb_of_views in
        (* pp_dep_set ss; *)
        (* let idx = trace ~raise self_mini_c_tracer @@ Self_mini_c.all_expression options idx in *)
        (view, idx) in
      let views = List.mapi ~f:aux view_names in
      let aux (vn, mini_c) = (vn, Ligo_compile.Of_mini_c.compile_view ~raise ~options mini_c) in
      let michelsons = List.map ~f:aux views in
      let () = Ligo_compile.Of_michelson.check_view_restrictions ~raise (List.map ~f:snd michelsons) in
      michelsons
