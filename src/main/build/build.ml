
open Simple_utils
open Trace
open Main_errors

module type Params = sig
  val raise : all raise
  val add_warning : Main_warnings.all -> unit
  val options : Compiler_options.t
end

module M (Params : Params) =
  struct
    let raise = Params.raise
    let add_warning = Params.add_warning
    let options = Params.options
    type file_name = string
    type module_name = string
    type compilation_unit = Buffer.t
    type meta_data = Ligo_compile.Helpers.meta
    let preprocess : file_name -> compilation_unit * meta_data * (file_name * module_name) list =
      fun file_name ->
      let meta = Ligo_compile.Of_source.extract_meta ~raise "auto" file_name in
      let c_unit, deps = Ligo_compile.Helpers.preprocess_file ~raise ~meta ~options:options.frontend file_name in
      c_unit,meta,deps
    module AST = struct
      type declaration = Ast_typed.declaration_loc
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
        let module_binder = Ast_typed.ModuleVar.of_input_var module_binder in
        (Location.wrap @@ (Ast_typed.Declaration_module {module_binder;module_=ast_typed;module_attr={public=true}}: Ast_typed.declaration))
      let make_module_alias : module_name -> file_name -> declaration =
        fun module_name file_name ->
        let module_name = Ast_typed.ModuleVar.of_input_var module_name in
        let file_name   = Ast_typed.ModuleVar.of_input_var file_name in
        Location.wrap @@ (Ast_typed.Module_alias {alias=module_name;binders=file_name,[]}: Ast_typed.declaration)
    end
    let compile : AST.environment -> file_name -> meta_data -> compilation_unit -> AST.t =
      fun env file_name meta c_unit ->
      let options = Compiler_options.set_init_env options env in
      let ast_core = Ligo_compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit file_name in
      let ast_typed = Ligo_compile.Of_core.typecheck ~raise ~add_warning ~options Ligo_compile.Of_core.Env ast_core in
      ast_typed

  end

module Infer (Params : Params) = struct
  include M(Params)
  module AST = struct
    include AST
    type declaration = Ast_core.declaration Location.wrap
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
        let module_binder = Ast_core.ModuleVar.of_input_var module_binder in
        (Location.wrap @@ (Ast_core.Declaration_module {module_binder;module_=ast_typed;module_attr={public=true}}: Ast_core.declaration))
      let make_module_alias : module_name -> file_name -> declaration =
        fun module_name file_name ->
        let module_name = Ast_core.ModuleVar.of_input_var module_name in
        let file_name   = Ast_core.ModuleVar.of_input_var file_name in
        Location.wrap @@ (Ast_core.Module_alias {alias=module_name;binders=file_name,[]}: Ast_core.declaration)
  end

  let compile : AST.environment -> file_name -> meta_data -> compilation_unit -> AST.t =
    fun _ file_name meta c_unit ->
    Ligo_compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit file_name

end

module Build(Params : Params) = BuildSystem.Make(M(Params))

type file_name = string

let dependency_graph ~raise ~add_warning : options:Compiler_options.t -> Ligo_compile.Of_core.form -> file_name -> _ =
  fun ~options _form file_name ->
    let open Build(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end) in
    dependency_graph file_name

let infer_contract ~raise ~add_warning : options:Compiler_options.t -> file_name -> Ast_core.module_ =
  fun ~options main_file_name ->
    let open BuildSystem.Make(Infer(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end)) in
    trace ~raise build_error_tracer @@ from_result (compile_separate main_file_name)

let type_contract ~raise ~add_warning : options:Compiler_options.t -> file_name -> _ =
  fun ~options file_name ->
    let open Build(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end) in
    trace ~raise build_error_tracer @@ from_result (compile_separate file_name)

let build_context ~raise ~add_warning : options:Compiler_options.t -> file_name -> Ast_typed.program =
  fun ~options file_name ->
    let open BuildSystem.Make(Infer(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end)) in
    let contract = trace ~raise build_error_tracer @@ from_result (compile_combined file_name) in
    let contract = Ligo_compile.Of_core.typecheck ~raise ~add_warning ~options Env contract in
    contract

let build_typed ~raise ~add_warning :
  options:Compiler_options.t -> Ligo_compile.Of_core.form -> file_name -> Ast_typed.program * Ast_typed.program =
    fun ~options entry_point file_name ->
      let open Build(struct
        let raise = raise
        let add_warning = add_warning
        let options = options
      end) in
      let contract = build_context ~raise ~add_warning ~options file_name in
      let applied =
        match entry_point with
        | Ligo_compile.Of_core.Contract entrypoint ->
          trace ~raise self_ast_typed_tracer @@ Self_ast_typed.all_contract entrypoint contract
        | View (view_name,main_name) ->
          trace ~raise self_ast_typed_tracer @@ Self_ast_typed.all_view view_name main_name contract
        | Env -> contract
      in
      applied, contract

let build_expression ~raise ~add_warning : options:Compiler_options.t -> string -> string -> file_name option -> _ =
  fun ~options syntax expression file_name ->
    let contract, aggregated_prg =
      match file_name with
      | Some init_file ->
         let module_ = build_context ~raise ~add_warning ~options init_file in
         let contract = Ligo_compile.Of_typed.compile_program ~raise module_ in
         (module_, contract)
      | None -> ([], fun x -> Ligo_compile.Of_typed.compile_expression ~raise x)
    in
    let typed_exp       = Ligo_compile.Utils.type_expression ~raise ~options file_name syntax expression contract in
    let aggregated      = Ligo_compile.Of_typed.compile_expression_in_context ~raise typed_exp aggregated_prg in
    let mini_c_exp      = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    (mini_c_exp ,aggregated)

(* TODO: this function could be called build_michelson_code since it does not really reflect a "contract" (no views, parameter/storage types) *)
let build_contract ~raise ~add_warning : options:Compiler_options.t -> string -> file_name -> Stacking.compiled_expression * Ast_typed.program =
  fun ~options entry_point file_name ->
    let entry_point = Ast_typed.ValueVar.of_input_var entry_point in
    let typed_prg, contract = build_typed ~raise ~add_warning ~options (Ligo_compile.Of_core.Contract entry_point) file_name in
    let aggregated = Ligo_compile.Of_typed.apply_to_entrypoint_contract ~raise typed_prg entry_point in
    let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    let michelson  = Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c in
    michelson, contract

let build_wasm_code ~raise ~add_warning : options:Compiler_options.t -> string -> file_name -> unit =
  fun ~options entry_point file_name ->
    let entry_point_orig = entry_point in
    let entry_point = Stage_common.Var.of_input_var entry_point in
    let typed_prg, _contract = build_typed ~raise ~add_warning ~options (Ligo_compile.Of_core.Contract entry_point) file_name in
    let aggregated = Ligo_compile.Of_typed.apply_to_entrypoint_contract ~raise typed_prg entry_point in
    let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    let wasm  = Ligo_compile.Of_wasm.compile_contract ~raise ~options mini_c file_name entry_point_orig in
    let wasm = WasmObjectFile.Encode.encode wasm in
    let channel = Out_channel.create "work_in_progress.wasm" in
    Out_channel.output_string channel wasm;
    Out_channel.close channel; 
    (* link with GMP *)
    Ligo_compile.Of_wasm.link ["vendors/gmp/libgmp.a"; "src/passes/14-wasm/start.wasm"; "vendors/wasi/wasi-sdk-14.0/lib/clang/13.0.0/lib/wasi/libclang_rt.builtins-wasm32.a"; "work_in_progress.wasm"] file_name;
    ()
    (* , contract *)

let build_views ~raise ~add_warning :
  options:Compiler_options.t -> string -> string list * Ast_typed.program -> file_name -> (Ast_typed.ValueVar.t * Stacking.compiled_expression) list =
  fun ~options main_name (declared_views,program) source_file ->
    let main_name = Ast_typed.ValueVar.of_input_var main_name in
    let views =
      let annotated_views = Ligo_compile.Of_typed.get_views @@ program in
      match declared_views with
      | [] -> List.map annotated_views ~f:fst
      | _ -> (
        (* detects whether a declared view (passed with --views command line option) overwrites an annotated view ([@view] let ..)*)
        let () = List.iter annotated_views
          ~f:(fun (x,loc) ->
            if Option.is_none (List.find ~f:(fun s -> Ast_typed.ValueVar.is_name x s) declared_views) then
              add_warning (`Main_view_ignored loc)
          )
        in
        List.map ~f:Ast_typed.ValueVar.of_input_var declared_views
      )
    in
    match views with
    | [] -> []
    | _ ->
    let _, contract  = build_typed ~raise ~add_warning:(fun _ -> ()) ~options (Ligo_compile.Of_core.View (views,main_name)) source_file in
    let aggregated = Ligo_compile.Of_typed.apply_to_entrypoint_view ~raise contract views in
    let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    let mini_c = Self_mini_c.all_expression ~raise mini_c in
    let mini_c_tys = trace_option ~raise (`Self_mini_c_tracer (Self_mini_c.Errors.corner_case "Error reconstructing type of views")) @@
                       Mini_c.get_t_tuple mini_c.type_expression in
    let aux i view =
      let idx_ty = trace_option ~raise (`Self_mini_c_tracer (Self_mini_c.Errors.corner_case "Error reconstructing type of view")) @@
                     List.nth mini_c_tys i in
      let idx = Mini_c.e_proj mini_c idx_ty i (List.length views) in
      let idx = Self_mini_c.all_expression ~raise idx in
      (view, idx) in
    let views = List.mapi ~f:aux views in
    let aux (vn, mini_c) = (vn, Ligo_compile.Of_mini_c.compile_view ~raise ~options mini_c) in
    let michelsons = List.map ~f:aux views in
    michelsons
