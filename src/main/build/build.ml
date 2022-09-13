open Simple_utils
open Trace
open Main_errors
open Ligo_prim

module Stdlib = Stdlib
module Source_input = BuildSystem.Source_input

module type Params = sig
  val raise : (all, Main_warnings.all) raise
  val options : Compiler_options.t
  val std_lib : Stdlib.t
end

module M (Params : Params) =
  struct
    let raise = Params.raise
    let options = Params.options
    let std_lib = Params.std_lib
    type file_name = Source_input.file_name
    type raw_input = Source_input.raw_input
    type code_input = Source_input.code_input
    type module_name = string
    type compilation_unit = Buffer.t
    type meta_data = Ligo_compile.Helpers.meta

    let preprocess : code_input -> compilation_unit * meta_data * (file_name * module_name) list =
      fun code_input ->
      let syntax = Syntax.of_string_opt ~raise (Syntax_name "auto")
        (match code_input with From_file file_name -> Some file_name | Raw {id ; _} -> Some id) in
      let meta = Ligo_compile.Of_source.extract_meta syntax in
      let c_unit, deps = match code_input with
        | From_file file_name -> Ligo_compile.Helpers.preprocess_file ~raise ~meta ~options:options.frontend file_name
        | Raw {id = _ ; code} -> Ligo_compile.Helpers.preprocess_string ~raise ~meta ~options:options.frontend code
      in
      c_unit,meta,deps
    module AST = struct
      type declaration = Ast_typed.declaration
      type t = Ast_typed.program
      type environment = Environment.t
      let add_ast_to_env : t -> environment -> environment = fun ast env ->
        Environment.append ast env
      let add_module_to_env : module_name -> environment -> environment -> environment =
        fun module_name ast_typed_env env ->
          let module_name = Module_var.of_input_var module_name in
          Environment.add_module ~public:() module_name (Environment.to_module ast_typed_env) env
      let init_env : environment = options.middle_end.init_env
      let make_module_declaration : module_name -> t -> declaration =
        fun module_binder ast_typed ->
        let module_ = Location.wrap (Module_expr.M_struct ast_typed) in
        let module_binder = Module_var.of_input_var module_binder in
        Location.wrap Ast_typed.(D_module {module_binder;module_;module_attr={public=true;hidden=true}})
    end

    let curry_uncurry_lib : unit -> AST.t =
      fun () -> std_lib.typed_mod_def

    let compile : AST.environment -> file_name -> meta_data -> compilation_unit -> AST.t =
      fun env file_name meta c_unit ->
      let prelude = Stdlib.select_prelude meta.syntax std_lib in
      let core_c_unit = Ligo_compile.Utils.to_core ~raise ~options ~meta c_unit file_name in
      let ast_core =
        let syntax = Syntax.of_string_opt ~raise (Syntax_name "auto") (Some file_name) in
        (* REMITODO why not meta.syntax here ? *)
        Helpers.inject_declaration ~options ~raise syntax core_c_unit
      in
      let ast_core = prelude @ ast_core in
      let options = Compiler_options.set_init_env options
        (Environment.append std_lib.typed_mod_def env) in
      Ligo_compile.Of_core.typecheck ~raise ~options Ligo_compile.Of_core.Env ast_core

  end

(* TODO: Why "infer" ? couldn't call it Core_repr ? *)
module Infer (Params : Params) = struct
  include M(Params)
  module AST = struct
      type declaration = Ast_core.declaration
      type t = Ast_core.program
      type environment = Environment.core
      let add_ast_to_env : t -> environment -> environment = fun ast env ->
        Environment.append_core ast env
      let add_module_to_env : module_name -> environment -> environment -> environment =
        fun module_name ast_typed_env env ->
          let module_name = Module_var.of_input_var module_name in
          Environment.add_core_module ~public:() module_name (Environment.to_core_module ast_typed_env) env
      let init_env : environment = Environment.init_core @@ Checking.untype_program @@ Environment.to_program @@ options.middle_end.init_env
      let make_module_declaration : module_name -> t -> declaration =
        fun module_binder ast_typed ->
        let module_ = Location.wrap (Module_expr.M_struct ast_typed) in
        let module_binder = Module_var.of_input_var module_binder in
        Location.wrap Ast_core.(D_module {module_binder;module_;module_attr={public=true;hidden=true}})
  end

  let curry_uncurry_lib : unit -> AST.t =
    fun () -> std_lib.core_mod_def

  let compile : AST.environment -> file_name -> meta_data -> compilation_unit -> AST.t =
    fun _ file_name meta c_unit ->
      let prelude = Stdlib.select_prelude meta.syntax std_lib in
      let module_ = Ligo_compile.Utils.to_core ~raise ~options ~meta c_unit file_name in
      let module_ =
        let syntax = Syntax.of_string_opt ~raise (Syntax_name "auto") (Some file_name) in
        Helpers.inject_declaration ~options ~raise syntax module_
      in
      std_lib.core_mod_def @ prelude @ module_

end

module Build_typed(Params : Params) = BuildSystem.Make(M(Params))
module Build_core(Params : Params) = BuildSystem.Make(Infer(Params))

let dependency_graph ~raise : options:Compiler_options.t -> Ligo_compile.Of_core.form -> Source_input.file_name -> _ =
  fun ~options _form file_name ->
    let open Build_typed(struct
      let raise = raise
      let options = options
      let std_lib = Stdlib.get ~options
    end) in
    dependency_graph (Source_input.From_file file_name)

let unqualified_core ~raise : options:Compiler_options.t -> Source_input.file_name -> Ast_core.program =
  fun ~options main_file_name ->
    let std_lib = Stdlib.get ~options in
    let open Build_core(struct
      let raise = raise
      let options = options
      let std_lib = std_lib
    end) in
    trace ~raise build_error_tracer @@ from_result (compile_unqualified (Source_input.From_file main_file_name))

let unqualified_typed ~raise : options:Compiler_options.t -> Ligo_compile.Of_core.form -> Source_input.file_name -> Ast_typed.program =
  fun ~options form file_name ->
    let open Build_typed(struct
      let raise = raise
      let options = options
      let std_lib = Stdlib.get ~options
    end) in
    let x = trace ~raise build_error_tracer @@ from_result (compile_unqualified (Source_input.From_file file_name)) in
    trace ~raise self_ast_typed_tracer @@ Ligo_compile.Of_core.specific_passes form x

let qualified_core ~raise : options:Compiler_options.t -> Source_input.file_name -> Ast_core.program =
  fun ~options file_name ->
    let open Build_core(struct
      let raise = raise
      let options = options
      let std_lib = Stdlib.get ~options
    end) in
    trace ~raise build_error_tracer @@ from_result (compile_qualified (Source_input.From_file file_name))

let qualified_typed ~raise : options:Compiler_options.t -> Ligo_compile.Of_core.form -> Source_input.file_name -> Ast_typed.program =
  fun ~options form file_name ->
    let open Build_typed(struct
      let raise = raise
      let options = options
      let std_lib = Stdlib.get ~options
    end) in
    let prg = trace ~raise build_error_tracer @@ from_result (compile_qualified (Source_input.From_file file_name)) in
    let prg = trace ~raise self_ast_typed_tracer @@ Ligo_compile.Of_core.specific_passes form prg in
    prg

let merge_and_type_libraries_str ~raise : options:Compiler_options.t -> string -> Ast_typed.program =
  fun ~options code ->
    let std_lib = Stdlib.get ~options in
    let open Build_core(struct
      let raise = raise
      let options = options
      let std_lib = std_lib
    end) in
    let id = match options.frontend.syntax with Some s -> "from_build"^(Syntax.to_ext s) | None -> "from_build" in
    let s = Source_input.Raw { code = code ; id } in
    let contract = trace ~raise build_error_tracer @@ from_result (compile_qualified s) in
    let contract = Ligo_compile.Of_core.typecheck ~raise ~options Env contract in
    std_lib.typed_mod_def @ contract

let build_expression ~raise : options:Compiler_options.t -> Syntax_types.t -> string -> Source_input.file_name option -> _ =
  fun ~options syntax expression file_name_opt ->
    let init_prg =
      let f : Source_input.file_name -> Ast_typed.program = fun filename ->
        qualified_typed ~raise ~options Env filename
      in
      let default = Stdlib.select_lib_typed syntax (Stdlib.get ~options) in
      Option.value_map file_name_opt ~f ~default:default
    in
    let typed_exp  = Ligo_compile.Utils.type_expression ~raise ~options syntax expression init_prg in
    let init_prg   = Ligo_compile.Of_typed.compile_program ~raise init_prg in
    let aggregated = Ligo_compile.Of_typed.compile_expression_in_context ~raise ~options:options.middle_end typed_exp init_prg in
    let mini_c_exp = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    (mini_c_exp ,aggregated)

let build_aggregated ~raise : options:Compiler_options.t -> string -> Source_input.file_name -> Ast_aggregated.expression =
  fun ~options entry_point file_name ->
    let entry_point = Value_var.of_input_var entry_point in
    let typed_prg = qualified_typed ~raise ~options (Ligo_compile.Of_core.Contract entry_point) file_name in
    let aggregated = Ligo_compile.Of_typed.apply_to_entrypoint_contract ~raise ~options:options.middle_end typed_prg entry_point in
    let (parameter_ty, storage_ty) =
    trace_option ~raise (`Self_ast_aggregated_tracer (Self_ast_aggregated.Errors.corner_case "Could not recover types from contract")) (
      let open Option in
      let open Ast_aggregated in
      let* { type1 = input_ty ; _ }= Ast_aggregated.get_t_arrow aggregated.type_expression in
      Ast_aggregated.get_t_pair input_ty
    )
    in
    trace ~raise self_ast_aggregated_tracer @@ Self_ast_aggregated.all_contract parameter_ty storage_ty aggregated

(* TODO: this function could be called build_michelson_code since it does not really reflect a "contract" (no views, parameter/storage types) *)
let build_contract ~raise : options:Compiler_options.t -> string -> Source_input.file_name -> Stacking.compiled_expression =
  fun ~options entry_point file_name ->
    let aggregated = build_aggregated ~raise ~options entry_point file_name in
    let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
    Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c

let build_aggregated_views ~raise :
  options:Compiler_options.t -> string -> string list -> Source_input.file_name -> (Value_var.t list * Ast_aggregated.expression) option =
  fun ~options main_name cli_views source_file ->
    let form =
      let contract_entry = Value_var.of_input_var main_name in
      let command_line_views = match cli_views with [] -> None | x -> Some x in
      Ligo_compile.Of_core.View { command_line_views ; contract_entry }
    in
    let contract =
      let warning : Main_warnings.all -> unit = fun x -> match x with `Main_view_ignored _ -> raise.warning x | _ -> () in
      qualified_typed ~raise:{raise with warning} ~options form source_file
    in
    let view_names = List.map ~f:fst (Ast_typed.Helpers.get_views contract) in
    match view_names with
    | [] -> None
    | _ ->
      let contract = trace ~raise self_ast_typed_tracer @@ Self_ast_typed.remove_unused_for_views ~view_names contract in
      let aggregated = Ligo_compile.Of_typed.apply_to_entrypoint_view ~raise:{raise with warning = fun _ -> ()} ~options:options.middle_end contract in
      Some (view_names, aggregated)

let build_views ~raise :
  options:Compiler_options.t -> string -> string list -> Source_input.file_name -> (Value_var.t * Stacking.compiled_expression) list =
  fun ~options main_name cli_views source_file ->
    match build_aggregated_views ~raise ~options main_name cli_views source_file with
    | None -> []
    | Some (view_names, aggregated) ->
      let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
      let mini_c = trace ~raise self_mini_c_tracer @@ Self_mini_c.all_expression options mini_c in
      let mini_c_tys = trace_option ~raise (`Self_mini_c_tracer (Self_mini_c.Errors.corner_case "Error reconstructing type of views")) @@
                        Mini_c.get_t_tuple mini_c.type_expression in
      let nb_of_views = List.length view_names in
      let aux i view =
        let idx_ty = trace_option ~raise (`Self_mini_c_tracer (Self_mini_c.Errors.corner_case "Error reconstructing type of view")) @@
                      List.nth mini_c_tys i in
        let idx = Mini_c.e_proj mini_c idx_ty i nb_of_views in
        (* let idx = trace ~raise self_mini_c_tracer @@ Self_mini_c.all_expression options idx in *)
        (view, idx) in
      let views = List.mapi ~f:aux view_names in
      let aux (vn, mini_c) = (vn, Ligo_compile.Of_mini_c.compile_view ~raise ~options mini_c) in
      let michelsons = List.map ~f:aux views in
      let () = Ligo_compile.Of_michelson.check_view_restrictions ~raise (List.map ~f:snd michelsons) in
      michelsons
