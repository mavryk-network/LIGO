open Api_helpers
open Simple_utils
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers
module Run = Ligo_run.Of_michelson

let test (raw_options : Compiler_options.raw) source_file display_format () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ligo_interpreter.Formatter.tests_format) get_warnings @@
      fun ~raise ->
      let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
      let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options = Compiler_options.make ~protocol_version ~syntax ~raw_options () in
      let Compiler_options.{ steps ; _ } = options.test_framework in
      let typed   = Build.merge_and_type_libraries ~raise ~add_warning ~options source_file in
      Interpreter.eval_test ~raise ~add_warning ~steps ~options typed

let get_meta_ligo_eq ~raise ~add_warning file =
  let meta_ligo_eq = Run_meta_files.get file in
  Ligo_compile.Utils.core_program_string ~raise ~add_warning CameLIGO meta_ligo_eq 

let dry_run (raw_options : Compiler_options.raw) source_file parameter storage amount balance sender source now display_format () =
    let warning_as_error = raw_options.warning_as_error in
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~warning_as_error ~display_format (Ligo_interpreter.Formatter.cli_format) get_warnings @@
      fun ~raise ->
      let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
      let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options = Compiler_options.make ~protocol_version ~syntax ~test_syntax:CameLIGO ~raw_options () in
      let Compiler_options.{ steps ; _ } = options.test_framework in
      let core_contract = Build.merge_libraries ~raise ~add_warning ~options source_file in


      let attr = Ast_core.{ inline = false ; no_mutation = false ; view = false ; public = true ; thunk = false ; hidden = false } in
      let module_attr = Ast_core.{ public = true ; hidden = false } in
      (* let type_attr = module_attr in *)

      let cli_parameter_m = Ast_core.ModuleVar.of_input_var "CLI_PARAMETERS" in
      (* let cli_parameter_ty_m = Ast_typed.ModuleVar.of_input_var "CLI_PARAMETERS_TY" in *)
      let binder_parameter = Ast_core.ValueVar.of_input_var "parameter" in
      let binder_storage = Ast_core.ValueVar.of_input_var "storage" in
      let binder_filename = Ast_core.ValueVar.of_input_var "filename" in
      let binder_entrypoint = Ast_core.ValueVar.of_input_var "entrypoint" in
      let binder_amount = Ast_core.ValueVar.of_input_var "amount" in
      let binder_balance = Ast_core.ValueVar.of_input_var "balance" in

      let cli_module : Ast_core.declaration =
        let open Ast_core in
        let expr_storage = Ligo_compile.Utils.core_expression_string ~raise ~add_warning syntax storage in
        let expr_parameter = Ligo_compile.Utils.core_expression_string ~raise ~add_warning syntax parameter in
        let mutez_conv (ferr : (string -> Main_errors.all)) (amount: string option) : Ast_core.expression =
          let f : string -> Ast_core.expression = fun str ->
            try
              let z = Z.of_string str in
              Ast_core.(e_some (e_mutez z)) 
            with _ -> raise.raise (ferr str)
          in
          let default = Ast_core.(e_ascription (e_none ()) (t_option (t_mutez ()))) in
          Option.value_map amount ~default ~f
        in
        let lst = [
          Declaration_constant { binder = make_binder binder_parameter  ; attr ; expr = expr_parameter                                               } ;
          Declaration_constant { binder = make_binder binder_storage    ; attr ; expr = expr_storage                                                 } ;
          Declaration_constant { binder = make_binder binder_filename   ; attr ; expr = e_string (Ligo_string.standard source_file)                  } ;
          Declaration_constant { binder = make_binder binder_entrypoint ; attr ; expr = e_string (Ligo_string.standard options.frontend.entry_point) } ;
          Declaration_constant { binder = make_binder binder_amount     ; attr ; expr = mutez_conv (Main_errors.main_invalid_amount) amount                                            } ;
          Declaration_constant { binder = make_binder binder_balance    ; attr ; expr = mutez_conv (Main_errors.main_invalid_balance) balance                                           } ;
          ]
        in
        let module_ = Location.wrap @@ M_struct (List.map ~f:(Location.wrap) lst) in
        let cli_parameters_module = Location.wrap @@ Declaration_module { module_binder = cli_parameter_m ; module_ ; module_attr } in

        (* let binder_parameter_ty = TypeVar.of_input_var "parameter_ty" in
        let binder_storage_ty = TypeVar.of_input_var "storage_ty" in
        let lst =
          [ Declaration_type {type_binder = binder_parameter_ty ; type_expr = expr_parameter.type_expression ; type_attr } ;
            Declaration_type {type_binder = binder_storage_ty   ; type_expr = expr_storage.type_expression ; type_attr } ;
          ]
        in
        let module_ = Location.wrap @@ M_struct (List.map ~f:(Location.wrap) lst) in
        let cli_parameters_ty_module : Ast_typed.module_ =
          [ Location.wrap @@ Declaration_module { module_binder = cli_parameter_ty_m ; module_ ; module_attr } ] in
        List.append [cli_parameters_module] cli_parameters_ty_module *)
        cli_parameters_module
      in
      let cli_entry = core_contract @ cli_module :: get_meta_ligo_eq ~raise ~add_warning "dry_run.mligo" in
      let cli_entry_typed = Ligo_compile.Of_core.typecheck ~raise ~add_warning ~options Env cli_entry in
      let agg = Ligo_compile.Of_typed.apply_to_entrypoint ~raise ~options:options.middle_end cli_entry_typed "test_dry_run" in
      Interpreter.eval ~raise ~add_warning ~steps ~options agg


let interpret (raw_options : Compiler_options.raw) expression init_file amount balance sender source now display_format () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Decompile.Formatter.expression_format) get_warnings @@
      fun ~raise ->
      let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) init_file in
      let options =
        let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let (mini_c_exp, typed_exp) = Build.build_expression ~raise ~add_warning ~options syntax expression init_file in
      let compiled_exp = Compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp in
      let options           = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
      let runres  = Run.run_expression ~raise ~options compiled_exp.expr compiled_exp.expr_ty in
      Decompile.Of_michelson.decompile_expression ~raise typed_exp.type_expression runres

let evaluate_call (raw_options : Compiler_options.raw) source_file parameter amount balance sender source now display_format () =
    let warning_as_error = raw_options.warning_as_error in
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~warning_as_error ~display_format (Decompile.Formatter.expression_format) get_warnings @@
      fun ~raise ->
      let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options =
        let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let Compiler_options.{ entry_point ; _ } = options.frontend in
      let init_prog, aggregated_prg =
        let typed_prg = Build.merge_and_type_libraries ~raise ~add_warning ~options source_file in
        let agg_prg         = Compile.Of_typed.compile_program ~raise typed_prg in
        typed_prg, agg_prg
      in
      let meta             = Compile.Of_source.extract_meta syntax in
      let c_unit_param,_   = Compile.Of_source.compile_string ~raise ~options:options.frontend ~meta parameter in
      let imperative_param = Compile.Of_c_unit.compile_expression ~add_warning ~raise ~meta c_unit_param in
      let sugar_param      = Compile.Of_imperative.compile_expression ~raise imperative_param in
      let core_param       = Compile.Of_sugar.compile_expression ~raise sugar_param in
      let app              = Compile.Of_core.apply entry_point core_param in
      let typed_app        = Compile.Of_core.compile_expression ~raise ~add_warning ~options ~init_prog app in
      let app_aggregated   = Compile.Of_typed.compile_expression_in_context ~raise ~options:options.middle_end typed_app aggregated_prg in
      let app_mini_c       = Compile.Of_aggregated.compile_expression ~raise app_aggregated in
      let michelson        = Compile.Of_mini_c.compile_expression ~raise ~options app_mini_c in
      let options          = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None} in
      let runres           = Run.run_expression ~raise ~options michelson.expr michelson.expr_ty in
      Decompile.Of_michelson.decompile_expression ~raise app_aggregated.type_expression runres

let evaluate_expr (raw_options : Compiler_options.raw) source_file amount balance sender source now display_format () =
    let warning_as_error = raw_options.warning_as_error in
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~warning_as_error ~display_format Decompile.Formatter.expression_format get_warnings @@
      fun ~raise ->
        let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
        let options =
          let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
          Compiler_options.make ~protocol_version ~raw_options ~syntax ()
        in
        let Compiler_options.{ entry_point ; _ } = options.frontend in
        let (mini_c_exp, typed_exp) = Build.build_expression ~raise ~add_warning ~options syntax entry_point (Some source_file) in
        let compiled_exp = Compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp in
        let options           = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
        let runres  = Run.run_expression ~raise ~options compiled_exp.expr compiled_exp.expr_ty in
        Decompile.Of_michelson.decompile_expression ~raise typed_exp.type_expression runres
