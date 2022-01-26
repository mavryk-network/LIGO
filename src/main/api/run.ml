open Api_helpers
open Simple_utils
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers
module Run = Ligo_run.Of_michelson

let dry_run_exp ="
  let (c, _, _) =\ 
    Test.originate_from_file\ 
      CLI_PARAMETERS.filename\ 
      CLI_PARAMETERS.entrypoint\ 
      ([] : string list)\ 
      (Test.eval CLI_PARAMETERS.storage)\ 
      0tez\ 
  in\ 
  let gas_consumption =\ 
    Test.transfer_exn\ 
      c\ 
      (Test.eval CLI_PARAMETERS.parameter)\ 
      0tez\ 
  in\ 
  { gas_consumption = gas_consumption ; returned_storage = Test.get_storage_of_address c }\ 
"

let test source_file syntax steps protocol_version display_format project_root () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ligo_interpreter.Formatter.tests_format) get_warnings @@
      fun ~raise ->
      let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
      let options = Compiler_options.make ~test:true ~protocol_version ?project_root () in
      let typed   = Build.build_context ~raise ~add_warning ~options syntax source_file in
      Interpreter.eval_test ~raise ~steps ~options ~protocol_version typed

let dry_run source_file entry_point parameter storage amount balance sender source now (syntax:string) protocol_version display_format werror project_root () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~werror ~display_format (Ligo_interpreter.Formatter.tests_format) get_warnings @@
      fun ~raise ->
      let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
      let options = Compiler_options.make ~protocol_version ?project_root () in
      let core : Ast_core.module_ = Build.infer_contract ~raise ~add_warning ~options source_file in


      let attr = Ast_core.{ inline = false ; no_mutation = false ; view = false ; public = true } in
      let syntax = Ligo_compile.Helpers.(syntax_to_variant ~raise (Syntax_name syntax) (Some source_file)) in

      let (cli_parameters_module, (binder_parameter,binder_storage)) =
        let open Ast_core in
        let expr_storage = Ligo_compile.Utils.core_expression_string ~raise ~options syntax storage in
        let expr_parameter = Ligo_compile.Utils.core_expression_string ~raise ~options syntax parameter in
        let binder_parameter = Location.wrap @@ Var.of_name "parameter" in
        let binder_storage = Location.wrap @@ Var.of_name "storage" in
        let binder_filename = Location.wrap @@ Var.of_name "filename" in
        let binder_entrypoint = Location.wrap @@ Var.of_name "entrypoint" in
        (* let binder_parameter = Location.wrap @@ Var.fresh ~name:"PARAMETER" () in
        let binder_storage = Location.wrap @@ Var.fresh ~name:"STORAGE" () in
        let binder_filename = Location.wrap @@ Var.fresh ~name:"FILENAME" () in
        let binder_entrypoint = Location.wrap @@ Var.fresh ~name:"ENTRYPOINT" () in *)
        let lst = [
          Declaration_constant { name = None ; binder = make_binder binder_parameter  ; attr ; expr = expr_parameter                              } ;
          Declaration_constant { name = None ; binder = make_binder binder_filename   ; attr ; expr = e_string (Ligo_string.standard source_file) } ;
          Declaration_constant { name = None ; binder = make_binder binder_entrypoint ; attr ; expr = e_string (Ligo_string.standard entry_point) } ;
          Declaration_constant { name = None ; binder = make_binder binder_storage    ; attr ; expr = expr_storage                                } ;
          ]
        in
        let module_ = List.append core (List.map ~f:(Location.wrap) lst) in
        Declaration_module {module_binder = "CLI_PARAMETERS" ; module_ ; module_attr = {public = true} }, (binder_parameter,binder_storage)
      in
      let cli_parameters_ty_module =
        let cli_parameters_module = Compile.Of_core.typecheck ~raise ~add_warning ~options Env [Location.wrap cli_parameters_module] in
        let typed_cli_params =
          let accessor_parameter = Ast_core.(e_module_accessor "CLI_PARAMETERS" (e_variable binder_parameter)) in
          let accessor_storage = Ast_core.(e_module_accessor "CLI_PARAMETERS" (e_variable binder_storage)) in
          let pair = Ast_core.(e_pair accessor_parameter accessor_storage) in
          Compile.Of_core.compile_expression ~raise ~options ~init_prog:cli_parameters_module pair
        in
        let (a,b) = Option.value_exn (Ast_typed.get_a_pair typed_cli_params) in
        let open Ast_typed in
        (* let binder_parameter_ty = Var.fresh ~name:"PARAMETER_TYPE" () in
        let binder_storage_ty = Var.fresh ~name:"STORAGE_TYPE" () in *)
        let binder_parameter_ty = Var.of_name "parameter_ty" in
        let binder_storage_ty = Var.of_name "storage_ty" in
        let lst = [
          Declaration_type {type_binder = binder_parameter_ty ; type_expr = a.type_expression ; type_attr = {public = true}} ;
          Declaration_type {type_binder = binder_storage_ty   ; type_expr = b.type_expression ; type_attr = {public = true}} ;
          ]
        in
        let module_ = List.map ~f:(Location.wrap) lst in
        let cli_parameters_ty_module = [ Location.wrap @@ Declaration_module {module_binder = "CLI_PARAMETERS_TY" ; module_ ; module_attr = {public = true} } ] in
        let cli_data = List.append cli_parameters_module cli_parameters_ty_module in
        let test_exp = Ligo_compile.Utils.type_expression_string ~raise ~options syntax dry_run_exp cli_data in
        let decl : Ast_typed.module_ = List.append cli_data [ Location.wrap @@ Declaration_constant { name = None ; binder = Location.wrap @@ Var.of_name "test_dry_run" ; attr ; expr = test_exp } ] in
        decl
      in
      Interpreter.eval_test ~raise ~steps:9000 ~options ~protocol_version cli_parameters_ty_module

let interpret expression init_file syntax protocol_version amount balance sender source now display_format project_root () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Decompile.Formatter.expression_format) get_warnings @@
      fun ~raise ->
      let options =
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        Compiler_options.make ~protocol_version ?project_root ()
      in
      let (mini_c_exp, typed_exp) = Build.build_expression ~raise ~add_warning ~options syntax expression init_file in
      let compiled_exp = Compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp in
      let options           = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
      let runres  = Run.run_expression ~raise ~options compiled_exp.expr compiled_exp.expr_ty in
      Decompile.Of_michelson.decompile_expression ~raise typed_exp.type_expression runres

let evaluate_call source_file entry_point parameter amount balance sender source now syntax protocol_version display_format werror project_root () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~werror ~display_format (Decompile.Formatter.expression_format) get_warnings @@
      fun ~raise ->
      let options =
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        Compiler_options.make ~protocol_version ?project_root ()
      in
      let init_prog, aggregated_prg =
        let typed_prg = Build.build_context ~raise ~add_warning ~options syntax source_file in
        let agg_prg         = Compile.Of_typed.compile_program ~raise typed_prg in
        typed_prg, agg_prg
      in
      let meta             = Compile.Of_source.extract_meta ~raise syntax source_file in
      let c_unit_param,_   = Compile.Of_source.compile_string ~raise ~options ~meta parameter in
      let imperative_param = Compile.Of_c_unit.compile_expression ~raise ~meta c_unit_param in
      let sugar_param      = Compile.Of_imperative.compile_expression ~raise imperative_param in
      let core_param       = Compile.Of_sugar.compile_expression sugar_param in
      let app              = Compile.Of_core.apply entry_point core_param in
      let typed_app        = Compile.Of_core.compile_expression ~raise ~options ~init_prog app in
      let app_aggregated   = Compile.Of_typed.compile_expression_in_context ~raise typed_app aggregated_prg in
      let app_mini_c       = Compile.Of_aggregated.compile_expression ~raise app_aggregated in
      let michelson        = Compile.Of_mini_c.compile_expression ~raise ~options app_mini_c in
      let options          = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None} in
      let runres           = Run.run_expression ~raise ~options michelson.expr michelson.expr_ty in
      Decompile.Of_michelson.decompile_expression ~raise app_aggregated.type_expression runres

let evaluate_expr source_file entry_point amount balance sender source now syntax protocol_version display_format werror project_root () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~werror ~display_format Decompile.Formatter.expression_format get_warnings @@
      fun ~raise ->
        let options =
          let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
          Compiler_options.make ~protocol_version ?project_root ()
        in
        let (mini_c_exp, typed_exp) = Build.build_expression ~raise ~add_warning ~options syntax entry_point (Some source_file) in
        let compiled_exp = Compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp in
        let options           = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
        let runres  = Run.run_expression ~raise ~options compiled_exp.expr compiled_exp.expr_ty in
        Decompile.Of_michelson.decompile_expression ~raise typed_exp.type_expression runres
