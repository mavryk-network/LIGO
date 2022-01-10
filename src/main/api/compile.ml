
open Api_helpers
open Simple_utils
module Helpers   = Ligo_compile.Helpers
module Run = Ligo_run.Of_michelson

let no_comment node =
  Tezos_micheline.Micheline.(inject_locations (fun _ -> Simple_utils.Location.generated) (strip_locations node))

let contract ?werror source_file entry_point declared_views syntax protocol_version display_format disable_typecheck michelson_code_format michelson_comments () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ?werror ~display_format (Formatter.Michelson_formatter.michelson_format michelson_code_format michelson_comments) get_warnings @@
      fun ~raise ->
      let options =
          let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
          Compiler_options.make ~protocol_version ()
      in
      let code,env = Build.build_contract ~raise ~add_warning ~options syntax entry_point source_file in
      let views =
        Build.build_views ~raise ~add_warning ~options syntax entry_point (declared_views,env) source_file
      in
      Ligo_compile.Of_michelson.build_contract ~raise ~disable_typecheck code views

let expression expression syntax protocol_version init_file display_format without_run michelson_format werror () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~werror ~display_format (Formatter.Michelson_formatter.michelson_format michelson_format []) get_warnings @@
      fun ~raise ->
      let options =
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        Compiler_options.make ~protocol_version ()
      in
      let (mini_c_exp,_) = Build.build_expression ~raise ~add_warning ~options syntax expression init_file in
      let compiled_exp   = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp in
      no_comment @@
      if without_run then
        Run.clean_expression compiled_exp.expr
      else
        Run.evaluate_expression ~raise compiled_exp.expr compiled_exp.expr_ty

let parameter source_file entry_point expression syntax protocol_version amount balance sender source now display_format michelson_format werror () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~werror ~display_format (Formatter.Michelson_formatter.michelson_format michelson_format []) get_warnings @@
      fun ~raise ->
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        let options = Compiler_options.make ~protocol_version () in
        let typed_prg   = Build.combined_contract ~raise ~add_warning ~options syntax source_file in
        let aggregated_prg  = Ligo_compile.Of_typed.compile_program ~raise typed_prg in
        let _contract =
          let aggregated_contract = Ligo_compile.Of_typed.apply_to_entrypoint_contract ~raise typed_prg entry_point in
          let mini_c   = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_contract in
          let michelson = Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c in
        (* fails if the given entry point is not a valid contract *)
          Ligo_compile.Of_michelson.build_contract ~raise michelson in

        let typed_param      = Ligo_compile.Utils.type_expression ~raise ~options (Some source_file) syntax expression typed_prg in
        let aggregated_param = Ligo_compile.Of_typed.compile_expression_in_context ~raise typed_param aggregated_prg in
        let mini_c_param     = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_param in
        let compiled_param   = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_param in
        let ()               = Ligo_compile.Of_typed.assert_equal_contract_type ~raise Check_parameter entry_point typed_prg typed_param in
        let options          = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender;  source ; parameter_ty = None } in
        no_comment (Run.evaluate_expression ~raise ~options compiled_param.expr compiled_param.expr_ty)

let storage source_file entry_point expression syntax protocol_version amount balance sender source now display_format michelson_format werror () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~werror ~display_format (Formatter.Michelson_formatter.michelson_format michelson_format []) get_warnings @@
      fun ~raise ->
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        let options = Compiler_options.make ~protocol_version () in
        let typed_prg   = Build.combined_contract ~raise ~add_warning ~options syntax source_file in
        let aggregated_prg  = Ligo_compile.Of_typed.compile_program ~raise typed_prg in
        let _contract =
          let aggregated_contract = Ligo_compile.Of_typed.apply_to_entrypoint_contract ~raise typed_prg entry_point in
          let mini_c   = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_contract in
          let michelson = Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c in
         (* fails if the given entry point is not a valid contract *)
          Ligo_compile.Of_michelson.build_contract ~raise michelson in
        let typed_param      = Ligo_compile.Utils.type_expression ~raise ~options (Some source_file) syntax expression typed_prg in
        let aggregated_param = Ligo_compile.Of_typed.compile_expression_in_context ~raise typed_param aggregated_prg in
        let mini_c_param     = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_param in
        let compiled_param   = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_param in
        let ()               = Ligo_compile.Of_typed.assert_equal_contract_type ~raise Check_storage entry_point typed_prg typed_param in
        let options          = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender;  source ; parameter_ty = None } in
        no_comment (Run.evaluate_expression ~raise ~options compiled_param.expr compiled_param.expr_ty)
