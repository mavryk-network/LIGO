
open Api_helpers
open Simple_utils
module Helpers   = Ligo_compile.Helpers
module Run = Ligo_run.Of_michelson

let no_comment node =
  Tezos_micheline.Micheline.(inject_locations (fun _ -> Mini_c.dummy_meta) (strip_locations node))

let has_env_comments michelson_comments =
  Option.is_some (List.find michelson_comments ~f:(function `Env -> true | _ -> false))

let read_file_constants ~raise file_constants =
  match file_constants with
  | None -> []
  | Some fn ->
     try
       let buf = In_channel.read_all fn in
       let json = Yojson.Basic.from_string buf in
       json |> Yojson.Basic.Util.to_list |> List.map ~f:Yojson.Basic.Util.to_string
     with Sys_error _ -> raise.Trace.raise (`Main_cannot_open_global_constants fn)
        | Yojson.Json_error s -> raise.Trace.raise (`Main_cannot_parse_global_constants (fn, s))

let contract (raw_options : Compiler_options.raw) source_file display_format michelson_code_format michelson_comments () =
    let warning_as_error = raw_options.warning_as_error in
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~warning_as_error ~display_format (Formatter.Michelson_formatter.michelson_format michelson_code_format michelson_comments) get_warnings @@
      fun ~raise ->
      let options =
          let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
          let syntax = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
          let has_env_comments = has_env_comments michelson_comments in
          Compiler_options.make
            ~raw_options
            ~syntax
            ~protocol_version
            ~has_env_comments
            ()
      in
      let Compiler_options.{ disable_michelson_typechecking = disable_typecheck ; views ; constants ; file_constants ; _ } = options.backend in
      let Compiler_options.{ entry_point ; _ } = options.frontend in
      let code,env = Build.build_contract ~raise ~add_warning ~options entry_point source_file in
      let views = Build.build_views ~raise ~add_warning ~options entry_point (views,env) source_file in
      let file_constants = read_file_constants ~raise file_constants in
      let constants = constants @ file_constants in
      Ligo_compile.Of_michelson.build_contract ~raise ~add_warning ~protocol_version:options.middle_end.protocol_version ~disable_typecheck ~constants code views

let expression (raw_options : Compiler_options.raw) expression init_file display_format michelson_format () =
    let warning_as_error = raw_options.warning_as_error in
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~warning_as_error ~display_format (Formatter.Michelson_formatter.michelson_format michelson_format []) get_warnings @@
      fun ~raise ->
      let syntax = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) init_file in
      let options =
        let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
        Compiler_options.make
          ~raw_options
          ~syntax
          ~protocol_version
          ~has_env_comments:false
          ()
      in
      let Compiler_options.{ without_run ; _ } = options.backend in
      let Compiler_options.{ constants ; file_constants ; _ } = options.backend in
      let file_constants = read_file_constants ~raise file_constants in
      let constants = constants @ file_constants in
      let (mini_c_exp,_) = Build.build_expression ~raise ~add_warning ~options syntax expression init_file in
      let compiled_exp   = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp in
      no_comment @@
      if without_run then
        Run.clean_expression compiled_exp.expr
      else
        let options = Run.make_dry_run_options ~raise ~constants { now = None ; amount = "0" ; balance = "0" ; sender = None ;  source = None ; parameter_ty = None } in
        Run.evaluate_expression ~raise ~options compiled_exp.expr compiled_exp.expr_ty

let constant (raw_options : Compiler_options.raw) constants init_file display_format () =
    let warning_as_error = raw_options.warning_as_error in
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~warning_as_error ~display_format Formatter.Michelson_formatter.michelson_constant_format get_warnings @@
      fun ~raise ->
      let syntax = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) init_file in
      let options =
        let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
        Compiler_options.make
          ~raw_options
          ~syntax
          ~protocol_version
          ~has_env_comments:false
          ()
      in
      let Compiler_options.{ without_run ; _ } = options.backend in
      let (mini_c_exp,_) = Build.build_expression ~raise ~add_warning ~options syntax constants init_file in
      let compiled_exp   = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp in
      let (hash, value) = if without_run then
                            Run.clean_constant ~raise compiled_exp.expr
                          else
                            Run.evaluate_constant ~raise compiled_exp.expr compiled_exp.expr_ty in
      (hash, value)

let parameter (raw_options : Compiler_options.raw) source_file entry_point expression amount balance sender source now display_format michelson_format () =
    let warning_as_error = raw_options.warning_as_error in
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~warning_as_error ~display_format (Formatter.Michelson_formatter.michelson_format michelson_format []) get_warnings @@
      fun ~raise ->
        let entry_point = Ast_typed.ValueVar.of_input_var entry_point in
        let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
        let syntax = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
        let options = Compiler_options.make
            ~raw_options
            ~syntax
            ~protocol_version
            ~has_env_comments:false
            () in
        let Compiler_options.{ constants ; file_constants ; _ } = options.backend in
        let file_constants = read_file_constants ~raise file_constants in
        let constants = constants @ file_constants in
        let app_typed_prg, typed_prg =
          Build.build_typed ~raise ~add_warning ~options (Ligo_compile.Of_core.Contract entry_point) source_file in
        let typed_param              = Ligo_compile.Utils.type_expression ~raise ~add_warning ~options syntax expression typed_prg in
        let typed_param, typed_prg   = Self_ast_typed.remove_unused_expression typed_param typed_prg in
        let aggregated_prg           = Ligo_compile.Of_typed.compile_program ~raise typed_prg in
        let _contract : Mini_c.meta Run.Michelson.michelson =
          let aggregated_contract = Ligo_compile.Of_typed.apply_to_entrypoint_contract ~raise ~options:options.middle_end app_typed_prg entry_point in
          let mini_c              = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_contract in
          let michelson           = Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c in
        (* fails if the given entry point is not a valid contract *)
          Ligo_compile.Of_michelson.build_contract ~raise ~add_warning ~protocol_version ~constants michelson [] in
        let aggregated_param = Ligo_compile.Of_typed.compile_expression_in_context ~raise ~options:options.middle_end typed_param aggregated_prg in
        let mini_c_param     = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_param in
        let compiled_param   = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_param in
        let ()               = Ligo_compile.Of_typed.assert_equal_contract_type ~raise Check_parameter entry_point app_typed_prg typed_param in
        let options = Run.make_dry_run_options ~raise ~constants { now ; amount ; balance ; sender;  source ; parameter_ty = None } in
        no_comment (Run.evaluate_expression ~raise ~options compiled_param.expr compiled_param.expr_ty)

let storage (raw_options : Compiler_options.raw) source_file expression amount balance sender source now display_format michelson_format () =
    let warning_as_error = raw_options.warning_as_error in
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~warning_as_error ~display_format (Formatter.Michelson_formatter.michelson_format michelson_format []) get_warnings @@
      fun ~raise ->
        let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
        let syntax = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
        let options = Compiler_options.make
            ~raw_options
            ~syntax
            ~protocol_version
            ~has_env_comments:false
            () in
        let Compiler_options.{ entry_point ; _ } = options.frontend in
        let Compiler_options.{ constants ; file_constants ; _ } = options.backend in
        let file_constants = read_file_constants ~raise file_constants in
        let constants = constants @ file_constants in
        let entry_point = Ast_typed.ValueVar.of_input_var entry_point in
        let app_typed_prg, typed_prg =
          Build.build_typed ~raise ~add_warning ~options (Ligo_compile.Of_core.Contract entry_point) source_file in
        let typed_param              = Ligo_compile.Utils.type_expression ~raise ~add_warning ~options syntax expression typed_prg in
        let typed_param, typed_prg   = Self_ast_typed.remove_unused_expression typed_param typed_prg in
        let aggregated_prg           = Ligo_compile.Of_typed.compile_program ~raise typed_prg in
        let _contract : Mini_c.meta Run.Michelson.michelson =
          let aggregated_contract = Ligo_compile.Of_typed.apply_to_entrypoint_contract ~raise ~options:options.middle_end app_typed_prg entry_point in
          let mini_c              = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_contract in
          let michelson           = Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c in
         (* fails if the given entry point is not a valid contract *)
          Ligo_compile.Of_michelson.build_contract ~raise ~add_warning ~protocol_version ~constants michelson [] in
        let aggregated_param = Ligo_compile.Of_typed.compile_expression_in_context ~raise ~options:options.middle_end typed_param aggregated_prg in
        let mini_c_param     = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_param in
        let compiled_param   = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_param in
        let ()               = Ligo_compile.Of_typed.assert_equal_contract_type ~raise Check_storage entry_point app_typed_prg typed_param in
        let options = Run.make_dry_run_options ~raise ~constants { now ; amount ; balance ; sender;  source ; parameter_ty = None } in
        no_comment (Run.evaluate_expression ~raise ~options compiled_param.expr compiled_param.expr_ty)
