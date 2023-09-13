open Simple_utils
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Run = Ligo_run.Of_michelson
module Raw_options = Compiler_options.Raw_options

let test (raw_options : Raw_options.t) code_input =
  ( Ligo_interpreter.Formatter.tests_format
  , fun ~raise ->
      let open Lwt.Let_syntax in
      let raw_options =
        { raw_options with
          protocol_version = Environment.Protocols.(variant_to_string in_use)
        }
      in
      let protocol_version =
        Helpers.protocol_to_variant ~raise raw_options.protocol_version
      in
      let syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:raw_options.deprecated
          (Syntax_name raw_options.syntax)
          Build.Source_input.(
            match code_input with
            | HTTP uri -> Some (Http_uri.get_filename uri)
            | From_file file_name -> Some file_name
            | Raw { id; _ } -> Some id
            | Raw_input_lsp { file; _ } -> Some file)
      in
      let options = Compiler_options.make ~protocol_version ~syntax ~raw_options () in
      let Compiler_options.{ steps; _ } = options.test_framework in
      let typed = Build.qualified_typed ~raise ~options code_input in
      let%map result = Interpreter.eval_test ~raise ~steps ~options typed in
      result, [] )


let test_expression (raw_options : Raw_options.t) expr source_file =
  ( Ligo_interpreter.Formatter.tests_format
  , fun ~raise ->
      let open Lwt.Let_syntax in
      let raw_options =
        { raw_options with
          protocol_version = Environment.Protocols.(variant_to_string in_use)
        }
      in
      let protocol_version =
        Helpers.protocol_to_variant ~raise raw_options.protocol_version
      in
      let syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:raw_options.deprecated
          (Syntax_name raw_options.syntax)
          source_file
      in
      let options = Compiler_options.make ~protocol_version ~syntax ~raw_options () in
      let Compiler_options.{ steps; _ } = options.test_framework in
      let module Stdlib = Build.Stdlib in
      let module Source_input = BuildSystem.Source_input in
      let init_prg =
        let f : Source_input.file_name -> Ast_typed.program =
         fun filename ->
          Build.qualified_typed ~raise ~options (Build.Source_input.From_file filename)
        in
        let default = Stdlib.select_lib_typed syntax (Stdlib.get ~options) in
        Option.value_map source_file ~f ~default
      in
      let typed =
        Ligo_compile.Utils.type_expression ~raise ~options syntax expr init_prg.pr_sig
      in
      let%map b, v = Interpreter.eval_expression ~raise ~steps ~options init_prg typed in
      (b, [ "eval", v ]), [] )


let typed_contract_and_expression_impl
    ~raise
    ~(options : Compiler_options.t)
    ~syntax
    ~(typed_prg : Ast_typed.program)
    ~expressions
    ?entrypoint_ctor
    ()
  =
  let Compiler_options.{ constants; file_constants; _ } = options.backend in
  let Compiler_options.{ module_; _ } = options.frontend in
  let module_path = Build.parse_module_path ~loc:Location.generated module_ in
  let app_typed_prg =
    Trace.trace ~raise Main_errors.self_ast_typed_tracer
    @@ Self_ast_typed.all_program typed_prg
  in
  let storage_expression, parameter_expression = expressions in
  let _, ctrct_sig =
    let sig_ = Ast_typed.to_extended_signature typed_prg in
    Trace.trace_option
      ~raise
      (Main_errors.self_ast_typed_tracer @@ `Self_ast_typed_not_a_contract module_)
      (Ast_typed.get_contract_signature sig_ module_path)
  in
  let ctrct_sig =
    { ctrct_sig with
      parameter =
        (* to handle single entrypoints:
        parameter type for single entry-point contracts such as
        `[@entry] let main (p:p) (s:s) = ...`
        are now compiled to `| Main of p`
        This representation do not yet persist up until the michelson representation
        due to "optimisations" :  `| Main of p` compiles to `p`
        When using `compile parameter /path/to/file` without using the -e CLI option,
        we assume the given expression is of type `p` and not `| Main of p`
        *)
        (match
           Option.map (Ast_typed.get_t_sum ctrct_sig.parameter) ~f:Ast_typed.Row.to_alist
         with
        | Some [ (_, ty) ] -> ty
        | _ -> ctrct_sig.parameter)
    }
  in
  let app_typed_sig = Ast_typed.to_signature app_typed_prg.pr_module in
  let storage_expr =
    Trace.try_with
      (fun ~raise ~catch:_ ->
        Ligo_compile.Utils.type_expression
          ~raise
          ~options
          ?wrap_variant:entrypoint_ctor
          ~annotation:(Checking.untype_type_expression ctrct_sig.storage)
          syntax
          storage_expression
          app_typed_sig)
      (fun ~catch:_ _ ->
        let typed_param =
          Ligo_compile.Utils.type_expression
            ~raise
            ~options
            ~annotation:(Checking.untype_type_expression ctrct_sig.storage)
            ?wrap_variant:entrypoint_ctor
            syntax
            storage_expression
            app_typed_sig
        in
        let () =
          Ligo_compile.Of_typed.assert_equal_contract_type
            ~raise
            Runned_result.Check_storage
            ctrct_sig
            typed_param
        in
        typed_param)
  in
  let parameter_expr =
    Trace.try_with
      (fun ~raise ~catch:_ ->
        Ligo_compile.Utils.type_expression
          ~raise
          ~options
          ?wrap_variant:entrypoint_ctor
          ~annotation:(Checking.untype_type_expression ctrct_sig.parameter)
          syntax
          parameter_expression
          app_typed_sig)
      (fun ~catch:_ _ ->
        let typed_param =
          Ligo_compile.Utils.type_expression
            ~raise
            ~options
            ~annotation:(Checking.untype_type_expression ctrct_sig.parameter)
            ?wrap_variant:entrypoint_ctor
            syntax
            parameter_expression
            app_typed_sig
        in
        let () =
          Ligo_compile.Of_typed.assert_equal_contract_type
            ~raise
            Runned_result.Check_parameter
            ctrct_sig
            typed_param
        in
        typed_param)
  in
  storage_expr, parameter_expr, app_typed_prg, constants, ctrct_sig, module_path


let typed_contract_and_expression
    ~raise
    ~(options : Compiler_options.t)
    ~syntax
    ~(source_file : Build.Source_input.code_input)
    ~expressions
    ?entrypoint_ctor
    ()
  =
  let typed_prg = Build.qualified_typed ~raise ~options source_file in
  typed_contract_and_expression_impl
    ~raise
    ~options
    ~syntax
    ~typed_prg
    ~expressions
    ?entrypoint_ctor
    ()


let dry_run
    (raw_options : Raw_options.t)
    entry_point
    source_file
    parameter
    storage
    amount
    balance
    sender
    source
    now
  =
  ( Decompile.Formatter.expression_format
  , fun ~raise ->
      let open Lwt.Let_syntax in
      let protocol_version =
        Helpers.protocol_to_variant ~raise raw_options.protocol_version
      in
      let syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:raw_options.deprecated
          (Syntax_name raw_options.syntax)
          (Some source_file)
      in
      let options =
        Compiler_options.make
          ~raw_options
          ~syntax
          ~protocol_version
          ~has_env_comments:false
          ()
      in
      let expressions = storage, parameter in
      let ( typed_storage
          , typed_parameter
          , app_typed_prg
          , constants
          , contract_type
          , module_path )
        =
        typed_contract_and_expression
          ~raise
          ~options
          ~syntax
          ~source_file:(From_file source_file)
          ~expressions
          ()
      in
      let%bind (_ : Mini_c.meta Run.Michelson.michelson) =
        let aggregated_contract =
          Ligo_compile.Of_typed.apply_to_entrypoint_with_contract_type
            ~raise
            ~options:options.middle_end
            app_typed_prg
            module_path
            contract_type
        in
        let expanded =
          Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_contract
        in
        let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded in
        let%bind michelson =
          Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c
        in
        (* fails if the given entry point is not a valid contract *)
        Ligo_compile.Of_michelson.build_contract
          ~raise
          ~enable_typed_opt:options.backend.enable_typed_opt
          ~protocol_version
          ~constants
          michelson
          []
      in
      let aggregated_storage =
        Ligo_compile.Of_typed.compile_expression_in_context
          ~raise
          ~options:options.middle_end
          ~self_program:false
          None
          app_typed_prg
          typed_storage
      in
      let expanded_storage =
        Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_storage
      in
      let mini_c_storage =
        Ligo_compile.Of_expanded.compile_expression ~raise expanded_storage
      in
      let%bind compiled_storage =
        Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_storage
      in
      let aggregated_parameter =
        Ligo_compile.Of_typed.compile_expression_in_context
          ~raise
          ~options:options.middle_end
          ~self_program:false
          None
          app_typed_prg
          typed_parameter
      in
      let expanded_parameter =
        Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_parameter
      in
      let mini_c_parameter =
        Ligo_compile.Of_expanded.compile_expression ~raise expanded_parameter
      in
      let%bind compiled_parameter =
        Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_parameter
      in
      let aggregated_prg =
        Compile.Of_typed.apply_to_entrypoint_with_contract_type
          ~raise
          ~options:options.middle_end
          ~self_program:false
          app_typed_prg
          module_path
          contract_type
      in
      let expanded_prg = Compile.Of_aggregated.compile_expression ~raise aggregated_prg in
      let mini_c_prg = Compile.Of_expanded.compile_expression ~raise expanded_prg in
      let%bind compile_exp =
        Compile.Of_mini_c.compile_contract ~raise ~options mini_c_prg
      in
      let%bind parameter_ty =
        (* fails if the given entry point is not a valid contract *)
        let%map _contract : Mini_c.meta Tezos_utils.Michelson.michelson Lwt.t =
          Compile.Of_michelson.build_contract
            ~raise
            ~enable_typed_opt:options.backend.enable_typed_opt
            ~protocol_version
            compile_exp
            []
        in
        Option.map ~f:fst @@ Self_michelson.fetch_contract_ty_inputs compile_exp.expr_ty
      in
      let%bind compiled_input =
        Compile.Utils.compile_contract_input
          ~raise
          ~options
          parameter
          storage
          syntax
          app_typed_prg
      in
      let%bind args_michelson =
        Run.evaluate_expression ~raise compiled_input.expr compiled_input.expr_ty
      in
      let%bind options =
        Run.make_dry_run_options
          ~raise
          { now; amount; balance; sender; source; parameter_ty }
      in
      let%map runres =
        Run.run_contract
          ~raise
          ~options
          compile_exp.expr
          compile_exp.expr_ty
          args_michelson
      in
      ( Decompile.Of_michelson.decompile_value_from_contract_execution
          ~raise
          aggregated_prg.type_expression
          runres
      , [] ) )


let interpret
    (raw_options : Raw_options.t)
    expression
    init_file
    amount
    balance
    sender
    source
    now
  =
  ( Decompile.Formatter.expression_format
  , fun ~raise ->
      let open Lwt.Let_syntax in
      let syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:raw_options.deprecated
          (Syntax_name raw_options.syntax)
          init_file
      in
      let options =
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let%bind Build.{ expression; ast_type } =
        Build.build_expression ~raise ~options syntax expression init_file
      in
      let%bind options =
        Run.make_dry_run_options
          ~raise
          { now; amount; balance; sender; source; parameter_ty = None }
      in
      let%map runres =
        Run.run_expression ~raise ~options expression.expr expression.expr_ty
      in
      Decompile.Of_michelson.decompile_expression ~raise ast_type runres, [] )


let evaluate_call
    (raw_options : Raw_options.t)
    source_file
    function_name
    parameter
    amount
    balance
    sender
    source
    now
  =
  ( Decompile.Formatter.expression_format
  , fun ~raise ->
      let open Lwt.Let_syntax in
      let syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:raw_options.deprecated
          (Syntax_name raw_options.syntax)
          (Some source_file)
      in
      let options =
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let init_prog =
        Build.qualified_typed ~raise ~options (Build.Source_input.From_file source_file)
      in
      let entry_point =
        Ligo_prim.Value_var.of_input_var ~loc:Location.dummy function_name
      in
      let meta = Compile.Of_source.extract_meta syntax in
      let c_unit_param, _ =
        Compile.Of_source.preprocess_string
          ~raise
          ~options:options.frontend
          ~meta
          parameter
      in
      let imperative_param =
        Compile.Of_c_unit.compile_expression ~raise ~meta c_unit_param
      in
      let core_param =
        Compile.Of_unified.compile_expression ~raise ~options imperative_param
      in
      let app = Compile.Of_core.apply entry_point core_param in
      let typed_app =
        Compile.Of_core.compile_expression
          ~raise
          ~options
          ~context:(Ast_typed.to_signature init_prog.pr_module)
          app
      in
      let app_aggregated =
        Compile.Of_typed.compile_expression_in_context
          ~raise
          ~options:options.middle_end
          None
          init_prog
          typed_app
      in
      let app_expanded = Compile.Of_aggregated.compile_expression ~raise app_aggregated in
      let app_mini_c = Compile.Of_expanded.compile_expression ~raise app_expanded in
      let%bind michelson =
        Compile.Of_mini_c.compile_expression ~raise ~options app_mini_c
      in
      let%bind options =
        Run.make_dry_run_options
          ~raise
          { now; amount; balance; sender; source; parameter_ty = None }
      in
      let%map runres =
        Run.run_expression ~raise ~options michelson.expr michelson.expr_ty
      in
      ( Decompile.Of_michelson.decompile_expression
          ~raise
          app_aggregated.type_expression
          runres
      , [] ) )


let evaluate_expr
    (raw_options : Raw_options.t)
    source_file
    exp
    amount
    balance
    sender
    source
    now
  =
  ( Decompile.Formatter.expression_format
  , fun ~raise ->
      let open Lwt.Let_syntax in
      let syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:raw_options.deprecated
          (Syntax_name raw_options.syntax)
          (Some source_file)
      in
      let options =
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let%bind Build.{ expression; ast_type } =
        Build.build_expression ~raise ~options syntax exp (Some source_file)
      in
      let%bind options =
        Run.make_dry_run_options
          ~raise
          { now; amount; balance; sender; source; parameter_ty = None }
      in
      let%map runres =
        Run.run_expression ~raise ~options expression.expr expression.expr_ty
      in
      Decompile.Of_michelson.decompile_expression ~raise ast_type runres, [] )
