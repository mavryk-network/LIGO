open Api_helpers
open Simple_utils
module Helpers = Ligo_compile.Helpers
module Run = Ligo_run.Of_michelson
open Ligo_prim
module Raw_options = Compiler_options.Raw_options

let loc = Location.dummy

let no_comment node =
  Tezos_micheline.Micheline.(
    inject_locations (fun _ -> Mini_c.dummy_meta) (strip_locations node))


let has_env_comments michelson_comments =
  Option.is_some
    (List.find michelson_comments ~f:(function
        | `Location | `Env -> true
        | _ -> false))


let read_file_constants ~raise file_constants =
  match file_constants with
  | None -> []
  | Some fn ->
    (try
       let buf = In_channel.read_all fn in
       let json = Yojson.Basic.from_string buf in
       json |> Yojson.Basic.Util.to_list |> List.map ~f:Yojson.Basic.Util.to_string
     with
    | Sys_error _ -> raise.Trace.error (`Main_cannot_open_global_constants fn)
    | Yojson.Json_error s ->
      raise.Trace.error (`Main_cannot_parse_global_constants (fn, s)))


module Path = struct
  type t = string
end

type source =
  | Text of string * Syntax_types.t
  | File of Path.t
  | Json of Yojson.Safe.t

let contract
    (raw_options : Raw_options.t)
    source
    display_format
    michelson_code_format
    michelson_comments
    ()
  =
  let warning_as_error = raw_options.warning_as_error in
  format_result
    ~warning_as_error
    ~display_format
    ~no_colour:raw_options.no_colour
    (Formatter.Michelson_formatter.michelson_format
       michelson_code_format
       michelson_comments)
  @@ fun ~raise ->
  let options =
    let protocol_version =
      Helpers.protocol_to_variant ~raise raw_options.protocol_version
    in
    let syntax =
      match source with
      | Text (_source_code, syntax) -> syntax
      | File source_file ->
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
      | Json _ ->
        raise.error
          (`Main_invalid_syntax_name "Syntax cannot be detected from json file.")
    in
    let has_env_comments = has_env_comments michelson_comments in
    Compiler_options.make ~raw_options ~syntax ~protocol_version ~has_env_comments ()
  in
  let Compiler_options.
        { disable_michelson_typechecking = disable_typecheck
        ; views
        ; constants
        ; file_constants
        ; _
        }
    =
    options.backend
  in
  let Compiler_options.{ entry_point; _ } = options.frontend in
  let source =
    match source with
    | Json _ ->
      raise.error (`Main_invalid_syntax_name "Cannot compile contract in json format")
    | File filename -> BuildSystem.Source_input.From_file filename
    | Text (source_code, syntax) ->
      BuildSystem.Source_input.(
        Raw { id = "foo" ^ Syntax.to_ext syntax; code = source_code })
  in
  let code, views = Build.build_contract ~raise ~options entry_point views source in
  let file_constants = read_file_constants ~raise file_constants in
  let constants = constants @ file_constants in
  Ligo_compile.Of_michelson.build_contract
    ~raise
    ~enable_typed_opt:options.backend.enable_typed_opt
    ~protocol_version:options.middle_end.protocol_version
    ~has_env_comments:options.backend.has_env_comments
    ~disable_typecheck
    ~constants
    code
    views


let cst
    (raw_options : Raw_options.t)
    source
    display_format
    michelson_code_format
    michelson_comments
    ()
  =
  let translate_json_result value display_format =
    let (Simple_utils.Display.Ex_display_format display_format) = display_format in
    match value with
    | Ok (v, _) -> Ok (v, "")
    | Error (e, _) ->
      let open Simple_utils.Display in
      (match display_format with
      | Json ->
        let str =
          Format.asprintf
            "%a"
            (Main_errors.Formatter.error_ppformat
               ~display_format:Human_readable
               ~no_colour:false)
            e
        in
        Error ("error: " ^ str, "")
      | Dev | Human_readable ->
        let str =
          Format.asprintf
            "%a"
            (Main_errors.Formatter.error_ppformat
               ~display_format:Human_readable
               ~no_colour:false)
            e
        in
        Error ("error: " ^ str, ""))
  in
  let translate_michelson_result value _display_format = value in
  let format_michelson_result k =
    let warning_as_error = raw_options.warning_as_error in
    format_result
      ~warning_as_error
      ~display_format
      ~no_colour:raw_options.no_colour
      (Formatter.Michelson_formatter.michelson_format
         michelson_code_format
         michelson_comments)
    @@ k
  in
  let format_json_result k = Trace.to_stdlib_result k in
  ignore format_json_result;
  ignore translate_json_result;
  let value =
    format_michelson_result
    @@ fun ~raise ->
    let compile source_code syntax =
      let options =
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        let has_env_comments = has_env_comments michelson_comments in
        Compiler_options.make ~raw_options ~syntax ~protocol_version ~has_env_comments ()
      in
      Ligo_compile.Utils.compile_cst_string
        ~raise
        ~options
        ~protocol_version:options.middle_end.protocol_version
        source_code
        syntax
        []
    in
    let json_to_and_from source_code syntax =
      let yojson = Yojson.Safe.from_string source_code in
      let open Syntax_types in
      match syntax with
      | PascaLIGO ->
        raise.error (`Main_invalid_syntax_name "abstract(): pascaligo not supported")
      | CameLIGO ->
        (match Parsing.Cameligo.CST.of_yojson yojson with
        | Ok cst -> Yojson.Safe.to_string @@ Parsing.Cameligo.CST.to_yojson cst
        | Error e ->
          raise.error
            (`Main_invalid_syntax_name ("abstract(): yojson from string failed: " ^ e)))
      | JsLIGO ->
        raise.error (`Main_invalid_syntax_name "abstract(): jsligo not supported")
    in
    ignore json_to_and_from;
    let file_name =
      match source with
      | Json _ -> None
      | File _filename -> None
      | Text (_source_code, _syntax) -> Some "foo.mligo"
    in
    let syntax = Syntax.of_string_opt ~raise (Syntax_name "cameligo") file_name in
    let source_code =
      match source with
      | Json _ ->
        raise.error (`Main_invalid_syntax_name "TODO compile from yojson directly")
      | File filename ->
        let read_whole_file filename =
          let ch = In_channel.create filename in
          let[@warning "-3"] s =
            really_input_string
              ch
              (ch |> In_channel.length |> Int64.to_int |> Stdlib.Option.get)
          in
          In_channel.close ch;
          s
        in
        read_whole_file filename
      | Text (source_code, _syntax) -> source_code
    in
    (* Let's try simply json stringifying it back instead of sending it to the compiler pipelines *)
    (* json_to_and_from source_code syntax *)
    (* compile to michelson *)
    compile source_code syntax
  in
  translate_michelson_result value display_format


(* translate_json_result value display_format *)

let ast_typed
    (raw_options : Raw_options.t)
    source
    display_format
    michelson_code_format
    michelson_comments
    ()
  =
  let translate_json_result value display_format =
    let (Simple_utils.Display.Ex_display_format display_format) = display_format in
    match value with
    | Ok (v, _) -> Ok (v, "")
    | Error (e, _) ->
      let open Simple_utils.Display in
      (match display_format with
      | Json ->
        let str =
          Format.asprintf
            "%a"
            (Main_errors.Formatter.error_ppformat
               ~display_format:Human_readable
               ~no_colour:false)
            e
        in
        Error ("error: " ^ str, "")
      | Dev | Human_readable ->
        let str =
          Format.asprintf
            "%a"
            (Main_errors.Formatter.error_ppformat
               ~display_format:Human_readable
               ~no_colour:false)
            e
        in
        Error ("error: " ^ str, ""))
  in
  let translate_michelson_result value _display_format = value in
  let format_michelson_result k =
    let warning_as_error = raw_options.warning_as_error in
    format_result
      ~warning_as_error
      ~display_format
      ~no_colour:raw_options.no_colour
      (Formatter.Michelson_formatter.michelson_format
         michelson_code_format
         michelson_comments)
    @@ k
  in
  let format_json_result k = Trace.to_stdlib_result k in
  ignore format_json_result;
  ignore translate_json_result;
  let value =
    format_michelson_result
    @@ fun ~raise ->
    let compile source_code syntax =
      let options =
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        let has_env_comments = has_env_comments michelson_comments in
        Compiler_options.make ~raw_options ~syntax ~protocol_version ~has_env_comments ()
      in
      Ligo_compile.Utils.compile_ast_typed_string
        ~raise
        ~options
        ~protocol_version:options.middle_end.protocol_version
        source_code
        []
    in
    let compile_yojson source_yojson syntax =
      let options =
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        let has_env_comments = has_env_comments michelson_comments in
        Compiler_options.make ~raw_options ~syntax ~protocol_version ~has_env_comments ()
      in
      Ligo_compile.Utils.compile_ast_typed_yojson
        ~raise
        ~options
        ~protocol_version:options.middle_end.protocol_version
        source_yojson
        []
    in
    let json_to_and_from source_code syntax =
      let yojson = Yojson.Safe.from_string source_code in
      let open Syntax_types in
      match syntax with
      | PascaLIGO ->
        raise.error (`Main_invalid_syntax_name "type_program(): pascaligo not supported")
      | CameLIGO ->
        (match Ast_typed.Types.program_of_yojson yojson with
        | Ok ast_typed ->
          Yojson.Safe.to_string @@ Ast_typed.Types.program_to_yojson ast_typed
        | Error e ->
          raise.error
            (`Main_invalid_syntax_name
              ("type_program(): yojson from string failed: " ^ e)))
      | JsLIGO ->
        raise.error (`Main_invalid_syntax_name "type_program(): jsligo not supported")
    in
    ignore json_to_and_from;
    let file_name =
      match source with
      | Json _ -> Some "foo.mligo"
      | File _filename -> None
      | Text (_source_code, _syntax) -> Some "foo.mligo"
    in
    let syntax = Syntax.of_string_opt ~raise (Syntax_name "cameligo") file_name in
    match source with
    | Json source_yojson -> compile_yojson source_yojson syntax
    | File filename ->
      let read_whole_file filename =
        let ch = In_channel.create filename in
        let[@warning "-3"] s =
          really_input_string
            ch
            (ch |> In_channel.length |> Int64.to_int |> Stdlib.Option.get)
        in
        In_channel.close ch;
        s
      in
      let source_code = read_whole_file filename in
      (* Let's try simply json stringifying it back instead of sending it to the compiler pipelines *)
      (* json_to_and_from source_code syntax *)
      (* compile to michelson *)
      compile source_code syntax
    | Text (source_code, _syntax) ->
      (* Let's try simply json stringifying it back instead of sending it to the compiler pipelines *)
      (* json_to_and_from source_code syntax *)
      (* compile to michelson *)
      compile source_code syntax
  in
  translate_michelson_result value display_format


(* translate_json_result value display_format *)

let expression
    (raw_options : Raw_options.t)
    expression
    init_file
    display_format
    no_colour
    michelson_format
    ()
  =
  let warning_as_error = raw_options.warning_as_error in
  format_result
    ~warning_as_error
    ~display_format
    ~no_colour
    (Formatter.Michelson_formatter.michelson_format michelson_format [])
  @@ fun ~raise ->
  let syntax = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) init_file in
  let options =
    let protocol_version =
      Helpers.protocol_to_variant ~raise raw_options.protocol_version
    in
    Compiler_options.make
      ~raw_options
      ~syntax
      ~protocol_version
      ~has_env_comments:false
      ()
  in
  let Compiler_options.{ without_run; _ } = options.backend in
  let Compiler_options.{ constants; file_constants; _ } = options.backend in
  let file_constants = read_file_constants ~raise file_constants in
  let constants = constants @ file_constants in
  let mini_c_exp, _ =
    Build.build_expression ~raise ~options syntax expression init_file
  in
  let compiled_exp =
    Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp
  in
  no_comment
  @@
  if without_run
  then Run.clean_expression compiled_exp.expr
  else (
    let options =
      Run.make_dry_run_options
        ~raise
        ~constants
        { now = None
        ; amount = "0"
        ; balance = "0"
        ; sender = None
        ; source = None
        ; parameter_ty = None
        }
    in
    Run.evaluate_expression ~raise ~options compiled_exp.expr compiled_exp.expr_ty)


let constant (raw_options : Raw_options.t) constants init_file display_format no_colour ()
  =
  let warning_as_error = raw_options.warning_as_error in
  format_result
    ~warning_as_error
    ~display_format
    ~no_colour
    Formatter.Michelson_formatter.michelson_constant_format
  @@ fun ~raise ->
  let syntax = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) init_file in
  let options =
    let protocol_version =
      Helpers.protocol_to_variant ~raise raw_options.protocol_version
    in
    Compiler_options.make
      ~raw_options
      ~syntax
      ~protocol_version
      ~has_env_comments:false
      ()
  in
  let Compiler_options.{ without_run; _ } = options.backend in
  let mini_c_exp, _ = Build.build_expression ~raise ~options syntax constants init_file in
  let compiled_exp =
    Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp
  in
  let hash, value =
    if without_run
    then Run.clean_constant ~raise compiled_exp.expr
    else Run.evaluate_constant ~raise compiled_exp.expr compiled_exp.expr_ty
  in
  hash, value


let parameter
    (raw_options : Raw_options.t)
    source_file
    expression
    amount
    balance
    sender
    source
    now
    display_format
    no_colour
    michelson_format
    ()
  =
  let warning_as_error = raw_options.warning_as_error in
  format_result
    ~warning_as_error
    ~display_format
    ~no_colour
    (Formatter.Michelson_formatter.michelson_format michelson_format [])
  @@ fun ~raise ->
  let protocol_version =
    Helpers.protocol_to_variant ~raise raw_options.protocol_version
  in
  let syntax =
    Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
  in
  let options =
    Compiler_options.make
      ~raw_options
      ~syntax
      ~protocol_version
      ~has_env_comments:false
      ()
  in
  let Compiler_options.{ constants; file_constants; _ } = options.backend in
  let Compiler_options.{ entry_point; _ } = options.frontend in
  let file_constants = read_file_constants ~raise file_constants in
  let constants = constants @ file_constants in
  let entry_point = Value_var.of_input_var ~loc entry_point in
  let app_typed_prg =
    Build.qualified_typed ~raise ~options Env (Build.Source_input.From_file source_file)
  in
  let Self_ast_typed.Helpers.{ parameter = parameter_ty; storage = _ } =
    Trace.trace ~raise Main_errors.self_ast_typed_tracer
    @@ Self_ast_typed.Helpers.fetch_contract_type entry_point app_typed_prg
  in
  let parameter_ty = Checking.untype_type_expression parameter_ty in
  let typed_param =
    Trace.try_with
      (fun ~raise ~catch:_ ->
        Ligo_compile.Utils.type_expression
          ~raise
          ~options
          ~annotation:parameter_ty
          syntax
          expression
          app_typed_prg)
      (fun ~catch:_ _ ->
        let typed_param =
          Ligo_compile.Utils.type_expression
            ~raise
            ~options
            syntax
            expression
            app_typed_prg
        in
        let () =
          Ligo_compile.Of_typed.assert_equal_contract_type
            ~raise
            Check_parameter
            entry_point
            app_typed_prg
            typed_param
        in
        typed_param)
  in
  let typed_param, typed_prg =
    Self_ast_typed.remove_unused_expression typed_param app_typed_prg
  in
  let (_ : Mini_c.meta Run.Michelson.michelson) =
    let aggregated_contract =
      Ligo_compile.Of_typed.apply_to_entrypoint_contract
        ~raise
        ~options:options.middle_end
        app_typed_prg
        entry_point
    in
    let expanded =
      Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_contract
    in
    let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded in
    let michelson = Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c in
    (* fails if the given entry point is not a valid contract *)
    Ligo_compile.Of_michelson.build_contract
      ~raise
      ~enable_typed_opt:options.backend.enable_typed_opt
      ~protocol_version
      ~constants
      michelson
      []
  in
  let aggregated_param =
    Ligo_compile.Of_typed.compile_expression_in_context
      ~raise
      ~options:options.middle_end
      typed_prg
      typed_param
  in
  let expanded_param =
    Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_param
  in
  let mini_c_param = Ligo_compile.Of_expanded.compile_expression ~raise expanded_param in
  let compiled_param =
    Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_param
  in
  let () =
    Ligo_compile.Of_typed.assert_equal_contract_type
      ~raise
      Check_parameter
      entry_point
      app_typed_prg
      typed_param
  in
  let options =
    Run.make_dry_run_options
      ~raise
      ~constants
      { now; amount; balance; sender; source; parameter_ty = None }
  in
  no_comment
    (Run.evaluate_expression ~raise ~options compiled_param.expr compiled_param.expr_ty)


let storage
    (raw_options : Raw_options.t)
    source_file
    expression
    amount
    balance
    sender
    source
    now
    display_format
    no_colour
    michelson_format
    ()
  =
  let warning_as_error = raw_options.warning_as_error in
  format_result
    ~warning_as_error
    ~display_format
    ~no_colour
    (Formatter.Michelson_formatter.michelson_format michelson_format [])
  @@ fun ~raise ->
  let protocol_version =
    Helpers.protocol_to_variant ~raise raw_options.protocol_version
  in
  let syntax =
    Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
  in
  let options =
    Compiler_options.make
      ~raw_options
      ~syntax
      ~protocol_version
      ~has_env_comments:false
      ()
  in
  let Compiler_options.{ entry_point; _ } = options.frontend in
  let Compiler_options.{ constants; file_constants; _ } = options.backend in
  let file_constants = read_file_constants ~raise file_constants in
  let constants = constants @ file_constants in
  let entry_point = Value_var.of_input_var ~loc entry_point in
  let app_typed_prg =
    Build.qualified_typed
      ~raise
      ~options
      Ligo_compile.Of_core.Env
      (Build.Source_input.From_file source_file)
  in
  let Self_ast_typed.Helpers.{ parameter = _; storage = storage_ty } =
    Trace.trace ~raise Main_errors.self_ast_typed_tracer
    @@ Self_ast_typed.Helpers.fetch_contract_type entry_point app_typed_prg
  in
  let storage_ty = Checking.untype_type_expression storage_ty in
  let typed_param =
    Trace.try_with
      (fun ~raise ~catch:_ ->
        Ligo_compile.Utils.type_expression
          ~raise
          ~options
          ~annotation:storage_ty
          syntax
          expression
          app_typed_prg)
      (fun ~catch:_ _ ->
        let typed_param =
          Ligo_compile.Utils.type_expression
            ~raise
            ~options
            syntax
            expression
            app_typed_prg
        in
        let () =
          Ligo_compile.Of_typed.assert_equal_contract_type
            ~raise
            Check_storage
            entry_point
            app_typed_prg
            typed_param
        in
        typed_param)
  in
  let typed_param, typed_prg =
    Self_ast_typed.remove_unused_expression typed_param app_typed_prg
  in
  let (_ : Mini_c.meta Run.Michelson.michelson) =
    let aggregated_contract =
      Ligo_compile.Of_typed.apply_to_entrypoint_contract
        ~raise
        ~options:options.middle_end
        app_typed_prg
        entry_point
    in
    let expanded =
      Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_contract
    in
    let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded in
    let michelson = Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c in
    (* fails if the given entry point is not a valid contract *)
    Ligo_compile.Of_michelson.build_contract
      ~raise
      ~enable_typed_opt:options.backend.enable_typed_opt
      ~protocol_version
      ~constants
      michelson
      []
  in
  let aggregated_param =
    Ligo_compile.Of_typed.compile_expression_in_context
      ~raise
      ~options:options.middle_end
      typed_prg
      typed_param
  in
  let expanded_param =
    Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_param
  in
  let mini_c_param = Ligo_compile.Of_expanded.compile_expression ~raise expanded_param in
  let compiled_param =
    Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_param
  in
  let () =
    Ligo_compile.Of_typed.assert_equal_contract_type
      ~raise
      Check_storage
      entry_point
      app_typed_prg
      typed_param
  in
  let options =
    Run.make_dry_run_options
      ~raise
      ~constants
      { now; amount; balance; sender; source; parameter_ty = None }
  in
  no_comment
    (Run.evaluate_expression ~raise ~options compiled_param.expr compiled_param.expr_ty)
