module Location = Simple_utils.Location
open Api_helpers
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Raw_options = Compiler_options.Raw_options

let loc = Location.dummy

let pretty_print (raw_options : Raw_options.t) source_file display_format () =
  let warning_as_error = raw_options.warning_as_error in
  format_result
    ~warning_as_error
    ~display_format
    ~no_colour:raw_options.no_colour
    Parsing.Formatter.ppx_format
  @@ fun ~raise ->
  let syntax =
    Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
  in
  let options = Compiler_options.make ~raw_options ~syntax () in
  let meta = Compile.Of_source.extract_meta syntax in
  Compile.Utils.pretty_print
    ~preprocess:false
    ~raise
    ~options:options.frontend
    ~meta
    source_file


let dependency_graph (raw_options : Raw_options.t) source_file display_format () =
  format_result
    ~display_format
    ~no_colour:raw_options.no_colour
    BuildSystem.Formatter.graph_format
  @@ fun ~raise ->
  let syntax =
    Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
  in
  let options = Compiler_options.make ~raw_options ~syntax () in
  let g, _ = Build.dependency_graph ~raise ~options Env source_file in
  g, source_file


let preprocess (raw_options : Raw_options.t) source_file display_format () =
  format_result
    ~display_format
    ~no_colour:raw_options.no_colour
    Parsing.Formatter.ppx_format
  @@ fun ~raise ->
  fst
  @@
  let syntax =
    Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
  in
  let options = Compiler_options.make ~raw_options ~syntax () in
  let meta = Compile.Of_source.extract_meta syntax in
  Compile.Of_source.preprocess_file ~raise ~options:options.frontend ~meta source_file


let cst
    (raw_options : Raw_options.t)
    ?syntax
    (source : [ `Raw of string | `File of string ])
    display_format
    ()
  =
  let (Simple_utils.Display.Ex_display_format display_format) = display_format in
  let k ~raise =
    let open Simple_utils.Display in
    let open Compile.Utils in
    let syntax =
      match syntax, source with
      | Some v, _ -> v
      | None, `File source ->
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source)
      | None, `Raw _ -> failwith "API expected syntax along with raw source"
    in
    let meta = Compile.Of_source.extract_meta syntax in
    let k : raise:(Parsing.Errors.t, Main_warnings.all) Trace.raise -> 'a =
     fun ~raise ->
      let preprocess = true in
      let buffer =
        match source with
        | `Raw code ->
          let buffer = Buffer.create 0 in
          Buffer.add_string buffer code;
          buffer
        | `File file_path -> buffer_from_path file_path
      in
      match source, meta.syntax with
      | `Raw _code, PascaLIGO ->
        let cst = Parsing.Pascaligo.parse_string ~preprocess ~raise buffer in
        (match display_format with
        | Json -> Yojson.Safe.pretty_to_string @@ `String "TODO"
        | Dev | Human_readable ->
          let buf = Parsing.Pascaligo.pretty_print_cst_cst cst in
          Format.asprintf "%s" (Buffer.contents buf))
      | `File file_path, PascaLIGO ->
        let cst = Parsing.Pascaligo.parse_file ~preprocess ~raise buffer file_path in
        (match display_format with
        | Json -> Yojson.Safe.pretty_to_string @@ `String "TODO"
        | Dev | Human_readable ->
          let buf = Parsing.Pascaligo.pretty_print_cst_cst cst in
          Format.asprintf "%s" (Buffer.contents buf))
      | `File file_path, CameLIGO ->
        let cst = Build.parse_file_to_cst ~preprocess ~raise buffer file_path in
        (match display_format with
        | Json -> Yojson.Safe.pretty_to_string @@ Parsing.Cameligo.CST.to_yojson cst
        | Dev | Human_readable ->
          let buf = Parsing.Cameligo.pretty_print_cst_cst cst in
          Format.asprintf "%s" (Buffer.contents buf))
      | `Raw _code, CameLIGO ->
        let cst = Build.parse_string_to_cst ~preprocess ~raise buffer in
        (match display_format with
        | Json -> Yojson.Safe.pretty_to_string @@ Parsing.Cameligo.CST.to_yojson cst
        | Dev | Human_readable ->
          let buf = Parsing.Cameligo.pretty_print_cst_cst cst in
          Format.asprintf "%s" (Buffer.contents buf))
      | `Raw _code, JsLIGO ->
        let cst = Parsing.Jsligo.parse_string ~preprocess ~raise buffer in
        (match display_format with
        | Json -> Yojson.Safe.pretty_to_string @@ `String "TODO"
        | Dev | Human_readable ->
          let buf = Parsing.Jsligo.pretty_print_cst_cst cst in
          Format.asprintf "%s" (Buffer.contents buf))
      | `File file_path, JsLIGO ->
        let cst = Parsing.Jsligo.parse_file ~preprocess ~raise buffer file_path in
        (match display_format with
        | Json -> Yojson.Safe.pretty_to_string @@ `String "TODO"
        | Dev | Human_readable ->
          let buf = Parsing.Jsligo.pretty_print_cst_cst cst in
          Format.asprintf "%s" (Buffer.contents buf))
    in
    match Trace.to_stdlib_result @@ k with
    | Ok v -> v
    | Error (e, _) -> raise.error (`Pretty_tracer e)
  in
  let value = Trace.to_stdlib_result @@ k in
  match value with
  | Ok ((v, _), _) -> Ok (v, "")
  | Error (e, _) ->
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


let ast (raw_options : Raw_options.t) source_file display_format () =
  format_result
    ~display_format
    ~no_colour:raw_options.no_colour
    Ast_imperative.Formatter.program_format
  @@ fun ~raise ->
  let syntax =
    Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
  in
  let options = Compiler_options.make ~raw_options ~syntax () in
  let meta = Compile.Of_source.extract_meta syntax in
  let c_unit, _ =
    Compile.Of_source.preprocess_file ~raise ~options:options.frontend ~meta source_file
  in
  Compile.Utils.to_imperative ~raise ~options ~meta c_unit source_file


let ast_core (raw_options : Raw_options.t) source_file display_format () =
  format_result
    ~display_format
    ~no_colour:raw_options.no_colour
    Ast_core.Formatter.program_format
  @@ fun ~raise ->
  let syntax =
    Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
  in
  let options = Compiler_options.make ~raw_options ~syntax () in
  let meta = Compile.Of_source.extract_meta syntax in
  let c_unit, _ =
    Compile.Of_source.preprocess_file ~raise ~options:options.frontend ~meta source_file
  in
  let core = Compile.Utils.to_core ~raise ~options ~meta c_unit source_file in
  core


let ast_typed (raw_options : Raw_options.t) source_file display_format () =
  format_result
    ~display_format
    ~no_colour:raw_options.no_colour
    Ast_typed.Formatter.program_format
  @@ fun ~raise ->
  let options =
    (* TODO: options should be computed outside of the API *)
    let syntax =
      Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
    in
    let protocol_version =
      Helpers.protocol_to_variant ~raise raw_options.protocol_version
    in
    Compiler_options.make ~protocol_version ~raw_options ~syntax ()
  in
  let Compiler_options.{ self_pass; _ } = options.tools in
  let typed =
    Build.qualified_typed ~raise ~options Env (Build.Source_input.From_file source_file)
  in
  (* Here, I would like to write this, but it become slow ...
         let typed = Build.unqualified_typed ~raise ~options Env source_file in
      *)
  if self_pass
  then
    Trace.trace ~raise Main_errors.self_ast_typed_tracer
    @@ Self_ast_typed.all_program
         ~warn_unused_rec:options.middle_end.warn_unused_rec
         typed
  else typed


let ast_aggregated (raw_options : Raw_options.t) source_file display_format () =
  format_result
    ~display_format
    ~no_colour:raw_options.no_colour
    Ast_aggregated.Formatter.expression_format
  @@ fun ~raise ->
  let options =
    (* TODO: options should be computed outside of the API *)
    let syntax =
      Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
    in
    let protocol_version =
      Helpers.protocol_to_variant ~raise raw_options.protocol_version
    in
    Compiler_options.make ~protocol_version ~raw_options ~syntax ()
  in
  let Compiler_options.{ self_pass; _ } = options.tools in
  let typed =
    Build.qualified_typed ~raise Env ~options (Build.Source_input.From_file source_file)
  in
  Compile.Of_typed.compile_expression_in_context
    ~raise
    ~options:options.middle_end
    ~self_pass
    typed
    (Ast_typed.e_a_unit ~loc ())


let ast_expanded (raw_options : Raw_options.t) source_file display_format no_colour () =
  format_result ~display_format ~no_colour Ast_expanded.Formatter.expression_format
  @@ fun ~raise ->
  let options =
    (* TODO: options should be computed outside of the API *)
    let syntax =
      Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
    in
    let protocol_version =
      Helpers.protocol_to_variant ~raise raw_options.protocol_version
    in
    Compiler_options.make ~protocol_version ~raw_options ~syntax ()
  in
  let Compiler_options.{ self_pass; _ } = options.tools in
  let typed =
    Build.qualified_typed ~raise Env ~options (Build.Source_input.From_file source_file)
  in
  let aggregated =
    Compile.Of_typed.compile_expression_in_context
      ~raise
      ~options:options.middle_end
      ~self_pass
      typed
      (Ast_typed.e_a_unit ~loc ())
  in
  Compile.Of_aggregated.compile_expression ~raise aggregated


let mini_c (raw_options : Raw_options.t) source_file display_format optimize () =
  format_result
    ~display_format
    ~no_colour:raw_options.no_colour
    Mini_c.Formatter.program_format
  @@ fun ~raise ->
  let options =
    (* TODO: options should be computed outside of the API *)
    let syntax =
      Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
    in
    let protocol_version =
      Helpers.protocol_to_variant ~raise raw_options.protocol_version
    in
    Compiler_options.make ~protocol_version ~raw_options ~syntax ()
  in
  let typed =
    Build.qualified_typed ~raise Env ~options (Build.Source_input.From_file source_file)
  in
  match optimize with
  | None ->
    let expr =
      Compile.Of_typed.compile_expression_in_context
        ~raise
        ~options:options.middle_end
        typed
        (Ast_typed.e_a_unit ~loc ())
    in
    let expanded = Compile.Of_aggregated.compile_expression ~raise expr in
    let mini_c = Compile.Of_expanded.compile_expression ~raise expanded in
    Mini_c.Formatter.Raw mini_c
  | Some entry_point ->
    let expr =
      Compile.Of_typed.apply_to_entrypoint
        ~raise
        ~options:options.middle_end
        typed
        entry_point
    in
    let expanded = Compile.Of_aggregated.compile_expression ~raise expr in
    let mini_c = Compile.Of_expanded.compile_expression ~raise expanded in
    let _, o = Compile.Of_mini_c.optimize_for_contract ~raise options mini_c in
    Mini_c.Formatter.Optimized o.body
