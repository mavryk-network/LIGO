open Ligo_api_common
open Api_helpers
open Simple_utils.Trace
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Raw_options = Compiler_options.Raw_options

let mutate_ast (raw_options : Raw_options.t) source_file display_format _seed () =
  format_result ~display_format Parsing.Formatter.ppx_format
  @@ fun ~raise ->
  let protocol_version =
    Helpers.protocol_to_variant ~raise raw_options.protocol_version
  in
  let syntax =
    Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
  in
  let options = Compiler_options.make ~raw_options ~syntax ~protocol_version () in
  let meta = Compile.Of_source.extract_meta syntax in
  let c_unit, _ =
    Compile.Of_source.preprocess_file ~raise ~options:options.frontend ~meta source_file
  in
  let imperative_prg =
    Compile.Utils.to_imperative ~raise ~options ~meta c_unit source_file
  in
  let syntax = Syntax.to_string meta.syntax in
  let buffer =
    Decompile.Of_imperative.decompile ~raise imperative_prg (Syntax_name syntax)
  in
  buffer


let mutate_cst (raw_options : Raw_options.t) source_file display_format _seed () =
  format_result ~display_format Parsing.Formatter.ppx_format
  @@ fun ~raise ->
  let protocol_version =
    Helpers.protocol_to_variant ~raise raw_options.protocol_version
  in
  let syntax =
    Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
  in
  let options = Compiler_options.make ~raw_options ~syntax ~protocol_version () in
  let meta = Compile.Of_source.extract_meta syntax in
  let c_unit, _ =
    Compile.Of_source.preprocess_file ~raise ~options:options.frontend ~meta source_file
  in
  match meta with
  | { syntax = CameLIGO } ->
    let raw =
      trace ~raise Main_errors.parser_tracer
      @@ Parsing.Cameligo.parse_file c_unit source_file
    in
    let buffer = Parsing.Cameligo.pretty_print raw in
    buffer
  | { syntax = PascaLIGO } ->
    let raw =
      trace ~raise Main_errors.parser_tracer
      @@ Parsing.Pascaligo.parse_file c_unit source_file
    in
    let buffer = Parsing.Pascaligo.pretty_print raw in
    buffer
  | { syntax = JsLIGO } ->
    let raw =
      trace ~raise Main_errors.parser_tracer
      @@ Parsing.Jsligo.parse_file c_unit source_file
    in
    let buffer = Parsing.Jsligo.pretty_print raw in
    buffer
