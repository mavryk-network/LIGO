open Trace
open Main_errors

type s_syntax = Syntax_name of string
type v_syntax = PascaLIGO | CameLIGO | ReasonLIGO

type meta = {
  syntax : v_syntax;
}

let protocol_to_variant : string -> (Environment.Protocols.t, all) result =
  fun s ->
  trace_option (invalid_protocol_version Environment.Protocols.protocols_str s)
  @@ Environment.Protocols.protocols_to_variant s

let get_initial_env  : string -> (Ast_typed.environment, all) result =
  fun protocol_as_str ->
  let%bind protocol = protocol_to_variant protocol_as_str in
  ok @@ Environment.default protocol

let syntax_to_variant (Syntax_name syntax) source =
  match syntax, source with
    "auto", Some sf ->
      (match Filename.extension sf with
         ".ligo" | ".pligo" -> ok PascaLIGO
       | ".mligo"           -> ok CameLIGO
       | ".religo"          -> ok ReasonLIGO
       | ext                -> fail (syntax_auto_detection ext))
  | ("pascaligo" | "PascaLIGO"),   _ -> ok PascaLIGO
  | ("cameligo" | "CameLIGO"),     _ -> ok CameLIGO
  | ("reasonligo" | "ReasonLIGO"), _ -> ok ReasonLIGO
  | _ -> fail (invalid_syntax syntax)

let typer_switch_to_variant t =
  match t with
  | "old" -> ok Ast_typed.Old
  | "new" -> ok Ast_typed.New
  | _ -> fail (invalid_typer_switch t)

(* Preprocessing *)

type options = Compiler_options.t

let preprocess_file ~(options:options) ~meta file_path =
  let open Preprocessing in
  let preprocess_file =
    match meta.syntax with
      PascaLIGO  -> Pascaligo.preprocess_file
    | CameLIGO   -> Cameligo.preprocess_file
    | ReasonLIGO -> Reasonligo.preprocess_file
  in trace preproc_tracer @@
     preprocess_file options.libs file_path

let preprocess_string ~(options:options) ~meta file_path =
  let open Preprocessing in
  let preprocess_string =
    match meta.syntax with
      PascaLIGO  -> Pascaligo.preprocess_string
    | CameLIGO   -> Cameligo.preprocess_string
    | ReasonLIGO -> Reasonligo.preprocess_string
  in trace preproc_tracer @@
     preprocess_string options.libs file_path

(* Lexing only *)

(* TODO *)

(* Parsing only *)

(* TODO *)

(* Front-end compilation *)

type file_path = string

let parse_and_abstract_pascaligo buffer file_path =
  let%bind raw =
    trace parser_tracer @@
    Parsing.Pascaligo.parse_file buffer file_path in
  let%bind imperative =
    trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_program raw
  in ok imperative

let parse_and_abstract_expression_pascaligo buffer =
  let%bind raw =
    trace parser_tracer @@
    Parsing.Pascaligo.parse_expression buffer in
  let%bind imperative =
    trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_expression raw
  in ok imperative

let parse_and_abstract_cameligo buffer file_path =
  let%bind raw =
    trace parser_tracer @@
    Parsing.Cameligo.parse_file buffer file_path in
  let%bind imperative =
    trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_program raw
  in ok imperative

let parse_and_abstract_expression_cameligo buffer =
  let%bind raw =
    trace parser_tracer @@
    Parsing.Cameligo.parse_expression buffer in
  let%bind imperative =
    trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_expression raw
  in ok imperative

let parse_and_abstract_reasonligo buffer file_path =
  let%bind raw =
    trace parser_tracer @@
    Parsing.Reasonligo.parse_file buffer file_path in
  let%bind imperative =
    trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_program raw
  in ok imperative

let parse_and_abstract_expression_reasonligo buffer =
  let%bind raw =
    trace parser_tracer @@
    Parsing.Reasonligo.parse_expression buffer in
  let%bind imperative =
    trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_expression raw
  in ok imperative

let parse_and_abstract ~meta buffer file_path
    : (Ast_imperative.program, _) Trace.result =
  let%bind parse_and_abstract =
    match meta.syntax with
      PascaLIGO  -> ok parse_and_abstract_pascaligo
    | CameLIGO   -> ok parse_and_abstract_cameligo
    | ReasonLIGO -> ok parse_and_abstract_reasonligo in
  let%bind abstracted =
    parse_and_abstract buffer file_path in
  let%bind applied =
    trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_program abstracted in
  ok applied

let parse_and_abstract_expression ~meta buffer =
  let%bind parse_and_abstract =
    match meta.syntax with
      PascaLIGO ->
        ok parse_and_abstract_expression_pascaligo
    | CameLIGO ->
        ok parse_and_abstract_expression_cameligo
    | ReasonLIGO ->
        ok parse_and_abstract_expression_reasonligo in
  let%bind abstracted =
    parse_and_abstract buffer in
  let%bind applied =
    trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_expression abstracted
  in ok applied

let parse_and_abstract_string_reasonligo buffer =
  let%bind raw = trace parser_tracer @@
    Parsing.Reasonligo.parse_string buffer in
  let%bind imperative = trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_program raw
  in ok imperative

let parse_and_abstract_string_pascaligo buffer =
  let%bind raw =
    trace parser_tracer @@
    Parsing.Pascaligo.parse_string buffer in
  let%bind imperative =
    trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_program raw
  in ok imperative

let parse_and_abstract_string_cameligo buffer =
  let%bind raw =
    trace parser_tracer @@
    Parsing.Cameligo.parse_string buffer in
  let%bind imperative =
    trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_program raw
  in ok imperative

let parse_and_abstract_string syntax buffer =
  let%bind parse_and_abstract =
    match syntax with
      PascaLIGO ->
        ok parse_and_abstract_string_pascaligo
    | CameLIGO ->
        ok parse_and_abstract_string_cameligo
    | ReasonLIGO ->
        ok parse_and_abstract_string_reasonligo in
  let%bind abstracted =
    parse_and_abstract buffer in
  let%bind applied =
    trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_program abstracted
  in ok applied

let pretty_print_pascaligo_cst =
  Parsing.Pascaligo.pretty_print_cst

let pretty_print_cameligo_cst =
  Parsing.Cameligo.pretty_print_cst

let pretty_print_reasonligo_cst =
  Parsing.Reasonligo.pretty_print_cst

let pretty_print_cst ~meta buffer file_path=
  let print =
    match meta.syntax with
      PascaLIGO  -> pretty_print_pascaligo_cst
    | CameLIGO   -> pretty_print_cameligo_cst
    | ReasonLIGO -> pretty_print_reasonligo_cst
  in trace parser_tracer @@ print buffer file_path

let pretty_print_pascaligo =
  Parsing.Pascaligo.pretty_print_file

let pretty_print_cameligo =
  Parsing.Cameligo.pretty_print_file

let pretty_print_reasonligo =
  Parsing.Reasonligo.pretty_print_file

let pretty_print ~meta buffer file_path =
  let print =
    match meta.syntax with
      PascaLIGO  -> pretty_print_pascaligo
    | CameLIGO   -> pretty_print_cameligo
    | ReasonLIGO -> pretty_print_reasonligo
  in trace parser_tracer @@ print buffer file_path
