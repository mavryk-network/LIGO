open Simple_utils.Trace
open Main_errors

type meta = {
  syntax : Syntax_types.t;
}

let protocol_to_variant ~raise : string -> Environment.Protocols.t =
  fun s ->
  trace_option ~raise (main_invalid_protocol_version Environment.Protocols.protocols_str s)
  @@ Environment.Protocols.protocols_to_variant s

(* Preprocessing *)

type options = Compiler_options.t

let preprocess_file ~raise ~(options:options) ~(meta: meta) file_path
  : Preprocessing.Pascaligo.success =
  let open Preprocessing in
  let project_root = options.project_root in
  let preprocess_file =
    match meta.syntax with
      PascaLIGO _ -> Pascaligo.preprocess_file
    | CameLIGO    -> Cameligo.preprocess_file
    | ReasonLIGO  -> Reasonligo.preprocess_file
    | JsLIGO      -> Jsligo.preprocess_file
  in trace ~raise preproc_tracer @@
      Simple_utils.Trace.from_result (preprocess_file ?project_root options.libs file_path)

let preprocess_string ~raise ~(options:options) ~(meta: meta) file_path =
  let open Preprocessing in
  let project_root = options.project_root in
  let preprocess_string =
    match meta.syntax with
      PascaLIGO _ -> Pascaligo.preprocess_string
    | CameLIGO    -> Cameligo.preprocess_string
    | ReasonLIGO  -> Reasonligo.preprocess_string
    | JsLIGO      -> Jsligo.preprocess_string
  in trace ~raise preproc_tracer @@
     from_result (preprocess_string ?project_root options.libs file_path)

(* Front-end compilation *)

type file_path = string

let parse_and_abstract_pascaligo ~raise buffer file_path =
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Pascaligo.parse_file buffer file_path in
  let imperative =
    trace ~raise cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_module raw.decl
  in imperative

let parse_and_abstract_expression_pascaligo ~raise buffer =
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Pascaligo.parse_expression buffer in
  let imperative =
    trace ~raise cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_expression raw
  in imperative

let parse_and_abstract_cameligo ~raise buffer file_path =
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Cameligo.parse_file buffer file_path in
  let imperative =
    trace ~raise cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_module raw
  in imperative

let parse_and_abstract_expression_cameligo ~raise buffer =
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Cameligo.parse_expression buffer in
  let imperative =
    trace ~raise cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_expression raw
  in imperative

let parse_and_abstract_reasonligo ~raise buffer file_path =
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Reasonligo.parse_file buffer file_path in
  let imperative =
    trace ~raise cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_module raw
  in imperative

let parse_and_abstract_expression_reasonligo ~raise buffer =
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Reasonligo.parse_expression buffer in
  let imperative =
    trace ~raise cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_expression raw
  in imperative

let parse_and_abstract_jsligo ~raise buffer file_path =
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Jsligo.parse_file buffer file_path in
  let imperative =
    trace ~raise cit_jsligo_tracer @@
    Tree_abstraction.Jsligo.compile_module raw
  in imperative

let parse_and_abstract_expression_jsligo ~raise buffer =
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Jsligo.parse_expression buffer in
  let imperative =
    trace ~raise cit_jsligo_tracer @@
    Tree_abstraction.Jsligo.compile_expression raw
  in imperative

let parse_and_abstract ~raise ~(meta: meta) ~add_warning buffer file_path
    : Ast_imperative.module_ =
  let parse_and_abstract =
    match meta.syntax with
      PascaLIGO _ -> parse_and_abstract_pascaligo
    | CameLIGO    -> parse_and_abstract_cameligo
    | ReasonLIGO  -> parse_and_abstract_reasonligo
    | JsLIGO      -> parse_and_abstract_jsligo in
  let abstracted =
    parse_and_abstract ~raise buffer file_path in
  let applied =
    trace ~raise self_ast_imperative_tracer @@
    Self_ast_imperative.all_module abstracted ~add_warning ~lang:meta.syntax in
  applied

let parse_and_abstract_expression ~raise ~(meta: meta) buffer =
  let parse_and_abstract =
    match meta.syntax with
      PascaLIGO _ ->
        parse_and_abstract_expression_pascaligo
    | CameLIGO    ->
        parse_and_abstract_expression_cameligo
    | ReasonLIGO  ->
        parse_and_abstract_expression_reasonligo
    | JsLIGO      ->
        parse_and_abstract_expression_jsligo
      in
  let abstracted =
    parse_and_abstract ~raise buffer in
  let applied =
    trace ~raise self_ast_imperative_tracer @@
    Self_ast_imperative.all_expression ~lang:meta.syntax abstracted
  in applied

let parse_and_abstract_string_reasonligo ~raise buffer =
  let raw = trace ~raise parser_tracer @@
    Parsing.Reasonligo.parse_string buffer in
  let imperative = trace ~raise cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_module raw
  in imperative

let parse_and_abstract_string_pascaligo ~raise buffer =
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Pascaligo.parse_string buffer in
  let imperative =
    trace ~raise cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_module raw.decl
  in imperative

let parse_and_abstract_string_cameligo ~raise buffer =
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Cameligo.parse_string buffer in
  let imperative =
    trace ~raise cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_module raw
  in imperative

let parse_and_abstract_string_jsligo ~raise buffer =
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Jsligo.parse_string buffer in
  let imperative =
    trace ~raise cit_jsligo_tracer @@
    Tree_abstraction.Jsligo.compile_module raw
  in imperative

let parse_and_abstract_string ~raise ~add_warning (syntax: Syntax_types.t) buffer =
  let parse_and_abstract =
    match syntax with
      PascaLIGO _ ->
        parse_and_abstract_string_pascaligo
    | CameLIGO    ->
        parse_and_abstract_string_cameligo
    | ReasonLIGO  ->
        parse_and_abstract_string_reasonligo
    | JsLIGO      ->
        parse_and_abstract_string_jsligo in
  let abstracted =
    parse_and_abstract ~raise buffer in
  let applied =
    trace ~raise self_ast_imperative_tracer @@
    Self_ast_imperative.all_module abstracted ~add_warning ~lang:syntax
  in applied

let pretty_print_pascaligo_cst =
  Parsing.Pascaligo.pretty_print_cst

let pretty_print_cameligo_cst =
  Parsing.Cameligo.pretty_print_cst

let pretty_print_reasonligo_cst =
  Parsing.Reasonligo.pretty_print_cst

let pretty_print_jsligo_cst =
  Parsing.Jsligo.pretty_print_cst

let pretty_print_cst ~raise ~(meta: meta) buffer file_path=
  let print =
    match meta.syntax with
      PascaLIGO _ -> pretty_print_pascaligo_cst
    | CameLIGO    -> pretty_print_cameligo_cst
    | ReasonLIGO  -> pretty_print_reasonligo_cst
    | JsLIGO      -> pretty_print_jsligo_cst
  in trace ~raise parser_tracer @@ print buffer file_path

let pretty_print_pascaligo =
  Parsing.Pascaligo.pretty_print_file

let pretty_print_cameligo =
  Parsing.Cameligo.pretty_print_file

let pretty_print_reasonligo =
  Parsing.Reasonligo.pretty_print_file

let pretty_print_jsligo =
  Parsing.Jsligo.pretty_print_file

let pretty_print ~raise ~(meta: meta) buffer file_path =
  let print =
    match meta.syntax with
      PascaLIGO _ -> pretty_print_pascaligo
    | CameLIGO    -> pretty_print_cameligo
    | ReasonLIGO  -> pretty_print_reasonligo
    | JsLIGO      -> pretty_print_jsligo
  in trace ~raise parser_tracer @@ print buffer file_path
