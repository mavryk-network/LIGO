module Requests = Ligo_lsp.Server.Requests
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
open Lsp_helpers

type diagnostics_test =
  { file_path : string
  ; max_number_of_problems : int option
  }

let get_diagnostics_test ({ file_path; max_number_of_problems } : diagnostics_test) : unit
  =
  let file_path_normalized = normalize_path file_path in
  let config =
    Option.map max_number_of_problems ~f:(fun max_number_of_problems ->
        { default_test_config with max_number_of_problems })
  in
  let _uri, actual_diagnostics =
    test_run_session ?config @@ open_file file_path_normalized
  in
  let module Map = Map.Make (Path) in
  let to_map l =
    match Map.of_alist l with
    | `Ok map -> map
    | `Duplicate_key path -> failwithf "Key duplication: %s." (Path.to_string path) ()
  in
  let actual =
    actual_diagnostics
    |> Requests.Handler.Path_hashtbl.to_alist
    |> to_map
    |> Map.to_alist
    |> List.map ~f:(fun (path, diags) -> path_to_relative path, diags)
  in
  Format.printf "%a" Fmt.Dump.(list @@ pair String.pp (list Diagnostic.pp)) actual


let%expect_test "Type errors" =
  get_diagnostics_test
    { file_path = "contracts/negative/error_typer_1.mligo"
    ; max_number_of_problems = None
    };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/negative/error_typer_1.mligo",
      [{
         "message": "This expression has type \"int\", but an expression was expected of type \n\"string\".\nType \"int\" is not compatible with type \"string\".",
         "range": {
           "end": { "character": 27, "line": 2 },
           "start": { "character": 19, "line": 2 }
         },
         "severity": 1
       };
       {
         "message": "Variable \"foo\" not found. ",
         "range": {
           "end": { "character": 34, "line": 5 },
           "start": { "character": 31, "line": 5 }
         },
         "severity": 1
       }])] |}]

let%expect_test "Syntax error" =
  get_diagnostics_test
    { file_path = "contracts/lsp/syntax_error.mligo"; max_number_of_problems = None };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/syntax_error.mligo",
      [{
         "message": "Ill-formed contract.\nAt this point, if the current declaration is complete, one of the\nfollowing is expected:\n  * another declaration;\n  * the end of the file.\n",
         "range": {
           "end": { "character": 11, "line": 0 },
           "start": { "character": 10, "line": 0 }
         },
         "severity": 1
       }])] |}]

let%expect_test "Warnings" =
  get_diagnostics_test
    { file_path = "contracts/lsp/warnings.jsligo"; max_number_of_problems = None };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/warnings.jsligo",
      [{
         "message": "Toplevel let declaration is silently changed to const declaration.",
         "range": {
           "end": { "character": 17, "line": 0 },
           "start": { "character": 7, "line": 0 }
         },
         "severity": 2
       };
       {
         "message": "\nWarning: unused variable \"x\".\nHint: replace it by \"_x\" to prevent this warning.\n",
         "range": {
           "end": { "character": 11, "line": 2 },
           "start": { "character": 10, "line": 2 }
         },
         "severity": 2
       }])] |}]

let%expect_test "Syntax and type errors" =
  get_diagnostics_test
    { file_path = "contracts/lsp/syntax_plus_type_errors.jsligo"
    ; max_number_of_problems = None
    };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/syntax_plus_type_errors.jsligo",
      [{
         "message": "Invalid type(s).\nExpected \"string\", but got: \"int\".",
         "range": {
           "end": { "character": 21, "line": 2 },
           "start": { "character": 19, "line": 2 }
         },
         "severity": 1
       };
       {
         "message": "Ill-formed expression.\nAt this point, an expression is expected.\n",
         "range": {
           "end": { "character": 18, "line": 4 },
           "start": { "character": 15, "line": 4 }
         },
         "severity": 1
       };
       {
         "message": "Toplevel let declaration is silently changed to const declaration.",
         "range": {
           "end": { "character": 18, "line": 4 },
           "start": { "character": 15, "line": 4 }
         },
         "severity": 2
       };
       {
         "message": "Ill-formed value declaration.\nAt this point, a pattern is expected, e.g. a variable.\n",
         "range": {
           "end": { "character": 18, "line": 4 },
           "start": { "character": 18, "line": 4 }
         },
         "severity": 1
       }])] |}]

let%expect_test "All OK" =
  get_diagnostics_test
    { file_path = "contracts/lsp/simple.mligo"; max_number_of_problems = None };
  [%expect {| [("../../../../../default/src/test/contracts/lsp/simple.mligo", [])] |}]

let%expect_test "Limit from 11 to 2 diagnostics in session" =
  get_diagnostics_test
    { file_path = "contracts/warning_sum_types.mligo"; max_number_of_problems = Some 2 };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/warning_sum_types.mligo",
      [{
         "message": "Warning: The type of \"TopTop(42)\" is ambiguous: Inferred type is \"ttop2\" but could be of type \"ttop\".\nHint: You might want to add a type annotation. \n",
         "range": {
           "end": { "character": 23, "line": 85 },
           "start": { "character": 14, "line": 85 }
         },
         "severity": 2
       };
       {
         "message": "Warning: The type of \"TopA(42)\" is ambiguous: Inferred type is \"ta\" but could be of type \"ttop\".\nHint: You might want to add a type annotation. \n",
         "range": {
           "end": { "character": 21, "line": 87 },
           "start": { "character": 14, "line": 87 }
         },
         "severity": 2
       }])] |}]

let%expect_test "Polymorphic Type error" =
  get_diagnostics_test
    { file_path = "contracts/lsp/poly_type_error.mligo"; max_number_of_problems = None };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/poly_type_error.mligo",
      [{
         "message": "This expression has type \"int\", but an expression was expected of type \n\"( ^a * ^b ) -> ^a\".\nType \"int\" is not compatible with type \"( ^a * ^b ) -> ^a\".\nHint: \"^b\", \"^a\" represent placeholder type(s).\n",
         "range": {
           "end": { "character": 22, "line": 0 },
           "start": { "character": 21, "line": 0 }
         },
         "severity": 1
       }])] |}]

let%expect_test "No diagnostics for imported package." =
  get_diagnostics_test
    { file_path = "contracts/lsp/registry.jsligo"; max_number_of_problems = None };
  [%expect {| [("../../../../../default/src/test/contracts/lsp/registry.jsligo", [])] |}]

let%expect_test "Shows diagnostics from another file." =
  get_diagnostics_test
    { file_path = "contracts/lsp/import_warnings.jsligo"; max_number_of_problems = None };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/import_warnings.jsligo", []);
     ("../../../../../default/src/test/contracts/lsp/warnings.jsligo",
      [{
         "message": "\nWarning: unused variable \"x\".\nHint: replace it by \"_x\" to prevent this warning.\n",
         "range": {
           "end": { "character": 11, "line": 2 },
           "start": { "character": 10, "line": 2 }
         },
         "severity": 2
       };
       {
         "message": "Toplevel let declaration is silently changed to const declaration.",
         "range": {
           "end": { "character": 17, "line": 0 },
           "start": { "character": 7, "line": 0 }
         },
         "severity": 2
       }])] |}]

let%expect_test "Shows TZIP-16 checks with a top-level storage." =
  get_diagnostics_test
    { file_path = "contracts/lsp/test_metadata.mligo"; max_number_of_problems = None };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/test_metadata.mligo",
      [{
         "message": "Warning: If the following metadata is meant to be TZIP-16 compliant,\nthen it should be a 'big_map' from 'string' to 'bytes'.\nHint: The corresponding type should be :\n  (string, bytes) big_map\nYou can disable this warning with the '--no-metadata-check' flag.\n",
         "range": {
           "end": { "character": 19, "line": 2 },
           "start": { "character": 15, "line": 2 }
         },
         "severity": 2
       }])] |}]

let%expect_test "Shows a duplicate entrypoint error." =
  get_diagnostics_test
    { file_path = "contracts/lsp/entrypoints_repeated.mligo"
    ; max_number_of_problems = None
    };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/entrypoints_repeated.mligo",
      [{
         "message": "Duplicate entry-point ep_int",
         "range": {
           "end": { "character": 10, "line": 1 },
           "start": { "character": 4, "line": 1 }
         },
         "severity": 1
       }])] |}]

let%expect_test "Shows an error when two toplevel entrypoints have different storage." =
  get_diagnostics_test
    { file_path = "contracts/lsp/entrypoints_different_storage.mligo"
    ; max_number_of_problems = None
    };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/entrypoints_different_storage.mligo",
      [{
         "message": "Storage types do not match for different entrypoints:\n- ep_int : int\n- ep_string : string",
         "range": {
           "end": { "character": 10, "line": 1 },
           "start": { "character": 4, "line": 1 }
         },
         "severity": 1
       }])] |}]

let%expect_test "Shows views-related errors and storage warnings." =
  get_diagnostics_test
    { file_path = "contracts/lsp/entrypoints_views.mligo"; max_number_of_problems = None };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/entrypoints_views.mligo",
      [{
         "message": "Warning: If the following metadata is meant to be TZIP-16 compliant,\nthen it should be a 'big_map' from 'string' to 'bytes'.\nHint: The corresponding type should be :\n  (string, bytes) big_map\nYou can disable this warning with the '--no-metadata-check' flag.\n",
         "range": {
           "end": { "character": 20, "line": 4 },
           "start": { "character": 16, "line": 4 }
         },
         "severity": 2
       };
       {
         "message": "The view \"bad_view_not_func\" is not a function.",
         "range": {
           "end": { "character": 21, "line": 21 },
           "start": { "character": 4, "line": 21 }
         },
         "severity": 1
       }])] |}]

let%expect_test "Shows entrypoint-related errors in many modules simultaneously." =
  get_diagnostics_test
    { file_path = "contracts/lsp/entrypoints_modules.mligo"
    ; max_number_of_problems = None
    };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/entrypoints_modules.mligo",
      [{
         "message": "Storage types do not match for different entrypoints:\n- ep_string : string\n- ep_int : int",
         "range": {
           "end": { "character": 15, "line": 23 },
           "start": { "character": 6, "line": 23 }
         },
         "severity": 1
       };
       {
         "message": "Duplicate entry-point ep_string",
         "range": {
           "end": { "character": 15, "line": 32 },
           "start": { "character": 6, "line": 32 }
         },
         "severity": 1
       };
       {
         "message": "Not an entrypoint: unit -> ( list (operation) * string )",
         "range": {
           "end": { "character": 12, "line": 41 },
           "start": { "character": 6, "line": 41 }
         },
         "severity": 1
       };
       {
         "message": "Invalid type for view \"Bad_4\".\nA view must be a function.",
         "range": {
           "end": { "character": 7, "line": 51 },
           "start": { "character": 6, "line": 51 }
         },
         "severity": 1
       }])] |}]

let%expect_test "ghost_ident filter" =
  get_diagnostics_test
    { file_path = "contracts/lsp/missing_value.mligo"; max_number_of_problems = None };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/missing_value.mligo",
      [{
         "message": "Ill-formed value declaration.\nAt this point, an expression is expected.\n",
         "range": {
           "end": { "character": 7, "line": 0 },
           "start": { "character": 7, "line": 0 }
         },
         "severity": 1
       }])] |}]

let%expect_test "ghost string filter" =
  get_diagnostics_test
    { file_path = "contracts/lsp/missing_string.jsligo"; max_number_of_problems = None };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/missing_string.jsligo",
      [{
         "message": "Ill-formed variant type.\nAt this point, a string denoting a constructor is expected.\n",
         "range": {
           "end": { "character": 25, "line": 0 },
           "start": { "character": 24, "line": 0 }
         },
         "severity": 1
       };
       {
         "message": "Expected constructor \"Tail\" in expected sum type \"coin\".",
         "range": {
           "end": { "character": 23, "line": 4 },
           "start": { "character": 17, "line": 4 }
         },
         "severity": 1
       }])] |}]

let%expect_test "Ghost_ident filter" =
  get_diagnostics_test
    { file_path = "contracts/lsp/missing_module_name.mligo"
    ; max_number_of_problems = None
    };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/missing_module_name.mligo",
      [{
         "message": "Ill-formed module declaration.\nAt this point, the name of the module being declared is expected.\n",
         "range": {
           "end": { "character": 8, "line": 0 },
           "start": { "character": 7, "line": 0 }
         },
         "severity": 1
       }])] |}]

let%expect_test "Shows comparing error and suggests to use functions from Test module" =
  get_diagnostics_test
    { file_path = "contracts/lsp/diagnostics_equal.mligo"; max_number_of_problems = None };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/diagnostics_equal.mligo",
      [{
         "message": "Invalid arguments.\nThese types cannot be compared: \"list (int)\" and \"list (int)\".\nUse \"Test.equal\", \"Test.not_equal\", \"Test.greater\", \"Test.less\", \"Test.greater_or_equal\", or \"Test.less_or_equal\" to compare lists, maps, sets, etc.",
         "range": {
           "end": { "character": 27, "line": 2 },
           "start": { "character": 12, "line": 2 }
         },
         "severity": 1
       }])] |}]

let%expect_test "Shows wrong test primitive usage error" =
  get_diagnostics_test
    { file_path = "contracts/lsp/diagnostics_wrong_usage_of_test_primitives.mligo"
    ; max_number_of_problems = None
    };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/diagnostics_wrong_usage_of_test_primitives.mligo",
      [{
         "message": "Invalid usage of a Test primitive.",
         "range": {
           "end": { "character": 1, "line": 0 },
           "start": { "character": 0, "line": 0 }
         },
         "severity": 1
       }])] |}]

let%expect_test "Shows a warning for deprecated functions" =
  get_diagnostics_test
    { file_path = "contracts/deprecated.mligo"; max_number_of_problems = None };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/deprecated.mligo",
      [{
         "message": "\nWarning: deprecated value.\nReplace me by...\ng!\nmail: foo@bar.com\n",
         "range": {
           "end": { "character": 75, "line": 4 },
           "start": { "character": 74, "line": 4 }
         },
         "severity": 2
       }])] |}]

let%expect_test "No diagnostics for dynamic entrypoints (mligo)." =
  get_diagnostics_test
    { file_path = "contracts/dynamic_entrypoints.mligo"; max_number_of_problems = None };
  [%expect
    {| [("../../../../../default/src/test/contracts/dynamic_entrypoints.mligo", [])] |}]

let%expect_test "No diagnostics for dynamic entrypoints (jsligo)." =
  get_diagnostics_test
    { file_path = "contracts/dynamic_entrypoints.jsligo"; max_number_of_problems = None };
  [%expect
    {| [("../../../../../default/src/test/contracts/dynamic_entrypoints.jsligo", [])] |}]

let%expect_test "Shows errors for unsupported record fields (jsligo)." =
  get_diagnostics_test
    { file_path = "contracts/lsp/unsupported_record_field.jsligo"
    ; max_number_of_problems = None
    };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/unsupported_record_field.jsligo",
      [{
         "message": "Unsupported object field",
         "range": {
           "end": { "character": 33, "line": 6 },
           "start": { "character": 32, "line": 6 }
         },
         "severity": 1
       }])] |}]

let%expect_test "Shows errors for unbound variables in records (jsligo)." =
  get_diagnostics_test
    { file_path = "contracts/lsp/unbound_var_in_record.jsligo"
    ; max_number_of_problems = None
    };
  [%expect
    {|
    [("../../../../../default/src/test/contracts/lsp/unbound_var_in_record.jsligo",
      [{
         "message": "Variable \"a\" not found. ",
         "range": {
           "end": { "character": 38, "line": 1 },
           "start": { "character": 37, "line": 1 }
         },
         "severity": 1
       }])] |}]
