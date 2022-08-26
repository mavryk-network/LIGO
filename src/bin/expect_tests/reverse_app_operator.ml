open Cli_expect

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; (test "reverse_app.mligo") ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value ().
    - test exited with value ().
    - test exited with value ().
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; (bad_test "error_reverse_app.mligo")] ;
  [%expect{|
    File "../../test/contracts/negative/error_reverse_app.mligo", line 4, characters 19-26:
      3 |
      4 | let typing_error = "Hello" |> f

    Invalid type(s).
    Expected: "string", but got: "int". |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; (bad_test "error_reverse_app_2.mligo")] ;
  [%expect{|
    Invalid type(s)
    Cannot unify int -> int with int. |}]