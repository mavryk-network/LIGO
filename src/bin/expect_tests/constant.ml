open Cli_expect

let contracts basename = "../../test/contracts/" ^ basename

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; contracts "constant.mligo" ];
  [%expect {|
    File "../../test/contracts/constant.mligo", line 4, characters 20-21:
      3 | // Errors. Should report: Expected: "nat", but got: "int"
      4 | let bar : nat = foo 1

    Invalid type(s).
    Expected: "nat", but got: "int". |}]