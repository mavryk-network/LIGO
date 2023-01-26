open Cli_expect

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "option_map.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (option int) ;
      code { CDR ;
             MAP { PUSH string "foo" ; PAIR } ;
             MAP { CDR } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "option_map.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "option_map.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (option int) ;
      code { CDR ;
             MAP { PUSH string "foo" ; PAIR } ;
             MAP { CDR } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "option_map.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "x"; "--init-file"; test "option.mligo" ];
  [%expect {| None |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "n"; "--init-file"; test "option.mligo" ];
  [%expect {| None |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "option_record.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Option_map_tests.(fun) in file "src/bin/expect_tests/option_map_tests.ml", line 50, characters 2-61
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts/option_record.mligo", line 9, characters 22-37:
    8 | let @test =
    9 |   let (taddr, _, _) = Test.@originate main (None : t) 0tez in
   10 |   let ctr = Test.to_contract taddr in

  Variable "@originate" not found. |}]
