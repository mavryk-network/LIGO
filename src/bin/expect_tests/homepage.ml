open Cli_expect

let base path =
  "../../../gitlab-pages/website/src/components/HomepageCodeExamples/" ^ path


let%expect_test _ =
  run_ligo_good [ "run"; "test"; base "cameligo.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Homepage.(fun) in file "src/bin/expect_tests/homepage.ml", line 8, characters 2-56
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../gitlab-pages/website/src/components/HomepageCodeExamples/cameligo.mligo", line 28, characters 21-36:
   27 | let test_initial_storage =
   28 |  let (taddr, _, _) = Test.@originate main initial_storage 0tez in
   29 |  assert (Test.get_storage taddr = initial_storage)

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; base "jsligo.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_initial_storage exited with value ().
    - test_increment exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; base "cameligo.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Homepage.(fun) in file "src/bin/expect_tests/homepage.ml", line 38, characters 2-64
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../gitlab-pages/website/src/components/HomepageCodeExamples/cameligo.mligo", line 28, characters 21-36:
   27 | let test_initial_storage =
   28 |  let (taddr, _, _) = Test.@originate main initial_storage 0tez in
   29 |  assert (Test.get_storage taddr = initial_storage)

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; base "jsligo.jsligo" ];
  [%expect
    {|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}]
