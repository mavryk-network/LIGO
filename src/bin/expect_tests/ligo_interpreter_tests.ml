open Cli_expect

let test basename = "./" ^ basename
let pwd = Caml.Sys.getcwd ()
let () = Caml.Sys.chdir "../../test/contracts/interpreter_tests/"

(* test comparison on sum/record types *)
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_compare.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_cmp exited with value ().
    - test_cmp_list exited with value ().
    - test_cmp_record exited with value (). |}]

(* events payload being records and not decompiled to pairs in the interpreter *)
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_events_pair_vs_record.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 19, characters 2-74
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_events_pair_vs_record.mligo", line 11, characters 19-34:
   10 | let test_foo =
   11 |   let (ta, _, _) = Test.@originate main () 0tez in
   12 |   let _ = Test.transfer_to_contract_exn (Test.to_contract ta) {num1 = 1n ; num2 = 2n} 0tez in

  Variable "@originate" not found. |}]

(* decompilation of timestamp *)
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_timestamp_contract.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 42, characters 2-71
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_timestamp_contract.mligo", line 17, characters 22-37:
   16 |
   17 |   let (taddr, _, _) = Test.@originate main init_storage 0mutez in
   18 |   let contr = Test.to_contract taddr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "interpret_test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_lambda_call exited with value ().
    - test_higher_order1 exited with value ().
    - test_higher_order2 exited with value ().
    - test_higher_order3 exited with value ().
    - test_higher_order4 exited with value ().
    - test_concats exited with value ().
    - test_record_concat exited with value ().
    - test_record_patch exited with value ().
    - test_record_lambda exited with value ().
    - test_variant_match exited with value ().
    - test_bool_match exited with value ().
    - test_list_match exited with value ().
    - test_tuple_proj exited with value ().
    - test_list_const exited with value ().
    - test_options_match_some exited with value ().
    - test_options_match_none exited with value ().
    - test_is_nat_yes exited with value ().
    - test_is_nat_no exited with value ().
    - test_abs_int exited with value ().
    - test_nat_int exited with value ().
    - test_map_list exited with value ().
    - test_fold_list exited with value ().
    - test_comparison_int exited with value ().
    - test_comparison_string exited with value ().
    - test_divs_int exited with value ().
    - test_divs_nat exited with value ().
    - test_var_neg exited with value ().
    - test_sizes exited with value ().
    - test_modi exited with value ().
    - test_assertion_pass exited with value ().
    - test_map_finds exited with value ().
    - test_map_fold exited with value ().
    - test_map_map exited with value ().
    - test_map_mem exited with value ().
    - test_map_remove exited with value ().
    - test_map_update exited with value ().
    - test_set_add exited with value ().
    - test_set_mem exited with value ().
    - test_set_remove exited with value ().
    - test_recursion_let_rec_in exited with value ().
    - test_top_level_recursion exited with value ().
    - test_bitwise_ops exited with value ().
    - test_bitwise_module exited with value ().
    - test_list_concat exited with value ().
    - test_list_head_opt exited with value ().
    - test_list_tail_opt exited with value ().
    - test_list_reverse exited with value ().
    - test_set_fold_desc exited with value ().
    - test_set_update exited with value ().
    - test_map_get_and_update exited with value ().
    - test_big_map_get_and_update exited with value ().
    - test_add_mutez exited with value ().
    - test_sub_mutez exited with value ().
    - test_div_mutez exited with value ().
    - test_sub_timestamp exited with value ().
    - test_list_fold_left_sum exited with value ().
    - test_bytes_sub exited with value ().
    - test_with_error exited with value ().
    - test_some exited with value ().
    - test_some_with_error exited with value ().
    - test_none exited with value ().
    - test_none_with_error exited with value ().
    - test_unopt exited with value ().
    - test_unopt_with_error exited with value ().
    - test_sha256 exited with value ().
    - test_sha512 exited with value ().
    - test_blake2b exited with value ().
    - test_keccak exited with value ().
    - test_sha3 exited with value ().
    - test_key_hash exited with value ().
    - test_check exited with value ().
    - test_int_bls exited with value ().
    - test_not exited with value ().
    - test_chain_id exited with value ().
    - test_concats exited with value (). |}]

let%expect_test _ =
  (* This tests a possible regression on the way modules are evaluated. It is possible that the number of element in the environment explodes. *)
  run_ligo_good [ "run"; "test"; test "imported_modules/test.mligo"; "--format"; "dev" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 146, characters 2-88
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./imported_modules/test.mligo", line 4, characters 16-31:
    3 | let test1 =
    4 |   let (_,_,_) = Test.@originate Main.main "a" 1tez in
    5 |   ()

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "views_test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "interpret_test_log.mligo" ];
  [%expect
    {|
    {a = 1 ; b = 2n ; c = "aaa"}
    One (())
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_fail.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 182, characters 2-57
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_fail.mligo", line 4, characters 29-44:
    3 | let @test =
    4 |   let (typed_addr,_code,_) = Test.@originate main () 0tez in
    5 |   let contr = Test.to_contract typed_addr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_fail_from_file.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "compile_expr.mligo" ];
  [%expect
    {|
  Everything at the top-level was executed.
  - test1 exited with value ().
  - test2 exited with value ().
  - test3 exited with value ().
  - test4 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "compile_expr_from_file.mligo" ];
  [%expect
    {|
  Everything at the top-level was executed.
  - test1 exited with value ().
  - test2 exited with value ().
  - test3 exited with value ().
  - test4 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_example.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 230, characters 2-60
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_example.mligo", line 18, characters 34-49:
   17 |
   18 |   let (typed_addr, _code, size) = Test.@originate main (None : storage) 0tez in
   19 |   let () = assert ((None : storage) = (Test.get_storage typed_addr : storage)) in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_example.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 252, characters 2-60
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_example.mligo", line 18, characters 34-49:
   17 |
   18 |   let (typed_addr, _code, size) = Test.@originate main (None : storage) 0tez in
   19 |   let () = assert ((None : storage) = (Test.get_storage typed_addr : storage)) in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "catch_balance_too_low.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 274, characters 2-69
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./catch_balance_too_low.mligo", line 6, characters 35-50:
    5 |
    6 |   let (typed_addr, _code, _size) = Test.@originate main (None : storage) 0tez in
    7 |

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_subst_with_storage.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 296, characters 2-71
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_subst_with_storage.mligo", line 5, characters 29-44:
    4 |   let init_storage = {foo = 0 ; bar = "bar"} in
    5 |   let (addr, _code, _size) = Test.@originate main init_storage 0tez in
    6 |   let store = Test.get_storage addr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_subst_with_storage_from_file.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "nesting_modules.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 324, characters 2-63
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./nesting_modules.mligo", line 31, characters 22-37:
   30 | let @test =
   31 |   let (taddr, _, _) = Test.@originate main 0 0tez in
   32 |   let c = Test.to_contract taddr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "map_map.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value ["one" -> "foo" ; "two" -> "foo"]. |}]

(* DEPRECATED
let%expect_test _ =
run_ligo_good ["run";"test" ; test "bootstrapped_contracts.mligo" ] ;
  [%expect {|
  "Initial states:"
  (Pair "KT1CSKPf2jeLpMmrgKquN2bCjBTkAcAdRVDy" 12)
  (Pair "KT1QuofAgnsWffHzLA7D78rxytJruGHDe7XG" 9)
  "Final states:"
  (Pair "KT1CSKPf2jeLpMmrgKquN2bCjBTkAcAdRVDy" 3)
  (Pair "KT1QuofAgnsWffHzLA7D78rxytJruGHDe7XG" 0)
  Everything at the top-level was executed.
  - test_transfer exited with value ().
  |}]
*)

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "override_function.mligo" ];
  [%expect
    {|
    4
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_fresh.mligo" ];
  [%expect {| Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_rec_contract.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 379, characters 2-65
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_rec_contract.mligo", line 5, characters 22-37:
    4 | let @test =
    5 |   let (taddr, _, _) = Test.@originate main () 0tez in
    6 |   let _contr = Test.to_contract taddr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_importer.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 401, characters 2-61
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_importer.mligo", line 4, characters 22-37:
    3 | let @test =
    4 |   let (taddr, _, _) = Test.@originate External.main External.D.default.initial 0tez in
    5 |   let contr = Test.to_contract taddr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_bigmap.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 423, characters 2-59
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_bigmap.mligo", line 9, characters 23-38:
    8 |   let init = Big_map.add 12 42n (Big_map.empty : storage) in
    9 |   let (_taddr, _, _) = Test.@originate main init 0tez in
   10 |   let init = Big_map.add 32 42n (Big_map.empty : storage) in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_bigmap_compare.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 445, characters 2-67
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_bigmap_compare.mligo", line 8, characters 23-38:
    7 |     let initial_storage = Big_map.literal [((a1, 0n), 42n)] in
    8 |     let (taddr, _,_) = Test.@originate main initial_storage 0tez in
    9 |     let contr = Test.to_contract taddr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_bigmap_set.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 467, characters 2-63
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_bigmap_set.mligo", line 9, characters 22-37:
    8 |   let init = (Big_map.empty : storage) in
    9 |   let (taddr, _, _) = Test.@originate main init 0tez in
   10 |   let ctr = Test.to_contract taddr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_module.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 489, characters 2-59
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_module.mligo", line 10, characters 22-37:
    9 | let @test =
   10 |   let (taddr, _, _) = Test.@originate main 0 0tez in
   11 |   let c = Test.to_contract taddr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "interpreter_nested_comparison_test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_equal exited with value ().
    - test_not_equal exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_no_mutation.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 519, characters 2-64
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_no_mutation.mligo", line 25, characters 22-37:
   24 |   let initial_storage = 7 in
   25 |   let (taddr, _, _) = Test.@originate mainf initial_storage 0tez in
   26 |   let contr = Test.to_contract taddr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_mutate_from_file.mligo" ];
  [%expect
    {|
    File "./test_mutate_from_file.mligo", line 7, characters 11-65:
      6 |   let _ = Test.transfer_exn a (Test.eval 1) 0tez in
      7 |   let () = assert (Test.get_storage_of_address a = (Test.eval 1)) in
      8 |   ()

    You are using Michelson failwith primitive (loaded from standard library).
    Consider using `Test.failwith` for throwing a testing framework failure.

    Everything at the top-level was executed.
    - tester exited with value <fun>. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "iteration.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_set exited with value 3.
    - test_list exited with value 3. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "func_michelson.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 564, characters 2-62
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./func_michelson.mligo", line 8, characters 22-37:
    7 | let @test =
    8 |   let (taddr, _, _) = Test.@originate main 1 0tez in
    9 |   let c = Test.to_contract taddr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "func_michelson_loop.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_many_imports.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 592, characters 2-65
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_many_imports.mligo", line 4, characters 22-37:
    3 | let @test =
    4 |   let (taddr, _, _) = Test.@originate C.main () 0tez in
    5 |   let c = Test.to_contract taddr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "switch_case_part_1.jsligo"; "--no-warn" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test1 exited with value ().
    - test2 exited with value ().
    - test3 exited with value ().
    - test4 exited with value ().
    - test5 exited with value ().
    - test6 exited with value ().
    - test7 exited with value ().
    - test8 exited with value ().
    - test9 exited with value ().
    - test10 exited with value ().
    - test11 exited with value ().
    - test12 exited with value ().
    - test13 exited with value ().
    - test14 exited with value ().
    - test15 exited with value ().
    - test16 exited with value ().
    - test17 exited with value ().
    - test18 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "switch_case_part_2.jsligo"; "--no-warn" ];
  [%expect
    {|
      Everything at the top-level was executed.
      - test1 exited with value ().
      - test2 exited with value ().
      - test3 exited with value ().
      - test4 exited with value ().
      - test5 exited with value ().
      - test6 exited with value ().
      - test7 exited with value ().
      - test8 exited with value ().
      - test9 exited with value ().
      - test10 exited with value ().
      - test11 exited with value ().
      - test12 exited with value ().
      - test13 exited with value ().
      - test14 exited with value ().
      - test15 exited with value ().
      - test16 exited with value ().
      - test17 exited with value ().
      - test18 exited with value ().
      - test19 exited with value ().
      - test20 exited with value ().
      - test21 exited with value ().
      - test22 exited with value ().
      - test23 exited with value ().
      - test24 exited with value ().
      - test25 exited with value ().
      - test26 exited with value ().
      - test27 exited with value ().
      - test28 exited with value ().
      - test29 exited with value ().
      - test30 exited with value ().
      - test31 exited with value ().
      - test32 exited with value ().
      - test33 exited with value ().
      - test34 exited with value ().
      - test35 exited with value ().
      - test36 exited with value ().
      - test37 exited with value ().
      - test38 exited with value ().
      - test39 exited with value ().
      - test40 exited with value ().
      - test41 exited with value ().
      - test42 exited with value ().
      - test43 exited with value ().
      - test44 exited with value ().
      - test45 exited with value ().
      - test46 exited with value ().
      - test47 exited with value ().
      - test48 exited with value ().
      - test49 exited with value ().
      - test50 exited with value ().
      - test51 exited with value ().
      - test52 exited with value ().
      - test53 exited with value ().
      - test54 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "switch_case_part_3.jsligo"; "--no-warn" ];
  [%expect
    {|
      Everything at the top-level was executed.
      - test1 exited with value ().
      - test2 exited with value ().
      - test3 exited with value ().
      - test4 exited with value ().
      - test5 exited with value ().
      - test6 exited with value ().
      - test7 exited with value ().
      - test8 exited with value ().
      - test9 exited with value ().
      - test10 exited with value ().
      - test11 exited with value ().
      - test12 exited with value ().
      - test13 exited with value ().
      - test14 exited with value ().
      - test15 exited with value ().
      - test16 exited with value ().
      - test17 exited with value ().
      - test18 exited with value ().
      - test19 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "switch_case_if_else.jsligo"; "--no-warn" ];
  [%expect
    {|
      Everything at the top-level was executed.
      - test_if_switch_break exited with value ().
      - test_if_switch_return exited with value ().
      - test_switch_if_break exited with value ().
      - test_switch_if_return exited with value ().
      - test_switch_switch_break exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "let_rec.mligo" ];
  [%expect
    {|
      Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_negative_big_map_id.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 740, characters 2-72
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_negative_big_map_id.mligo", line 7, characters 22-37:
    6 | let test_main =
    7 |     let (ta, _, _) =  Test.@originate main () 0tez in
    8 |     let c : unit @contract = Test.to_contract ta in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_FA12.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 762, characters 2-57
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_FA12.mligo", line 11, characters 27-42:
   10 |                   total_supply = 300n } in
   11 |   let (typed_addr, _, _) = Test.@originate main storage 0tez in
   12 |   let contr = Test.to_contract typed_addr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "pack_unpack.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_string exited with value ().
    - test_int exited with value ().
    - test_string_int exited with value ().
    - test_string_string exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_pack_unpack.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 794, characters 2-64
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_pack_unpack.mligo", line 6, characters 19-34:
    5 |   let b = Bytes.pack 42n in
    6 |   let (ta, _, _) = Test.@originate main b 0tez in
    7 |   let () = assert ((Bytes.unpack (Test.get_storage ta) : nat option) = Some 42n) in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "pairing_check.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "gas_consum.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 822, characters 2-58
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./gas_consum.mligo", line 14, characters 20-35:
   13 |   in
   14 |   let (ta, _, _) =  Test.@originate main big_list 0tez in
   15 |   let c : parameter @contract = Test.to_contract ta in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_implicit_account.jsligo" ];
  [%expect
    {|
    0mutez
    123mutez
    Everything at the top-level was executed.
    - test_addresses exited with value [tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx]. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_accounts.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_new exited with value 110000000mutez.
    - test_add exited with value 110000000mutez. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_baker_account.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 861, characters 2-66
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_baker_account.mligo", line 15, characters 19-34:
   14 |   let () = Test.set_baker a in
   15 |   let (ta, _, _) = Test.@originate main 41 5tez in
   16 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_register_delegate.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 883, characters 2-70
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_register_delegate.mligo", line 19, characters 19-34:
   18 |   let () = Test.set_baker a in
   19 |   let (ta, _, _) = Test.@originate main 41 5tez in
   20 |

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_global_constant.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 905, characters 2-68
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_global_constant.mligo", line 13, characters 22-37:
   12 | let @test =
   13 |   let (taddr, _, _) = Test.@originate main 1 0tez in
   14 |   let ctr = Test.to_contract taddr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_global_constant_2.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 927, characters 2-70
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_global_constant_2.mligo", line 12, characters 22-37:
   11 | let @test =
   12 |   let (taddr, _, _) = Test.@originate main 1 0tez in
   13 |   let ctr = Test.to_contract taddr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "recursion_uncurry.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 949, characters 2-65
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./recursion_uncurry.mligo", line 7, characters 18-33:
    6 | let @test =
    7 |   let (_, _, n) = Test.@originate main "" 1tez in
    8 |   n

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_timestamp.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_sub exited with value ().
    - test_get_time exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_context.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 979, characters 2-60
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_context.mligo", line 7, characters 19-34:
    6 |   let () = Test.log "test_contract:" in
    7 |   let (ta, _, _) = Test.@originate main 0 0tez in
    8 |   let c = Test.to_contract ta in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_error_balance.jsligo"; "--no-warn" ];
  [%expect
    {|
    100000000000000mutez
    3799997904750mutez
    Everything at the top-level was executed.
    - test exited with value {contract_balance = 3799997904750mutez ; contract_too_low = tz1TDZG4vFoA2xutZMYauUnS4HVucnAGQSpZ ; spend_request = 100000000000000mutez}. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_inline.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 1010, characters 2-59
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_inline.mligo", line 29, characters 13-28:
   28 |
   29 | let test_x = Test.@originate main init_storage 0mutez

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_read_contract.mligo" ];
  [%expect
    {|
    KT1KAUcMCQs7Q4mxLzoUZVH9yCCLETERrDtj
    [1 -> "hi"]
    Everything at the top-level was executed.
    - test_foo exited with value ().
    - test_bar exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "cli_arg.mligo"; "--arg"; "[ 1 ; 2 ; 3]" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_cli_arg exited with value [1 ; 2 ; 3]. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "reset_time.mligo" ];
  [%expect
    {|
  Everything at the top-level was executed.
  - test_x exited with value (timestamp(1970-01-01T00:00:00Z) , timestamp(2012-02-02T10:10:10Z)). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_get_account.mligo" ];
  [%expect
    {|
    (tz1MBWU1WkszFfkEER2pgn4ATKXE9ng7x1sR , edpkusHqa6fxkGPPL9YpgbcakvSTvcTBcwnLAmCdcevmws4Mh2MdHB , "edsk41aRaPPBpidY7w5xu54edk76uJJtJ6myTwYDEWhAwNHce9gKNo")
    3800000000000mutez
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_sign.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_create.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 1069, characters 2-59
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_create.mligo", line 7, characters 24-39:
    6 | let @test =
    7 |   let (fact_ta, _, _) = Test.@originate main_factory ([] : address list) 10tez in
    8 |   let fact_contract = Test.to_contract fact_ta in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_create2.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 1091, characters 2-60
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_create2.mligo", line 7, characters 24-39:
    6 | let @test =
    7 |   let (fact_ta, _, _) = Test.@originate main_factory ([] : address list) 10tez in
    8 |   let fact_contract = Test.to_contract fact_ta in

  Variable "@originate" not found. |}]

(*
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_transfer_entrypoint.ligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]
*)

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_print.mligo" ];
  [%expect
    {|
    Hello world
    @42
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_eprint.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.

    Ooops |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_random.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "get_contract.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 1144, characters 2-60
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./get_contract.mligo", line 8, characters 19-34:
    7 | let @test =
    8 |   let (ta, _, _) = Test.@originate main 0 0tez in
    9 |   let c = Test.to_contract ta in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_key.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 1166, characters 2-56
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_key.mligo", line 14, characters 22-37:
   13 |   let (_, pub_key, _) = Test.get_bootstrap_account 1n in
   14 |   let (taddr, _, _) = Test.@originate main {registry = (Big_map.empty : registry); next_id = 1n} 0mutez in
   15 |   let contr = Test.to_contract taddr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_tickets_and_bigmaps.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 1188, characters 2-72
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_tickets_and_bigmaps.mligo", line 49, characters 22-37:
   48 |
   49 |   let (taddr, _, _) = Test.@originate main init_storage 0mutez in
   50 |   let contr = Test.to_contract taddr in

  Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_chain_id.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_print_values.mligo" ];
  [%expect {| aloh |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_to_json.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 1220, characters 2-60
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "./test_to_json.mligo", line 8, characters 19-34:
    7 | let test_to_json =
    8 |   let (ta, _, _) = Test.@originate main ({ foo = 42 ; bar = ["hello"; "world"] } : storage) 0tez in
    9 |   let () = Test.println (Test.to_json ta) in

  Variable "@originate" not found. |}]

(*
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_imm.ligo" ];
  [%expect
    {test|
    Everything at the top-level was executed.
    - test_orig exited with value (). |test}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_record.ligo" ];
  [%expect
    {test|
    0
    Everything at the top-level was executed.
    - test_reproducing exited with value "OK". |test}]
*)

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "tuple_long.mligo" ];
  [%expect
    {test|
    Everything at the top-level was executed. |test}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_compare_setmap.mligo" ];
  [%expect
    {test|
    Everything at the top-level was executed.
    - test_address_set exited with value { "tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" ;
      "tz1TDZG4vFoA2xutZMYauUnS4HVucnAGQSpZ" }.
    - test_int_set exited with value { 3 ; 4 }.
    - test_map exited with value { Elt "tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" 900 ;
      Elt "KT1WoTZUkky48v3QqZWzkeJCYfhWhNaVFYuC" 100 }.
    - test_big_map exited with value { Elt "tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" 900 ;
      Elt "KT1WoTZUkky48v3QqZWzkeJCYfhWhNaVFYuC" 100 }. |test}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "test-expr"
    ; "cameligo"
    ; "type t = [@layout:comb] { num : int ; num_nat : nat ; str : string } in let v = \
       Test.parse_michelson {| { Elt 1 (Pair 1 1 \"q\") } |} in ((Test.decompile v : \
       (nat, t) big_map))"
    ];
  [%expect
    {test|
    Everything at the top-level was executed.
    - eval exited with value [1n -> {num = 1 ; num_nat = 1n ; str = "q"}]. |test}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "test"; test "display_format_json.mligo"; "--display-format"; "json" ];
  [%expect
    {xxx|
    [
      [ "test_x", [ "constant", [ "int", "65" ] ] ],
      [ "test_y", [ "constant", [ "string", "hello" ] ] ]
    ] |xxx}]

(* do not remove that :) *)
let () = Caml.Sys.chdir pwd

let () =
  Caml.Sys.chdir
    "../../test/contracts/interpreter_tests/originate_from_relative_path/test/a/b/"


let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test.mligo" ];
  [%expect.unreachable];
  run_ligo_good [ "run"; "test"; test "test.jsligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 1310, characters 2-52
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  An internal error ocurred. Please, contact the developers.
  "Assert_failure src/passes/02-parsing/cameligo/Parser.mly:1036:39". |}]

let () = Caml.Sys.chdir pwd

let () =
  Caml.Sys.chdir "../../test/contracts/interpreter_tests/originate_from_relative_path/"


let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test/a/b/test.mligo" ];
  [%expect.unreachable];
  run_ligo_good [ "run"; "test"; test "test/a/b/test.jsligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Ligo_interpreter_tests.(fun) in file "src/bin/expect_tests/ligo_interpreter_tests.ml", line 1336, characters 2-61
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  An internal error ocurred. Please, contact the developers.
  "Assert_failure src/passes/02-parsing/cameligo/Parser.mly:1036:39". |}]

let () = Caml.Sys.chdir pwd
let bad_test n = bad_test ("/interpreter_tests/" ^ n)

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_capture_meta_type.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_capture_meta_type.mligo", line 6, characters 2-17:
      5 | let ta, _, _ =
      6 |   Test.@originate main () 0tez
      7 |

    Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_random.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_random.mligo", line 6, characters 47-60:
      5 |   (* We generate the property *)
      6 |   let @test = PBT.make_test (PBT.gen_small : ((int @contract) list) pbt_gen) (fun (xs : (int @contract) list) -> List.length xs = 42n) in
      7 |   (* And run it *)

    Generator for type @contract (int) is not implemented. For now, only unit, string, bytes, address, int, nat, tez, records, sums, lists, sets, maps and big_maps can be generated. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_failure1.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_failure1.mligo", line 2, characters 2-25:
      1 | let @test : unit =
      2 |   failwith "I am failing"

    You are using Michelson failwith primitive (loaded from standard library).
    Consider using `Test.failwith` for throwing a testing framework failure.

    File "../../test/contracts/negative//interpreter_tests/test_failure1.mligo", line 2, characters 2-25:
      1 | let @test : unit =
      2 |   failwith "I am failing"

    An uncaught error occured:
    Failwith: "I am failing" |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_failure2.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_failure2.mligo", line 2, characters 4-16:
      1 | let @test =
      2 |     assert false

    You are using Michelson failwith primitive (loaded from standard library).
    Consider using `Test.failwith` for throwing a testing framework failure.

    File "../../test/contracts/negative//interpreter_tests/test_failure2.mligo", line 2, characters 4-16:
      1 | let @test =
      2 |     assert false

    An uncaught error occured:
    Failwith: "failed assertion"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_failure2.mligo", line 2, characters 4-16 |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "bad_balances_reset.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/bad_balances_reset.mligo", line 1, characters 12-49:
      1 | let @test = Test.reset_state 2n [4000tez;4000tez]

     baker account initial balance must at least reach 6000 tez |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_failure3.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_failure3.mligo", line 3, characters 2-17:
      2 |   let f = (fun (_ : (unit * unit)) -> ()) in
      3 |   Test.@originate f () 0tez

    Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_trace.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 3, characters 4-31:
      2 |   if x < 0 then
      3 |     (failwith "negative" : int)
      4 |   else

    You are using Michelson failwith primitive (loaded from standard library).
    Consider using `Test.failwith` for throwing a testing framework failure.

    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 3, characters 4-31:
      2 |   if x < 0 then
      3 |     (failwith "negative" : int)
      4 |   else

    An uncaught error occured:
    Failwith: "negative"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 9, characters 14-49 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 9, characters 14-49 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 9, characters 14-49 |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_trace2.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 11, characters 19-34:
     10 | let @test =
     11 |   let (ta, _, _) = Test.@originate main () 1tez in
     12 |   make_call (Test.to_contract ta)

    Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_mutation_loop.mligo"; "--steps"; "1000" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_mutation_loop.mligo", line 18, characters 36-83:
     17 |     | Some (_, mutation) -> let () = Test.log(mutation) in
     18 |                                     failwith "Some mutation also passes the tests!"

    You are using Michelson failwith primitive (loaded from standard library).
    Consider using `Test.failwith` for throwing a testing framework failure.

    File "../../test/contracts/negative//interpreter_tests/test_mutation_loop.mligo", line 18, characters 36-83:
     17 |     | Some (_, mutation) -> let () = Test.log(mutation) in
     18 |                                     failwith "Some mutation also passes the tests!"

    An uncaught error occured:
    Failwith: "Some mutation also passes the tests!"
    Mutation at: File "../../test/contracts/negative//interpreter_tests/test_mutation_loop.mligo", line 3, characters 29-30:
      2 |     if rounds > 0 then
      3 |         my_rec_fun (rounds - 1)
      4 |     else

    Replacing by: 2. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_source1.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_source1.mligo", line 5, characters 22-37:
      4 | let @test =
      5 |   let (taddr, _, _) = Test.@originate main () 0tez in
      6 |   let contr = Test.to_contract taddr in

    Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_source2.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_source2.mligo", line 5, characters 22-37:
      4 | let @test =
      5 |   let (taddr, _, _) = Test.@originate main () 0tez in
      6 |   let contr = Test.to_contract taddr in

    Variable "@originate" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_run_types.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_run_types.jsligo", line 2, characters 26-44:
      1 | const foo = (x: {field: int}): {field: int} => {return x};
      2 | const bar = Test.run(foo, {property: "toto"});
      3 |

    Invalid type(s)
    Cannot unify "record[property -> string]" with "record[field -> int]". |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_run_types2.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_run_types2.jsligo", line 2, characters 26-32:
      1 | const foo = (x:  {b:int}):  {b:int} => {return x};
      2 | const bar = Test.run(foo, "toto");

    Invalid type(s)
    Cannot unify "string" with "record[b -> int]". |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_run_types3.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_run_types3.jsligo", line 2, characters 26-41:
      1 | const foo = (x: int): int => {return x};
      2 | const bar = Test.run(foo, {field: "toto"});

    Invalid type(s)
    Cannot unify "record[field -> string]" with "int". |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_decompile.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_decompile.mligo", line 3, characters 2-29:
      2 |   let x = Test.eval 4n in
      3 |   (Test.decompile x : string)

    This Michelson value has assigned type 'nat', which does not coincide with expected type 'string'. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_register_delegate.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 19, characters 19-34:
     18 |   let () = Test.set_baker a in
     19 |   let (ta, _, _) = Test.@originate main 41 5tez in
     20 |

    Variable "@originate" not found. |}]

let pwd = Caml.Sys.getcwd ()
let () = Caml.Sys.chdir "../../test/contracts/negative/interpreter_tests/"

(* using typed_address in Bytes.pack *)
let%expect_test _ =
  run_ligo_bad [ "run"; "test"; "typed_addr_in_bytes_pack.mligo" ];
  [%expect
    {|
  File "typed_addr_in_bytes_pack.mligo", line 14, character 17 to line 18, character 5:
   13 |     let r = originate_record () in
   14 |     let packed = Bytes.pack (fun() ->
   15 |         match (Tezos.get_entrypoint_opt "%transfer" r.addr : unit @contract option) with
   16 |           Some(c) -> let op = Tezos.transaction () 0mutez c in [op]
   17 |         | None ->  ([] : operation list)
   18 |     ) in
   19 |     let () = Test.log(packed) in

  Cannot decompile value KT1KAUcMCQs7Q4mxLzoUZVH9yCCLETERrDtj of type typed_address (unit ,
  unit) |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_michelson_non_func.mligo" ];
  [%expect
    {test|
    File "../../test/contracts/negative//interpreter_tests/test_michelson_non_func.mligo", line 2, characters 16-55:
      1 | let @test =
      2 |   let x : int = [%Michelson ({|{ PUSH int 1 }|} : int)] in
      3 |   begin

    Embedded raw code can only have a functional type |test}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "get_contract.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/get_contract.mligo", line 8, characters 19-34:
      7 | let @test =
      8 |   let (ta, _, _) = Test.@originate main 0 0tez in
      9 |   let c = Test.to_contract ta in

    Variable "@originate" not found. |}]
