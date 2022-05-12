open Cli_expect

let test basename = "./" ^ basename
let pwd = Sys.getcwd ()
let () = Sys.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "interpret_test.mligo" ] ;
  [%expect {|
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
    - test_add_mutez exited with value ().
    - test_sub_mutez exited with value ().
    - test_div_mutez exited with value ().
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
    - test_int_bls exited with value (). |}]

let%expect_test _ =
  (* This tests a possible regression on the way modules are evaluated. It is possible that the number of element in the environment explodes. *)
  run_ligo_good ["run"; "test" ; test "imported_modules/test.mligo" ; "--format" ; "dev" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test1 exited with value (). |}]

let%expect_test _ =
  run_ligo_good ["run"; "test" ; test "views_test.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good ["run"; "test" ; test "test_timelock.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good ["run"; "test" ; test "interpret_test_log.mligo" ] ;
  [%expect {|
    {a = 1 ; b = 2n ; c = "aaa"}
    One (())
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good ["run";"test" ; test "test_fail.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value "my contract always fail". |}]

let%expect_test _ =
  run_ligo_good ["run";"test" ; test "test_fail_from_file.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value "my contract always fail". |}]


let%expect_test _ =
  run_ligo_good ["run";"test" ; test "compile_expr.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test1 exited with value ().
  - test2 exited with value ().
  - test3 exited with value ().
  - test4 exited with value (). |}]

let%expect_test _ =
  run_ligo_good ["run";"test" ; test "compile_expr_from_file.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test1 exited with value ().
  - test2 exited with value ().
  - test3 exited with value ().
  - test4 exited with value (). |}]

let%expect_test _ =
  run_ligo_good ["run";"test" ; test "test_example.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value 111.
    - test2 exited with value (). |}]

let%expect_test _ =
  run_ligo_good ["run";"test" ; test "test_example.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value 111.
    - test2 exited with value (). |}]

let%expect_test _ =
  run_ligo_good ["run";"test" ; test "catch_balance_too_low.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good ["run";"test" ; test "test_subst_with_storage.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good ["run";"test" ; test "test_subst_with_storage_from_file.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good ["run";"test" ; test "nesting_modules.mligo" ] ;
  [%expect{|
    111
    Everything at the top-level was executed.
    - test exited with value (). |}]

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
  run_ligo_good ["run";"test" ; test "override_function.mligo" ] ;
  [%expect {|
    4
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good ["run";"test" ; test "test_fresh.mligo" ] ;
  [%expect{| Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test "test_rec_contract.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test "test_importer.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test "test_bigmap.mligo" ] ;
  [%expect {|
    [32 -> 42n]
    None (())
    [32 -> 42n]
    [3 -> 42n ; 21 -> 42n ; 32 -> 42n]
    None (())
    Some (42n)
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test "test_bigmap_compare.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test "test_bigmap_set.mligo" ] ;
  [%expect {|
    9n
    0n
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test "test_module.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value 1. |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test "interpreter_nested_comparison_test.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value ().
    - test_equal exited with value ().
    - test_not_equal exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test "test_no_mutation.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value ().
    - test_mutation exited with value ().
    - test_mutation_all exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test "iteration.jsligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_set exited with value 3.
    - test_list exited with value 3. |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test "func_michelson.mligo" ] ;
  [%expect {|
    42
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test "func_michelson_loop.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test "test_many_imports.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test "switch_case_part_1.jsligo" ] ;
  [%expect{|
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
  run_ligo_good [ "run" ; "test" ; test "switch_case_part_2.jsligo" ] ;
    [%expect{|
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
  run_ligo_good [ "run" ; "test" ; test "switch_case_part_3.jsligo" ] ;
    [%expect{|
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
  run_ligo_good [ "run" ; "test" ; test "switch_case_if_else.jsligo" ] ;
    [%expect{|
      Everything at the top-level was executed.
      - test_if_switch_break exited with value ().
      - test_if_switch_return exited with value ().
      - test_switch_if_break exited with value ().
      - test_switch_if_return exited with value ().
      - test_switch_switch_break exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test "test_negative_big_map_id.mligo" ] ;
    [%expect{|
      Everything at the top-level was executed.
      - test_main exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "test_FA12.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test_transfer exited with value ().
    - test_transfer_not_e_allowance exited with value ().
    - test_transfer_not_e_balance exited with value ().
    - test_approve exited with value ().
    - test_approve_unsafe exited with value ().
    - test_get_allowance exited with value ().
    - test_get_balance exited with value ().
    - test_get_total_supply exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "pack_unpack.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test_string exited with value ().
    - test_int exited with value ().
    - test_string_int exited with value ().
    - test_string_string exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "test_pack_unpack.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "pairing_check.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "gas_consum.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (1802n , 1985n , 1985n). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "test_implicit_account.jsligo" ] ;
  [%expect {|
    0mutez
    123mutez
    Everything at the top-level was executed.
    - test_addresses exited with value [tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx]. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "test_accounts.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test_new exited with value 110000000mutez.
    - test_add exited with value 110000000mutez. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "test_baker_account.mligo" ] ;
  [%expect {|
    "STARTING BALANCE AND VOTING POWER"
    3800000000000mutez
    4000000000000n
    "BALANCE AND VOTING POWER AFTER ORIGINATE"
    3800011000000mutez
    4000000000000n
    "BALANCE AND VOTING POWER AFTER TRANSFER"
    3800022000000mutez
    4000000000000n
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "test_register_delegate.mligo" ] ;
  [%expect {|
    "STARTING BALANCE AND VOTING POWER"
    950000000000mutez
    0n
    "BALANCE AND VOTING POWER AFTER ORIGINATE"
    950011000000mutez
    0n
    "BALANCE AND VOTING POWER AFTER TRANSFER"
    950022000000mutez
    0n
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "test_global_constant.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "test_global_constant_2.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "recursion_uncurry.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value 116. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "test_timestamp.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test_sub exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "test_context.mligo" ] ;
  [%expect {|
    "test_contract:"
    0
    10
    5
    0
    0
    "test_move:"
    3800000000000mutez
    3800100000000mutez
    3800000000000mutez
    Everything at the top-level was executed.
    - test_contract exited with value ().
    - test_move exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; test "test_error_balance.jsligo" ] ;
  [%expect {|
    100000000000000mutez
    3799997904750mutez
    Everything at the top-level was executed.
    - test exited with value {contract_balance = 3799997904750mutez ; contract_too_low = tz1TDZG4vFoA2xutZMYauUnS4HVucnAGQSpZ ; spend_request = 100000000000000mutez}. |}]

(* do not remove that :) *)
let () = Sys.chdir pwd

let bad_test n = bad_test ("/interpreter_tests/"^n)

let%expect_test _ =
  run_ligo_bad ["run";"test" ; bad_test "test_failure1.mligo" ] ;
  [%expect {|
    Test failed with "I am failing"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_failure1.mligo", line 2, characters 2-25:
      1 | let test =
      2 |   failwith "I am failing" |}]

let%expect_test _ =
  run_ligo_bad ["run";"test" ; bad_test "test_failure2.mligo" ] ;
  [%expect {|
    failed assertion
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_failure2.mligo", line 2, characters 4-16:
      1 | let test =
      2 |     assert false |}]

let%expect_test _ =
  run_ligo_bad ["run"; "test" ; bad_test "bad_balances_reset.mligo" ] ;
  [%expect {|
    baker account initial balance must at least reach 6000 tez |}]

let%expect_test _ =
  run_ligo_bad ["run";"test" ; bad_test "test_failure3.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative//interpreter_tests/test_failure3.mligo", line 3, characters 2-16:
      2 |   let f = (fun (_ : (unit * unit)) -> ()) in
      3 |   Test.originate f () 0tez

    Invalid type(s).
    Expected: "( list (operation) * s )", but got: "unit". |}]

let%expect_test _ =
  run_ligo_bad ["run";"test" ; bad_test "test_trace.mligo" ] ;
  [%expect {|
    Test failed with "negative"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 3, characters 4-31:
      2 |   if x < 0 then
      3 |     (failwith "negative" : int)
      4 |   else

    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 9, characters 39-46 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 9, characters 14-49 |}]

let%expect_test _ =
  run_ligo_bad ["run";"test" ; bad_test "test_trace2.mligo" ] ;
  [%expect {|
    An uncaught error occured:
    Did not find service: GET ocaml:context/contracts/tz1fakefakefakefakefakefakefakcphLA5/storage
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 6, characters 10-88:
      5 | let make_call (contr : unit contract) =
      6 |   let _ = Test.get_storage_of_address ("tz1fakefakefakefakefakefakefakcphLA5" : address) in
      7 |   Test.transfer_to_contract_exn contr () 10tez

    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 12, characters 2-33 |}]

let%expect_test _ =
  run_ligo_bad [ "run" ; "test" ; bad_test "test_mutation_loop.mligo" ; "--steps" ; "1000" ] ;
  [%expect {|
    Mutation at: File "../../test/contracts/negative//interpreter_tests/test_mutation_loop.mligo", line 3, characters 29-30:
      2 |     if rounds > 0 then
      3 |         my_rec_fun (rounds - 1)
      4 |     else

    Replacing by: 2.

    Test failed with "Some mutation also passes the tests!"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_mutation_loop.mligo", line 17, character 28 to line 18, character 83:
     16 |     | None -> ()
     17 |     | Some (_, mutation) -> let () = Test.log(mutation) in
     18 |                                     failwith "Some mutation also passes the tests!" |}]

let%expect_test _ =
  run_ligo_bad [ "run" ; "test" ; bad_test "test_source1.mligo" ] ;
  [%expect {|
    The source address is not an implicit account
    KT1CJbrhkpX9eeh88JvkC58rSXZvRxGq3RiV |}]

let%expect_test _ =
  run_ligo_bad [ "run" ; "test" ; bad_test "test_source2.mligo" ] ;
  [%expect {|
    The source address is not an implicit account
    KT1CJbrhkpX9eeh88JvkC58rSXZvRxGq3RiV |}]

let%expect_test _ =
  run_ligo_bad [ "run" ; "test" ; bad_test "test_run_types.jsligo" ] ;
  [%expect {|
    File "../../test/contracts/negative//interpreter_tests/test_run_types.jsligo", line 2, characters 12-20:
      1 | const foo = (x: {field: int}): {field: int} => {return x};
      2 | const bar = Test.run(foo, {property: "toto"});
      3 |

    These types are not matching:
     - record[property -> string]
     - record[field -> int] |}]

let%expect_test _ =
  run_ligo_bad [ "run" ; "test" ; bad_test "test_run_types2.jsligo" ] ;
  [%expect {|
    File "../../test/contracts/negative//interpreter_tests/test_run_types2.jsligo", line 2, characters 12-20:
      1 | const foo = (x:  {b:int}):  {b:int} => {return x};
      2 | const bar = Test.run(foo, "toto");

    These types are not matching:
     - string
     - record[b -> int] |}]

let%expect_test _ =
  run_ligo_bad [ "run" ; "test" ; bad_test "test_run_types3.jsligo" ] ;
  [%expect {|
    File "../../test/contracts/negative//interpreter_tests/test_run_types3.jsligo", line 2, characters 12-20:
      1 | const foo = (x: int): int => {return x};
      2 | const bar = Test.run(foo, {field: "toto"});

    These types are not matching:
     - record[field -> string]
     - int |}]

let%expect_test _ =
  run_ligo_bad [ "run" ; "test" ; bad_test "test_decompile.mligo" ] ;
  [%expect {|
    This Michelson value has assigned type 'nat', which does not coincide with expected type 'string'. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test" ; bad_test "test_register_delegate.mligo" ] ;
  [%expect {|
    Baker cannot bake. Enough rolls? Enough cycles passed?
    "STARTING BALANCE AND VOTING POWER"
    95000000000mutez
    100000000000n |}]


let%expect_test _ =
  run_ligo_bad [ "run"; "test" ; bad_test "test_random.mligo" ] ;
  [%expect {|
    failed assertion
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_random.mligo", line 17, characters 18-30:
     16 |       | None -> ()
     17 |       | Some x -> assert false
     18 | end

    File "../../test/contracts/negative//interpreter_tests/test_random.mligo", line 29, characters 2-25 |}]

let pwd = Sys.getcwd ()
let () = Sys.chdir "../../test/contracts/negative/interpreter_tests/"

(* using typed_address in Bytes.pack *)
let%expect_test _ =
run_ligo_bad [ "run" ; "test" ; "typed_addr_in_bytes_pack.mligo" ] ;
[%expect{|
  File "typed_addr_in_bytes_pack.mligo", line 13, characters 8-9:
   12 | let test =
   13 |     let r = originate_record () in
   14 |     let packed = Bytes.pack (fun() ->

  Expected address but got typed_address (unit ,
  unit) |}]

let () = Sys.chdir pwd

let%expect_test _ =
  run_ligo_bad [ "run"; "test" ; bad_test "test_michelson_non_func.mligo" ] ;
  [%expect {xxx|
    File "../../test/contracts/negative//interpreter_tests/test_michelson_non_func.mligo", line 2, characters 16-55:
      1 | let test =
      2 |   let x : int = [%Michelson ({|{ PUSH int 1 }|} : int)] in
      3 |   begin

    Embedded raw code can only have a functional type |xxx}]

let pwd = Sys.getcwd ()
let () = Sys.chdir "../../test/projects/"

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "originate_contract/test.mligo" ; "--project-root" ; "originate_contract" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value KT1BxaPaFE2YDn8Toh2u2SJ18P6zf24oqbzZ(None). |}]

let () = Sys.chdir pwd
