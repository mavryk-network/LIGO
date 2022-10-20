open Cli_expect

let contract name = "../../test/contracts/entrypoint_gen/" ^ name

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "FA1.2.mligo" ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 39, characters 7-29
  Called from Cli_expect_tests__Entrypoint_gen.(fun) in file "src/bin/expect_tests/entrypoint_gen.ml", line 6, characters 2-67
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  Internal error: Entrypoint main does not exist |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "FA1.2.mligo" ; "-e" ; "transfer" ] ;
  [%expect{|
    File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 87, characters 4-11:
     86 | [@entry]
     87 | let approve (param, storage : approve * storage) : result =
     88 |   let allowances = storage.allowances in

    Warning: This entry will be ignored, command line option override [@entry] annotation

    File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 103, characters 4-16:
    102 | [@entry]
    103 | let getAllowance (param, storage : getAllowance * storage) : operation list * storage =
    104 |   let value =

    Warning: This entry will be ignored, command line option override [@entry] annotation

    File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 111, characters 4-14:
    110 | [@entry]
    111 | let getBalance (param, storage : getBalance * storage) : operation list * storage =
    112 |   let value =

    Warning: This entry will be ignored, command line option override [@entry] annotation

    File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 119, characters 4-18:
    118 | [@entry]
    119 | let getTotalSupply (param, storage : getTotalSupply * storage) : operation list * storage =
    120 |   let total = storage.total_supply in

    Warning: This entry will be ignored, command line option override [@entry] annotation

    { parameter (pair (address %from) (address %to) (nat %value)) ;
      storage
        (pair (pair (big_map %allowances (pair (address %owner) (address %spender)) nat)
                    (big_map %tokens address nat))
              (nat %total_supply)) ;
      code { UNPAIR ;
             DUP 2 ;
             CAR ;
             CAR ;
             DUP 3 ;
             CAR ;
             CDR ;
             DUP 3 ;
             CAR ;
             SENDER ;
             COMPARE ;
             EQ ;
             IF { SWAP }
                { SENDER ;
                  DUP 4 ;
                  CAR ;
                  PAIR ;
                  DUP 4 ;
                  GET 4 ;
                  DUP 4 ;
                  DUP 3 ;
                  GET ;
                  IF_NONE { PUSH nat 0 } {} ;
                  SUB ;
                  ISNAT ;
                  IF_NONE { PUSH string "NotEnoughAllowance" ; FAILWITH } {} ;
                  DIG 3 ;
                  PUSH nat 0 ;
                  DUP 3 ;
                  COMPARE ;
                  EQ ;
                  IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                  DIG 2 ;
                  UPDATE } ;
             DUP 3 ;
             GET 4 ;
             DUP 3 ;
             DUP 5 ;
             CAR ;
             GET ;
             IF_NONE { PUSH nat 0 } {} ;
             SUB ;
             ISNAT ;
             IF_NONE { PUSH string "NotEnoughBalance" ; FAILWITH } {} ;
             DIG 2 ;
             PUSH nat 0 ;
             DUP 3 ;
             COMPARE ;
             EQ ;
             IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
             DUP 4 ;
             CAR ;
             UPDATE ;
             DUP 3 ;
             GET 4 ;
             DUP 2 ;
             DUP 5 ;
             GET 3 ;
             GET ;
             IF_NONE { PUSH nat 0 } {} ;
             ADD ;
             DUP 5 ;
             CDR ;
             DIG 2 ;
             PUSH nat 0 ;
             DUP 4 ;
             COMPARE ;
             EQ ;
             IF { DIG 2 ; DROP ; NONE nat } { DIG 2 ; SOME } ;
             DIG 4 ;
             GET 3 ;
             UPDATE ;
             DIG 3 ;
             DROP ;
             DIG 2 ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "FA1.2.mligo" ; "-e" ; "transfer,approve" ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 39, characters 7-29
  Called from Cli_expect_tests__Entrypoint_gen.(fun) in file "src/bin/expect_tests/entrypoint_gen.ml", line 139, characters 2-95
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 103, characters 4-16:
  102 | [@entry]
  103 | let getAllowance (param, storage : getAllowance * storage) : operation list * storage =
  104 |   let value =

  Warning: This entry will be ignored, command line option override [@entry] annotation

  File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 111, characters 4-14:
  110 | [@entry]
  111 | let getBalance (param, storage : getBalance * storage) : operation list * storage =
  112 |   let value =

  Warning: This entry will be ignored, command line option override [@entry] annotation

  File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 119, characters 4-18:
  118 | [@entry]
  119 | let getTotalSupply (param, storage : getTotalSupply * storage) : operation list * storage =
  120 |   let total = storage.total_supply in

  Warning: This entry will be ignored, command line option override [@entry] annotation

  Internal error: Entrypoint main does not exist |}]

(* entrypoint doesn't exist *)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "FA1.2.mligo" ; "-e" ; "transfert" ] ;
  [%expect {|
    File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 50, characters 4-12:
     49 | [@entry]
     50 | let transfer (param, storage : transfer * storage) : result =
     51 |   let allowances = storage.allowances in

    Warning: This entry will be ignored, command line option override [@entry] annotation

    File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 87, characters 4-11:
     86 | [@entry]
     87 | let approve (param, storage : approve * storage) : result =
     88 |   let allowances = storage.allowances in

    Warning: This entry will be ignored, command line option override [@entry] annotation

    File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 103, characters 4-16:
    102 | [@entry]
    103 | let getAllowance (param, storage : getAllowance * storage) : operation list * storage =
    104 |   let value =

    Warning: This entry will be ignored, command line option override [@entry] annotation

    File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 111, characters 4-14:
    110 | [@entry]
    111 | let getBalance (param, storage : getBalance * storage) : operation list * storage =
    112 |   let value =

    Warning: This entry will be ignored, command line option override [@entry] annotation

    File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 119, characters 4-18:
    118 | [@entry]
    119 | let getTotalSupply (param, storage : getTotalSupply * storage) : operation list * storage =
    120 |   let total = storage.total_supply in

    Warning: This entry will be ignored, command line option override [@entry] annotation

    Internal error: Entrypoint transfert does not exist |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "FA1.2.mligo" ; "-e" ; "transfer,bad_transfer" ] ;
  [%expect {|
    File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 87, characters 4-11:
     86 | [@entry]
     87 | let approve (param, storage : approve * storage) : result =
     88 |   let allowances = storage.allowances in

    Warning: This entry will be ignored, command line option override [@entry] annotation

    File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 103, characters 4-16:
    102 | [@entry]
    103 | let getAllowance (param, storage : getAllowance * storage) : operation list * storage =
    104 |   let value =

    Warning: This entry will be ignored, command line option override [@entry] annotation

    File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 111, characters 4-14:
    110 | [@entry]
    111 | let getBalance (param, storage : getBalance * storage) : operation list * storage =
    112 |   let value =

    Warning: This entry will be ignored, command line option override [@entry] annotation

    File "../../test/contracts/entrypoint_gen/FA1.2.mligo", line 119, characters 4-18:
    118 | [@entry]
    119 | let getTotalSupply (param, storage : getTotalSupply * storage) : operation list * storage =
    120 |   let total = storage.total_supply in

    Warning: This entry will be ignored, command line option override [@entry] annotation


    Invalid entrypoint argument.
    Entrypoint 'bad_transfer' has storage type 'unit'.
    This is inconsitent with the first declared entrypoint 'transfer' with storage type '
    record[allowances -> big_map (record[owner -> address , spender -> address] , nat) , tokens -> big_map (address , nat) , total_supply -> nat]'. |}]
