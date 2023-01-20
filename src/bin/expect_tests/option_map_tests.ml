open Cli_expect

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "option_map.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (option int) ;
      code { CDR ;
             MAP { PUSH string "foo" ; PAIR } ;
             LAMBDA (pair string int) int { CDR } ;
             SWAP ;
             MAP { DUP 2 ; SWAP ; EXEC } ;
             SWAP ;
             DROP ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "option_map.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "option_map.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (option int) ;
      code { CDR ;
             MAP { PUSH string "foo" ; PAIR } ;
             LAMBDA (pair string int) int { CDR } ;
             SWAP ;
             MAP { DUP 2 ; SWAP ; EXEC } ;
             SWAP ;
             DROP ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "option_map.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

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
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]
