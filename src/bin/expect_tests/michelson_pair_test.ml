open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print"; "ast-typed"; contract "michelson_annotation_no_shadow_pair.ligo"] ;
  [%expect {|
  type type_decl1_pair = record[michelson_one -> int%one , michelson_two -> nat%two]
  type type_decl2_pair = record[michelson_four -> nat%four , michelson_three -> int%three] |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_pair_tree.ligo" ] ;
  [%expect {|
    { parameter unit ;
      storage (pair (string %three) (pair %four (int %one) (nat %two))) ;
      code { DROP ;
             PUSH string "foo" ;
             PUSH int 1 ;
             PUSH nat 2 ;
             SWAP ;
             PAIR ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_pair_tree.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage (pair (int %three) (pair %four (int %one) (nat %two))) ;
      code { DROP ;
             PUSH int 3 ;
             PUSH int 1 ;
             PUSH nat 2 ;
             SWAP ;
             PAIR ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_pair_tree.religo" ] ;
  [%expect {|
    { parameter unit ;
      storage (pair (int %three) (pair %four (int %one) (nat %two))) ;
      code { DROP ;
             PUSH int 3 ;
             PUSH int 1 ;
             PUSH nat 2 ;
             SWAP ;
             PAIR ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_pair_tree.jsligo" ] ;
  [%expect {|
    File "../../test/contracts/michelson_pair_tree.jsligo", line 8, characters 21-26:
      7 |
      8 | let main = ([action, store] : [unit, storage]) : return_ => {
      9 |   let foo = { michelson_three : 3 , michelson_four : { michelson_one : 1, michelson_two : 2 as nat} } ;
    :
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    File "../../test/contracts/michelson_pair_tree.jsligo", line 8, characters 13-19:
      7 |
      8 | let main = ([action, store] : [unit, storage]) : return_ => {
      9 |   let foo = { michelson_three : 3 , michelson_four : { michelson_one : 1, michelson_two : 2 as nat} } ;
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    { parameter unit ;
      storage (pair (int %three) (pair %four (int %one) (nat %two))) ;
      code { DROP ;
             PUSH int 3 ;
             PUSH int 1 ;
             PUSH nat 2 ;
             SWAP ;
             PAIR ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_pair_tree_intermediary.ligo" ] ;
  [%expect {|
    { parameter unit ;
      storage (pair (string %three) (pair (int %one) (nat %two))) ;
      code { DROP ;
             PUSH string "foo" ;
             PUSH int 1 ;
             PUSH nat 2 ;
             SWAP ;
             PAIR ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]
