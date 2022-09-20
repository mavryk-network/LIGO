open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
let a#491 : int = 42 in let b#492 : int = 1 in let x : int = a#491 in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let a#491 : int = 40 in
    let b#494 : int =
      let ba#492 : int = 1 in let baa#493 : int = ba#492 in ADD(ba#492 , baa#493) in
    let x : int = ADD(a#491 , b#494) in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let a#491 : int = 1 in
    let as#492 : int = 42 in
    let x#493 : int = a#491 in
    let b#494 : int = as#492 in let x : int = as#492 in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let as#491 : int = 20 in
  let s_as#492 : int = 22 in let x : int = ADD(as#491 , s_as#492) in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect{|
    let a#491 : int = 1 in
    let as#492 : int = 42 in let as#493 : int = 3 in let x : int = as#492 in unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let x#491 : int = 1 in
  let foo : int =
    let x : int = 20 in
    let x#492 : int = x in
    let y#493 : int = x#491 in
    let z#494 : int = y#493 in ADD(ADD(ADD(x#492 , y#493) , x) , z#494) in
  let x : int = foo in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let v#491 : int = 40 in
  let v#492 : int = ADD(v#491 , 1) in
  let v#493 : int = ADD(v#492 , 1) in let x : int = v#493 in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let x#491 : int = 41 in
  let x : int = 1 in
  let x#492 : int = x in
  let y#493 : int = x#491 in
  let u : int = ADD(x#492 , y#493) in let x : int = u in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let x#491 : int = 41 in
  let x#492 : int = ADD(x#491 , 1) in let x : int = x#492 in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let x#491 : int = 42 in
  let x#492 : int = 2 in let y#493 : int = x#491 in let x : int = y#493 in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let x#491 : int = 19 in
  let y#492 : int = 22 in
  let x : int =
    let x : int = 1 in
    let u : int = x#491 in let v : int = y#492 in ADD(ADD(u , v) , x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let a#491 : int = 42 in
  let x#492 : int = a#491 in let x : int = x#492 in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let current_turn#493 : nat -> nat =
    lambda (i : nat) : nat return ADD(i , +1) in
  let other#494 : nat -> unit =
    lambda (n : nat) : unit return let current_turn : nat =
                                     (current_turn#493)@(+1) in
                                   (assert)@(EQ(n , current_turn)) in
  let main : ( unit * unit ) -> ( list (operation) * unit ) =
    lambda (gen#2 : ( unit * unit )) : ( list (operation) * unit ) return
   match gen#2 with
    | ( _p : unit , _s : unit ) ->
    ( LIST_EMPTY() , (other#494)@(+2) ) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "bug_alias13.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH nat 1 ;
             PUSH nat 1 ;
             ADD ;
             PUSH nat 2 ;
             COMPARE ;
             EQ ;
             IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "effects.mligo" ] ;
  [%expect{|
    { parameter int ;
      storage int ;
      code { CDR ; PUSH string "foo" ; FAILWITH } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "pascaligo" ; "tm" ; "--init-file" ; contract "bug_module_record.ligo" ] ;
  [%expect{|
    (Pair 1 "b") |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "test" ; "--init-file" ; contract "bug_locally_bound_vars.mligo" ] ;
  [%expect{|
           42 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "test2" ; "--init-file" ; contract "bug_locally_bound_vars.mligo" ] ;
  [%expect{|
    "hehe" |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "test" ; "--init-file" ; contract "bug_locally_bound_vars2.mligo" ] ;
  [%expect{|
    43 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "test" ; "--init-file" ; contract "bug_locally_bound_vars3.mligo" ] ;
  [%expect{|
    2 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "test" ; "--init-file" ; contract "bug_locally_bound_vars4.mligo" ] ;
  [%expect{|
    42 |}]
