open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #A#a#226 : int = 42 in
    let #B#b#227 : int = 1 in
    let x : int = #A#a#226 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let #A#a#226 : int = 40 in
    let #B#b#229 : int = let #LOCAL#inA#ba#227 : int = 1 in
    let #LOCAL#inA#baa#228 : int = #LOCAL#inA#ba#227 in
    ((#add@{int}@{int})@(#LOCAL#inA#ba#227))@(#LOCAL#inA#baa#228) in
    let x : int = ((#add@{int}@{int})@(#A#a#226))@(#B#b#229) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #A#a#226 : int = 1 in
    let #A_s#as#227 : int = 42 in
    let #B#x#228 : int = #A#a#226 in
    let #B#b#229 : int = #A_s#as#227 in
    let x : int = #A_s#as#227 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #A_s#as#226 : int = 20 in
  let #A#s_as#227 : int = 22 in
  let x : int = ((#add@{int}@{int})@(#A_s#as#226))@(#A#s_as#227) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let #A#a#226 : int = 1 in
  let #A#A_s#as#227 : int = 42 in
  let #A#A_s#as#228 : int = 3 in
  let x : int = #A#A_s#as#227 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#226 : int = 1 in
  let foo : int = let x = 20 in
  let #LOCAL#inFoo#x#227 : int = x in
  let #LOCAL#inFoo#y#228 : int = #Foo#x#226 in
  let #LOCAL#inFoo#z#229 : int = #LOCAL#inFoo#y#228 in
  ((#add@{int}@{int})@(((#add@{int}@{int})@(((#add@{int}@{int})@(#LOCAL#inFoo#x#227))@(#LOCAL#inFoo#y#228)))@(x)))@(#LOCAL#inFoo#z#229) in
  let x : int = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let #A#v#226 : int = 40 in
  let #A#B#v#227 : int = ((#add@{int}@{int})@(#A#v#226))@(1) in
  let #A#B#C#v#228 : int = ((#add@{int}@{int})@(#A#B#v#227))@(1) in
  let x : int = #A#B#C#v#228 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#226 : int = 41 in
  let x : int = 1 in
  let #TFoo#x#227 : int = x in
  let #TFoo#y#228 : int = #Foo#x#226 in
  let u : int = ((#add@{int}@{int})@(#TFoo#x#227))@(#TFoo#y#228) in
  let x : int = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#226 : int = 41 in
  let #A#B#x#227 : int = ((#add@{int}@{int})@(#A#B#x#226))@(1) in
  let x : int = #A#B#x#227 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#226 : int = 42 in
  let #A#B#x#227 : int = 2 in
  let #A#y#228 : int = #A#B#x#226 in
  let x : int = #A#y#228 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#226 : int = 19 in
  let #Foo#y#227 : int = 22 in
  let x : int = let x = 1 in
  let u = #Foo#x#226 in
  let v = #Foo#y#227 in
  ((#add@{int}@{int})@(((#add@{int}@{int})@(u))@(v)))@(x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let #F#F#a#226 : int = 42 in
  let #F#F#x#227 : int = #F#F#a#226 in
  let x : int = #F#F#x#227 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #A#current_turn#228 : nat -> nat = lambda (i : nat) return ((#add@{nat}@{nat})@(i))@(+1) in
  let #A#other#229 : nat -> unit = lambda (n : nat) return let current_turn = (#A#current_turn#228)@(+1) in
  (assert)@(((#eq@{nat})@(n))@(current_turn)) in
  let main : ( unit * unit ) -> ( list (operation) * unit ) = lambda (gen#2 : ( unit * unit )) return  match
                                                                      gen#2 with
                                                                      | ( _p , _s ) ->
                                                                      ( LIST_EMPTY() , (#A#other#229)@(+2) ) in
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
  run_ligo_good [ "print" ; "mini-c" ; contract "bug_module_record.ligo" ] ;
  [%expect {|
    L(unit) |}]
