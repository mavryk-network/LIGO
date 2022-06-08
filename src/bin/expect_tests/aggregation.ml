open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #A#a#267 : int = 42 in
    let #B#b#268 : int = 1 in
    let x : int = #A#a#267 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let #A#a#267 : int = 40 in
    let #B#b#270 : int = let #LOCAL#inA#ba#268 : int = 1 in
    let #LOCAL#inA#baa#269 : int = #LOCAL#inA#ba#268 in
    ((#add@{int}@{int})@(#LOCAL#inA#ba#268))@(#LOCAL#inA#baa#269) in
    let x : int = ((#add@{int}@{int})@(#A#a#267))@(#B#b#270) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #A#a#267 : int = 1 in
    let #A_s#as#268 : int = 42 in
    let #B#x#269 : int = #A#a#267 in
    let #B#b#270 : int = #A_s#as#268 in
    let x : int = #A_s#as#268 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #A_s#as#267 : int = 20 in
  let #A#s_as#268 : int = 22 in
  let x : int = ((#add@{int}@{int})@(#A_s#as#267))@(#A#s_as#268) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let #A#a#267 : int = 1 in
  let #A#A_s#as#268 : int = 42 in
  let #A#A_s#as#269 : int = 3 in
  let x : int = #A#A_s#as#268 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#267 : int = 1 in
  let foo : int = let x = 20 in
  let #LOCAL#inFoo#x#268 : int = x in
  let #LOCAL#inFoo#y#269 : int = #Foo#x#267 in
  let #LOCAL#inFoo#z#270 : int = #LOCAL#inFoo#y#269 in
  ((#add@{int}@{int})@(((#add@{int}@{int})@(((#add@{int}@{int})@(#LOCAL#inFoo#x#268))@(#LOCAL#inFoo#y#269)))@(x)))@(#LOCAL#inFoo#z#270) in
  let x : int = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let #A#v#267 : int = 40 in
  let #A#B#v#268 : int = ((#add@{int}@{int})@(#A#v#267))@(1) in
  let #A#B#C#v#269 : int = ((#add@{int}@{int})@(#A#B#v#268))@(1) in
  let x : int = #A#B#C#v#269 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#267 : int = 41 in
  let x : int = 1 in
  let #TFoo#x#268 : int = x in
  let #TFoo#y#269 : int = #Foo#x#267 in
  let u : int = ((#add@{int}@{int})@(#TFoo#x#268))@(#TFoo#y#269) in
  let x : int = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#267 : int = 41 in
  let #A#B#x#268 : int = ((#add@{int}@{int})@(#A#B#x#267))@(1) in
  let x : int = #A#B#x#268 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#267 : int = 42 in
  let #A#B#x#268 : int = 2 in
  let #A#y#269 : int = #A#B#x#267 in
  let x : int = #A#y#269 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#267 : int = 19 in
  let #Foo#y#268 : int = 22 in
  let x : int = let x = 1 in
  let u = #Foo#x#267 in
  let v = #Foo#y#268 in
  ((#add@{int}@{int})@(((#add@{int}@{int})@(u))@(v)))@(x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let #F#F#a#267 : int = 42 in
  let #F#F#x#268 : int = #F#F#a#267 in
  let x : int = #F#F#x#268 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #A#current_turn#269 : nat -> nat = lambda (i : nat) return ((#add@{nat}@{nat})@(i))@(+1) in
  let #A#other#270 : nat -> unit = lambda (n : nat) return let current_turn = (#A#current_turn#269)@(+1) in
  (assert)@(((#eq@{nat})@(n))@(current_turn)) in
  let main : ( unit * unit ) -> ( list (operation) * unit ) = lambda (gen#2 : ( unit * unit )) return  match
                                                                      gen#2 with
                                                                      | ( _p , _s ) ->
                                                                      ( LIST_EMPTY() , (#A#other#270)@(+2) ) in
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
