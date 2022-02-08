open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    let #A#a#5 = 42 in
    let #B#b#6 = 1 in
    let x = #A#a#5 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {|
    let #A#a#7 = 40 in
    let #B#b#10 = let #LOCAL#inA#ba#8 = 1 in
    let #LOCAL#inA#baa#9 = #LOCAL#inA#ba#8 in
    ADD(#LOCAL#inA#ba#8 ,
    #LOCAL#inA#baa#9) in
    let x = ADD(#A#a#7 , #B#b#10) in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {|
    let #A#a#7 = 1 in
    let #A_s#as#8 = 42 in
    let #B#x#9 = #A#a#7 in
    let #B#b#10 = #A_s#as#8 in
    let x = #A_s#as#8 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
  let #A_s#as#5 = 20 in
  let #A#s_as#6 = 22 in
  let x = ADD(#A_s#as#5 , #A#s_as#6) in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {|
  let #A#a#6 = 1 in
  let #A#A_s#as#7 = 42 in
  let #A#A_s#as#8 = 3 in
  let x = #A#A_s#as#7 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {|
  let #Foo#x#9 = 1 in
  let foo = let x = 20 in
  let #LOCAL#inFoo#x#10 = x in
  let #LOCAL#inFoo#y#11 = #Foo#x#9 in
  let #LOCAL#inFoo#z#12 = #LOCAL#inFoo#y#11 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#10 , #LOCAL#inFoo#y#11) , x) ,
  #LOCAL#inFoo#z#12) in
  let x = foo in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
  let #A#v#6 = 40 in
  let #A#B#v#7 = ADD(#A#v#6 , 1) in
  let #A#B#C#v#8 = ADD(#A#B#v#7 , 1) in
  let x = #A#B#C#v#8 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {|
  let #Foo#x#8 = 41 in
  let x = 1 in
  let #TFoo#x#9 = x in
  let #TFoo#y#10 = #Foo#x#8 in
  let u = ADD(#TFoo#x#9 , #TFoo#y#10) in
  let x = u in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {|
  let #A#B#x#5 = 41 in
  let #A#B#x#6 = ADD(#A#B#x#5 , 1) in
  let x = #A#B#x#6 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {|
  let #A#B#x#6 = 42 in
  let #A#B#x#7 = 2 in
  let #A#y#8 = #A#B#x#6 in
  let x = #A#y#8 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {|
  let #Foo#x#8 = 19 in
  let #Foo#y#9 = 22 in
  let x = let x = 1 in
  let u = #Foo#x#8 in
  let v = #Foo#y#9 in
  ADD(ADD(u , v) ,
  x) in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {|
  let #F#F#a#5 = 42 in
  let #F#F#x#6 = #F#F#a#5 in
  let x = #F#F#x#6 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {|
  let #A#current_turn#14 = lambda (i) return ADD(i , +1) in
  let #A#other#15 = lambda (n) return let current_turn = (#A#current_turn#14)@(+1) in
  ASSERTION(EQ(n ,
  current_turn)) in
  let main = lambda (gen#9) return  match gen#9 with
                                     | ( _p , _s ) ->
                                     ( LIST_EMPTY() , (#A#other#15)@(+2) ) in
  unit |}]
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
  [%expect {|
    { parameter int ;
      storage int ;
      code { CDR ; PUSH string "foo" ; FAILWITH } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "bug_module_record.ligo" ] ;
  [%expect {|
    L(unit) |}]
