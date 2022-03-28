open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c )) in
    let #List#length#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs) in
    let #List#size#20 = lambda (xs) return (#List#length#19@{a})@(xs) in
    let #List#head_opt#21 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE() in
    let #List#tail_opt#22 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE() in
    let #A#a#23 = 42 in
    let #B#b#24 = 1 in
    let x = #A#a#23 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c )) in
    let #List#length#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs) in
    let #List#size#20 = lambda (xs) return (#List#length#19@{a})@(xs) in
    let #List#head_opt#21 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE() in
    let #List#tail_opt#22 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE() in
    let #A#a#23 = 40 in
    let #B#b#26 = let #LOCAL#inA#ba#24 = 1 in
    let #LOCAL#inA#baa#25 = #LOCAL#inA#ba#24 in
    ADD(#LOCAL#inA#ba#24 ,
    #LOCAL#inA#baa#25) in
    let x = ADD(#A#a#23 , #B#b#26) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c )) in
    let #List#length#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs) in
    let #List#size#20 = lambda (xs) return (#List#length#19@{a})@(xs) in
    let #List#head_opt#21 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE() in
    let #List#tail_opt#22 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE() in
    let #A#a#23 = 1 in
    let #A_s#as#24 = 42 in
    let #B#x#25 = #A#a#23 in
    let #B#b#26 = #A_s#as#24 in
    let x = #A_s#as#24 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c )) in
  let #List#length#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs) in
  let #List#size#20 = lambda (xs) return (#List#length#19@{a})@(xs) in
  let #List#head_opt#21 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE() in
  let #List#tail_opt#22 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE() in
  let #A_s#as#23 = 20 in
  let #A#s_as#24 = 22 in
  let x = ADD(#A_s#as#23 , #A#s_as#24) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c )) in
  let #List#length#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs) in
  let #List#size#20 = lambda (xs) return (#List#length#19@{a})@(xs) in
  let #List#head_opt#21 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE() in
  let #List#tail_opt#22 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE() in
  let #A#a#23 = 1 in
  let #A#A_s#as#24 = 42 in
  let #A#A_s#as#25 = 3 in
  let x = #A#A_s#as#24 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c )) in
  let #List#length#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs) in
  let #List#size#20 = lambda (xs) return (#List#length#19@{a})@(xs) in
  let #List#head_opt#21 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE() in
  let #List#tail_opt#22 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE() in
  let #Foo#x#23 = 1 in
  let foo = let x = 20 in
  let #LOCAL#inFoo#x#24 = x in
  let #LOCAL#inFoo#y#25 = #Foo#x#23 in
  let #LOCAL#inFoo#z#26 = #LOCAL#inFoo#y#25 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#24 , #LOCAL#inFoo#y#25) , x) ,
  #LOCAL#inFoo#z#26) in
  let x = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c )) in
  let #List#length#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs) in
  let #List#size#20 = lambda (xs) return (#List#length#19@{a})@(xs) in
  let #List#head_opt#21 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE() in
  let #List#tail_opt#22 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE() in
  let #A#v#23 = 40 in
  let #A#B#v#24 = ADD(#A#v#23 , 1) in
  let #A#B#C#v#25 = ADD(#A#B#v#24 , 1) in
  let x = #A#B#C#v#25 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c )) in
  let #List#length#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs) in
  let #List#size#20 = lambda (xs) return (#List#length#19@{a})@(xs) in
  let #List#head_opt#21 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE() in
  let #List#tail_opt#22 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE() in
  let #Foo#x#23 = 41 in
  let x = 1 in
  let #TFoo#x#24 = x in
  let #TFoo#y#25 = #Foo#x#23 in
  let u = ADD(#TFoo#x#24 , #TFoo#y#25) in
  let x = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c )) in
  let #List#length#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs) in
  let #List#size#20 = lambda (xs) return (#List#length#19@{a})@(xs) in
  let #List#head_opt#21 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE() in
  let #List#tail_opt#22 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE() in
  let #A#B#x#23 = 41 in
  let #A#B#x#24 = ADD(#A#B#x#23 , 1) in
  let x = #A#B#x#24 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c )) in
  let #List#length#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs) in
  let #List#size#20 = lambda (xs) return (#List#length#19@{a})@(xs) in
  let #List#head_opt#21 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE() in
  let #List#tail_opt#22 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE() in
  let #A#B#x#23 = 42 in
  let #A#B#x#24 = 2 in
  let #A#y#25 = #A#B#x#23 in
  let x = #A#y#25 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c )) in
  let #List#length#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs) in
  let #List#size#20 = lambda (xs) return (#List#length#19@{a})@(xs) in
  let #List#head_opt#21 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE() in
  let #List#tail_opt#22 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE() in
  let #Foo#x#23 = 19 in
  let #Foo#y#24 = 22 in
  let x = let x = 1 in
  let u = #Foo#x#23 in
  let v = #Foo#y#24 in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c )) in
  let #List#length#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs) in
  let #List#size#20 = lambda (xs) return (#List#length#19@{a})@(xs) in
  let #List#head_opt#21 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE() in
  let #List#tail_opt#22 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE() in
  let #F#F#a#23 = 42 in
  let #F#F#x#24 = #F#F#a#23 in
  let x = #F#F#x#24 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#21 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c )) in
  let #List#length#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs) in
  let #List#size#23 = lambda (xs) return (#List#length#22@{a})@(xs) in
  let #List#head_opt#24 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#14 ->
                                                  match ctor_proj#14 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#16 ->
                                                 NONE() in
  let #List#tail_opt#25 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#17 ->
                                                  match ctor_proj#17 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#19 ->
                                                 NONE() in
  let #A#current_turn#26 = lambda (i) return ADD(i , +1) in
  let #A#other#27 = lambda (n) return let current_turn = (#A#current_turn#26)@(+1) in
  ASSERTION(EQ(n ,
  current_turn)) in
  let main = lambda (gen#12) return  match gen#12 with
                                      | ( _p , _s ) ->
                                      ( LIST_EMPTY() , (#A#other#27)@(+2) ) in
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
  [%expect {|
    { parameter int ;
      storage int ;
      code { CDR ; PUSH string "foo" ; FAILWITH } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "bug_module_record.ligo" ] ;
  [%expect {|
    let #Bytes#concat#18 = fun p -> (({ UNPAIR ; CONCAT })@(p)) in
    let v = PAIR(L(1) , L("b")) in let #A#y#23 = v in let tm = #A#y#23 in L(unit) |}]
