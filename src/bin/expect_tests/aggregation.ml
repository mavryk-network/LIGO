open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#19 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#20 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#21 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#23 = lambda (xs) return (#List#length#22@{a})@(xs)[@inline] in
    let #List#head_opt#24 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#25 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#26 = 42 in
    let #B#b#27 = 1 in
    let x = #A#a#26 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#19 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#20 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#21 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#23 = lambda (xs) return (#List#length#22@{a})@(xs)[@inline] in
    let #List#head_opt#24 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#25 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#26 = 40 in
    let #B#b#29 = let #LOCAL#inA#ba#27 = 1 in
    let #LOCAL#inA#baa#28 = #LOCAL#inA#ba#27 in
    ADD(#LOCAL#inA#ba#27 ,
    #LOCAL#inA#baa#28) in
    let x = ADD(#A#a#26 , #B#b#29) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#19 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#20 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#21 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#23 = lambda (xs) return (#List#length#22@{a})@(xs)[@inline] in
    let #List#head_opt#24 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#25 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#26 = 1 in
    let #A_s#as#27 = 42 in
    let #B#x#28 = #A#a#26 in
    let #B#b#29 = #A_s#as#27 in
    let x = #A_s#as#27 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#19 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#20 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#21 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#23 = lambda (xs) return (#List#length#22@{a})@(xs)[@inline] in
  let #List#head_opt#24 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#25 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A_s#as#26 = 20 in
  let #A#s_as#27 = 22 in
  let x = ADD(#A_s#as#26 , #A#s_as#27) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#19 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#20 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#21 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#23 = lambda (xs) return (#List#length#22@{a})@(xs)[@inline] in
  let #List#head_opt#24 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#25 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#a#26 = 1 in
  let #A#A_s#as#27 = 42 in
  let #A#A_s#as#28 = 3 in
  let x = #A#A_s#as#27 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#19 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#20 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#21 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#23 = lambda (xs) return (#List#length#22@{a})@(xs)[@inline] in
  let #List#head_opt#24 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#25 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#26 = 1 in
  let foo = let x = 20 in
  let #LOCAL#inFoo#x#27 = x in
  let #LOCAL#inFoo#y#28 = #Foo#x#26 in
  let #LOCAL#inFoo#z#29 = #LOCAL#inFoo#y#28 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#27 , #LOCAL#inFoo#y#28) , x) ,
  #LOCAL#inFoo#z#29) in
  let x = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#19 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#20 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#21 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#23 = lambda (xs) return (#List#length#22@{a})@(xs)[@inline] in
  let #List#head_opt#24 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#25 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#v#26 = 40 in
  let #A#B#v#27 = ADD(#A#v#26 , 1) in
  let #A#B#C#v#28 = ADD(#A#B#v#27 , 1) in
  let x = #A#B#C#v#28 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#19 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#20 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#21 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#23 = lambda (xs) return (#List#length#22@{a})@(xs)[@inline] in
  let #List#head_opt#24 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#25 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#26 = 41 in
  let x = 1 in
  let #TFoo#x#27 = x in
  let #TFoo#y#28 = #Foo#x#26 in
  let u = ADD(#TFoo#x#27 , #TFoo#y#28) in
  let x = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#19 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#20 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#21 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#23 = lambda (xs) return (#List#length#22@{a})@(xs)[@inline] in
  let #List#head_opt#24 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#25 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#B#x#26 = 41 in
  let #A#B#x#27 = ADD(#A#B#x#26 , 1) in
  let x = #A#B#x#27 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#19 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#20 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#21 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#23 = lambda (xs) return (#List#length#22@{a})@(xs)[@inline] in
  let #List#head_opt#24 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#25 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#B#x#26 = 42 in
  let #A#B#x#27 = 2 in
  let #A#y#28 = #A#B#x#26 in
  let x = #A#y#28 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#19 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#20 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#21 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#23 = lambda (xs) return (#List#length#22@{a})@(xs)[@inline] in
  let #List#head_opt#24 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#25 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#26 = 19 in
  let #Foo#y#27 = 22 in
  let x = let x = 1 in
  let u = #Foo#x#26 in
  let v = #Foo#y#27 in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#18 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#19 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#20 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#21 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#23 = lambda (xs) return (#List#length#22@{a})@(xs)[@inline] in
  let #List#head_opt#24 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#25 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #F#F#a#26 = 42 in
  let #F#F#x#27 = #F#F#a#26 in
  let x = #F#F#x#27 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #Bytes#concat#21 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#22 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#23 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#24 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#25 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#26 = lambda (xs) return (#List#length#25@{a})@(xs)[@inline] in
  let #List#head_opt#27 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#14 ->
                                                  match ctor_proj#14 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#16 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#28 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#17 ->
                                                  match ctor_proj#17 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#19 ->
                                                 NONE()[@inline] in
  let #A#current_turn#29 = lambda (i) return ADD(i , +1) in
  let #A#other#30 = lambda (n) return let current_turn = (#A#current_turn#29)@(+1) in
  ASSERTION(EQ(n ,
  current_turn)) in
  let main = lambda (gen#12) return  match gen#12 with
                                      | ( _p , _s ) ->
                                      ( LIST_EMPTY() , (#A#other#30)@(+2) ) in
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
    let #Bytes#concat#18 = fun p -> (({ UNPAIR ; CONCAT })@(p))[@inline] in
    let #Bytes#sub#19 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #Bytes#length#21 = fun b -> (({ SIZE })@(b))[@inline] in
    let v = PAIR(L(1) , L("b")) in let #A#y#26 = v in let tm = #A#y#26 in L(unit) |}]
