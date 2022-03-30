open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #Crypto#blake2b#18 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
    let #Crypto#sha256#19 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
    let #Crypto#sha512#20 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
    let #Crypto#sha3#21 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
    let #Crypto#keccak#22 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
    let #Crypto#hash_key#23 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
    let #Crypto#check#24 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
    let #Bytes#concat#25 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#26 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#27 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#28 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#29 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#30 = lambda (xs) return (#List#length#29@{a})@(xs)[@inline] in
    let #List#head_opt#31 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#32 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#33 = 42 in
    let #B#b#34 = 1 in
    let x = #A#a#33 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let #Crypto#blake2b#18 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
    let #Crypto#sha256#19 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
    let #Crypto#sha512#20 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
    let #Crypto#sha3#21 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
    let #Crypto#keccak#22 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
    let #Crypto#hash_key#23 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
    let #Crypto#check#24 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
    let #Bytes#concat#25 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#26 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#27 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#28 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#29 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#30 = lambda (xs) return (#List#length#29@{a})@(xs)[@inline] in
    let #List#head_opt#31 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#32 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#33 = 40 in
    let #B#b#36 = let #LOCAL#inA#ba#34 = 1 in
    let #LOCAL#inA#baa#35 = #LOCAL#inA#ba#34 in
    ADD(#LOCAL#inA#ba#34 ,
    #LOCAL#inA#baa#35) in
    let x = ADD(#A#a#33 , #B#b#36) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #Crypto#blake2b#18 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
    let #Crypto#sha256#19 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
    let #Crypto#sha512#20 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
    let #Crypto#sha3#21 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
    let #Crypto#keccak#22 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
    let #Crypto#hash_key#23 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
    let #Crypto#check#24 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
    let #Bytes#concat#25 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#26 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#27 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#28 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#29 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#30 = lambda (xs) return (#List#length#29@{a})@(xs)[@inline] in
    let #List#head_opt#31 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#32 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#33 = 1 in
    let #A_s#as#34 = 42 in
    let #B#x#35 = #A#a#33 in
    let #B#b#36 = #A_s#as#34 in
    let x = #A_s#as#34 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #Crypto#blake2b#18 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#19 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#20 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#21 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#22 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#23 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#24 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#25 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#26 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#27 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#28 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#29 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#30 = lambda (xs) return (#List#length#29@{a})@(xs)[@inline] in
  let #List#head_opt#31 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#32 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A_s#as#33 = 20 in
  let #A#s_as#34 = 22 in
  let x = ADD(#A_s#as#33 , #A#s_as#34) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let #Crypto#blake2b#18 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#19 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#20 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#21 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#22 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#23 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#24 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#25 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#26 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#27 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#28 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#29 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#30 = lambda (xs) return (#List#length#29@{a})@(xs)[@inline] in
  let #List#head_opt#31 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#32 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#a#33 = 1 in
  let #A#A_s#as#34 = 42 in
  let #A#A_s#as#35 = 3 in
  let x = #A#A_s#as#34 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #Crypto#blake2b#18 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#19 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#20 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#21 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#22 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#23 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#24 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#25 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#26 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#27 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#28 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#29 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#30 = lambda (xs) return (#List#length#29@{a})@(xs)[@inline] in
  let #List#head_opt#31 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#32 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#33 = 1 in
  let foo = let x = 20 in
  let #LOCAL#inFoo#x#34 = x in
  let #LOCAL#inFoo#y#35 = #Foo#x#33 in
  let #LOCAL#inFoo#z#36 = #LOCAL#inFoo#y#35 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#34 , #LOCAL#inFoo#y#35) , x) ,
  #LOCAL#inFoo#z#36) in
  let x = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let #Crypto#blake2b#18 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#19 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#20 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#21 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#22 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#23 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#24 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#25 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#26 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#27 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#28 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#29 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#30 = lambda (xs) return (#List#length#29@{a})@(xs)[@inline] in
  let #List#head_opt#31 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#32 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#v#33 = 40 in
  let #A#B#v#34 = ADD(#A#v#33 , 1) in
  let #A#B#C#v#35 = ADD(#A#B#v#34 , 1) in
  let x = #A#B#C#v#35 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let #Crypto#blake2b#18 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#19 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#20 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#21 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#22 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#23 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#24 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#25 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#26 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#27 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#28 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#29 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#30 = lambda (xs) return (#List#length#29@{a})@(xs)[@inline] in
  let #List#head_opt#31 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#32 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#33 = 41 in
  let x = 1 in
  let #TFoo#x#34 = x in
  let #TFoo#y#35 = #Foo#x#33 in
  let u = ADD(#TFoo#x#34 , #TFoo#y#35) in
  let x = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let #Crypto#blake2b#18 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#19 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#20 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#21 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#22 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#23 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#24 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#25 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#26 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#27 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#28 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#29 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#30 = lambda (xs) return (#List#length#29@{a})@(xs)[@inline] in
  let #List#head_opt#31 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#32 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#B#x#33 = 41 in
  let #A#B#x#34 = ADD(#A#B#x#33 , 1) in
  let x = #A#B#x#34 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #Crypto#blake2b#18 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#19 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#20 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#21 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#22 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#23 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#24 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#25 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#26 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#27 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#28 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#29 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#30 = lambda (xs) return (#List#length#29@{a})@(xs)[@inline] in
  let #List#head_opt#31 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#32 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#B#x#33 = 42 in
  let #A#B#x#34 = 2 in
  let #A#y#35 = #A#B#x#33 in
  let x = #A#y#35 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #Crypto#blake2b#18 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#19 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#20 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#21 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#22 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#23 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#24 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#25 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#26 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#27 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#28 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#29 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#30 = lambda (xs) return (#List#length#29@{a})@(xs)[@inline] in
  let #List#head_opt#31 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#32 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#33 = 19 in
  let #Foo#y#34 = 22 in
  let x = let x = 1 in
  let u = #Foo#x#33 in
  let v = #Foo#y#34 in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let #Crypto#blake2b#18 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#19 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#20 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#21 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#22 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#23 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#24 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#25 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#26 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#27 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#28 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#29 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#30 = lambda (xs) return (#List#length#29@{a})@(xs)[@inline] in
  let #List#head_opt#31 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#32 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #F#F#a#33 = 42 in
  let #F#F#x#34 = #F#F#a#33 in
  let x = #F#F#x#34 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #Crypto#blake2b#21 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#22 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#23 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#24 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#25 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#26 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#27 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#28 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#29 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#30 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#31 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#32 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#33 = lambda (xs) return (#List#length#32@{a})@(xs)[@inline] in
  let #List#head_opt#34 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#14 ->
                                                  match ctor_proj#14 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#16 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#35 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#17 ->
                                                  match ctor_proj#17 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#19 ->
                                                 NONE()[@inline] in
  let #A#current_turn#36 = lambda (i) return ADD(i , +1) in
  let #A#other#37 = lambda (n) return let current_turn = (#A#current_turn#36)@(+1) in
  ASSERTION(EQ(n ,
  current_turn)) in
  let main = lambda (gen#12) return  match gen#12 with
                                      | ( _p , _s ) ->
                                      ( LIST_EMPTY() , (#A#other#37)@(+2) ) in
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
    let #Crypto#blake2b#22 = fun b -> (({ BLAKE2B })@(b))[@inline] in
    let #Crypto#sha256#23 = fun b -> (({ SHA256 })@(b))[@inline] in
    let #Crypto#sha512#24 = fun b -> (({ SHA512 })@(b))[@inline] in
    let #Crypto#sha3#25 = fun b -> (({ SHA3 })@(b))[@inline] in
    let #Crypto#keccak#26 = fun b -> (({ KECCAK })@(b))[@inline] in
    let #Crypto#hash_key#27 = fun k -> (({ HASH_KEY })@(k))[@inline] in
    let #Crypto#check#28 =
      fun gen#2 ->
      (let (gen#38, gen#39) = gen#2 in
       let (gen#40, gen#41) = gen#38 in
       let k = gen#40 in
       let s = gen#41 in
       let b = gen#39 in
       ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))[@inline] in
    let #Bytes#concat#29 = fun p -> (({ UNPAIR ; CONCAT })@(p))[@inline] in
    let #Bytes#sub#30 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #Bytes#length#32 = fun b -> (({ SIZE })@(b))[@inline] in
    let v = PAIR(L(1) , L("b")) in let #A#y#37 = v in let tm = #A#y#37 in L(unit) |}]
