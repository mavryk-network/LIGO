open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #String#length#18 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#size#19 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#sub#20 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#slice#21 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#sub#22 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #String#concat#23 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Crypto#blake2b#24 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
    let #Crypto#sha256#25 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
    let #Crypto#sha512#26 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
    let #Crypto#sha3#27 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
    let #Crypto#keccak#28 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
    let #Crypto#hash_key#29 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
    let #Crypto#check#30 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
    let #Bytes#concat#31 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#32 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#33 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#34 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#35 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#36 = lambda (xs) return (#List#length#35@{a})@(xs)[@inline] in
    let #List#head_opt#37 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#38 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#39 = 42 in
    let #B#b#40 = 1 in
    let x = #A#a#39 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let #String#length#18 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#size#19 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#sub#20 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#slice#21 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#sub#22 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #String#concat#23 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Crypto#blake2b#24 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
    let #Crypto#sha256#25 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
    let #Crypto#sha512#26 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
    let #Crypto#sha3#27 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
    let #Crypto#keccak#28 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
    let #Crypto#hash_key#29 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
    let #Crypto#check#30 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
    let #Bytes#concat#31 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#32 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#33 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#34 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#35 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#36 = lambda (xs) return (#List#length#35@{a})@(xs)[@inline] in
    let #List#head_opt#37 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#38 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#39 = 40 in
    let #B#b#42 = let #LOCAL#inA#ba#40 = 1 in
    let #LOCAL#inA#baa#41 = #LOCAL#inA#ba#40 in
    ADD(#LOCAL#inA#ba#40 ,
    #LOCAL#inA#baa#41) in
    let x = ADD(#A#a#39 , #B#b#42) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #String#length#18 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#size#19 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#sub#20 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#slice#21 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#sub#22 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #String#concat#23 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Crypto#blake2b#24 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
    let #Crypto#sha256#25 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
    let #Crypto#sha512#26 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
    let #Crypto#sha3#27 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
    let #Crypto#keccak#28 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
    let #Crypto#hash_key#29 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
    let #Crypto#check#30 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
    let #Bytes#concat#31 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#32 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#33 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#34 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#35 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#36 = lambda (xs) return (#List#length#35@{a})@(xs)[@inline] in
    let #List#head_opt#37 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#38 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#39 = 1 in
    let #A_s#as#40 = 42 in
    let #B#x#41 = #A#a#39 in
    let #B#b#42 = #A_s#as#40 in
    let x = #A_s#as#40 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #String#length#18 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#19 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#20 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#21 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#22 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#23 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#24 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#25 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#26 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#27 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#28 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#29 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#30 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#31 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#32 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#33 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#34 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#35 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#36 = lambda (xs) return (#List#length#35@{a})@(xs)[@inline] in
  let #List#head_opt#37 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#38 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A_s#as#39 = 20 in
  let #A#s_as#40 = 22 in
  let x = ADD(#A_s#as#39 , #A#s_as#40) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let #String#length#18 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#19 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#20 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#21 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#22 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#23 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#24 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#25 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#26 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#27 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#28 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#29 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#30 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#31 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#32 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#33 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#34 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#35 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#36 = lambda (xs) return (#List#length#35@{a})@(xs)[@inline] in
  let #List#head_opt#37 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#38 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#a#39 = 1 in
  let #A#A_s#as#40 = 42 in
  let #A#A_s#as#41 = 3 in
  let x = #A#A_s#as#40 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #String#length#18 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#19 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#20 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#21 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#22 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#23 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#24 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#25 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#26 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#27 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#28 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#29 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#30 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#31 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#32 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#33 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#34 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#35 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#36 = lambda (xs) return (#List#length#35@{a})@(xs)[@inline] in
  let #List#head_opt#37 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#38 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#39 = 1 in
  let foo = let x = 20 in
  let #LOCAL#inFoo#x#40 = x in
  let #LOCAL#inFoo#y#41 = #Foo#x#39 in
  let #LOCAL#inFoo#z#42 = #LOCAL#inFoo#y#41 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#40 , #LOCAL#inFoo#y#41) , x) ,
  #LOCAL#inFoo#z#42) in
  let x = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let #String#length#18 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#19 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#20 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#21 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#22 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#23 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#24 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#25 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#26 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#27 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#28 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#29 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#30 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#31 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#32 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#33 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#34 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#35 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#36 = lambda (xs) return (#List#length#35@{a})@(xs)[@inline] in
  let #List#head_opt#37 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#38 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#v#39 = 40 in
  let #A#B#v#40 = ADD(#A#v#39 , 1) in
  let #A#B#C#v#41 = ADD(#A#B#v#40 , 1) in
  let x = #A#B#C#v#41 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let #String#length#18 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#19 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#20 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#21 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#22 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#23 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#24 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#25 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#26 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#27 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#28 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#29 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#30 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#31 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#32 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#33 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#34 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#35 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#36 = lambda (xs) return (#List#length#35@{a})@(xs)[@inline] in
  let #List#head_opt#37 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#38 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#39 = 41 in
  let x = 1 in
  let #TFoo#x#40 = x in
  let #TFoo#y#41 = #Foo#x#39 in
  let u = ADD(#TFoo#x#40 , #TFoo#y#41) in
  let x = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let #String#length#18 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#19 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#20 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#21 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#22 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#23 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#24 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#25 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#26 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#27 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#28 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#29 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#30 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#31 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#32 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#33 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#34 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#35 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#36 = lambda (xs) return (#List#length#35@{a})@(xs)[@inline] in
  let #List#head_opt#37 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#38 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#B#x#39 = 41 in
  let #A#B#x#40 = ADD(#A#B#x#39 , 1) in
  let x = #A#B#x#40 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #String#length#18 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#19 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#20 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#21 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#22 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#23 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#24 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#25 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#26 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#27 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#28 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#29 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#30 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#31 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#32 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#33 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#34 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#35 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#36 = lambda (xs) return (#List#length#35@{a})@(xs)[@inline] in
  let #List#head_opt#37 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#38 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#B#x#39 = 42 in
  let #A#B#x#40 = 2 in
  let #A#y#41 = #A#B#x#39 in
  let x = #A#y#41 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #String#length#18 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#19 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#20 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#21 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#22 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#23 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#24 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#25 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#26 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#27 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#28 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#29 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#30 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#31 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#32 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#33 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#34 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#35 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#36 = lambda (xs) return (#List#length#35@{a})@(xs)[@inline] in
  let #List#head_opt#37 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#38 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#39 = 19 in
  let #Foo#y#40 = 22 in
  let x = let x = 1 in
  let u = #Foo#x#39 in
  let v = #Foo#y#40 in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let #String#length#18 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#19 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#20 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#21 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#22 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#23 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#24 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#25 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#26 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#27 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#28 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#29 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#30 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#31 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#32 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#33 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#34 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#35 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#36 = lambda (xs) return (#List#length#35@{a})@(xs)[@inline] in
  let #List#head_opt#37 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#38 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #F#F#a#39 = 42 in
  let #F#F#x#40 = #F#F#a#39 in
  let x = #F#F#x#40 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #String#length#21 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#22 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#23 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#24 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#25 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#26 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#27 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#28 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#29 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#30 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#31 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#32 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#33 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#34 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#35 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#36 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#37 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#38 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#39 = lambda (xs) return (#List#length#38@{a})@(xs)[@inline] in
  let #List#head_opt#40 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#14 ->
                                                  match ctor_proj#14 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#16 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#41 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#17 ->
                                                  match ctor_proj#17 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#19 ->
                                                 NONE()[@inline] in
  let #A#current_turn#42 = lambda (i) return ADD(i , +1) in
  let #A#other#43 = lambda (n) return let current_turn = (#A#current_turn#42)@(+1) in
  ASSERTION(EQ(n ,
  current_turn)) in
  let main = lambda (gen#12) return  match gen#12 with
                                      | ( _p , _s ) ->
                                      ( LIST_EMPTY() , (#A#other#43)@(+2) ) in
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
    let #String#length#22 = fun s -> (({ SIZE })@(s))[@inline] in
    let #String#size#23 = fun s -> (({ SIZE })@(s))[@inline] in
    let #String#sub#24 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #String#slice#25 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #String#concat#26 = fun p -> (({ UNPAIR ; CONCAT })@(p))[@inline] in
    let #Crypto#blake2b#27 = fun b -> (({ BLAKE2B })@(b))[@inline] in
    let #Crypto#sha256#28 = fun b -> (({ SHA256 })@(b))[@inline] in
    let #Crypto#sha512#29 = fun b -> (({ SHA512 })@(b))[@inline] in
    let #Crypto#sha3#30 = fun b -> (({ SHA3 })@(b))[@inline] in
    let #Crypto#keccak#31 = fun b -> (({ KECCAK })@(b))[@inline] in
    let #Crypto#hash_key#32 = fun k -> (({ HASH_KEY })@(k))[@inline] in
    let #Crypto#check#33 =
      fun gen#2 ->
      (let (gen#43, gen#44) = gen#2 in
       let (gen#45, gen#46) = gen#43 in
       let k = gen#45 in
       let s = gen#46 in
       let b = gen#44 in
       ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))[@inline] in
    let #Bytes#concat#34 = fun p -> (({ UNPAIR ; CONCAT })@(p))[@inline] in
    let #Bytes#sub#35 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #Bytes#length#37 = fun b -> (({ SIZE })@(b))[@inline] in
    let v = PAIR(L(1) , L("b")) in let #A#y#42 = v in let tm = #A#y#42 in L(unit) |}]
