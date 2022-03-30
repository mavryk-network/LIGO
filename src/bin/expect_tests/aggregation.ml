open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #Map#size#18 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
    let #Set#cardinal#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #Set#size#20 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
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
    let #Bytes#slice#36 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#37 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#38 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#39 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#40 = lambda (xs) return (#List#length#39@{a})@(xs)[@inline] in
    let #List#head_opt#41 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#42 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#43 = 42 in
    let #B#b#44 = 1 in
    let x = #A#a#43 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let #Map#size#18 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
    let #Set#cardinal#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #Set#size#20 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
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
    let #Bytes#slice#36 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#37 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#38 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#39 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#40 = lambda (xs) return (#List#length#39@{a})@(xs)[@inline] in
    let #List#head_opt#41 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#42 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#43 = 40 in
    let #B#b#46 = let #LOCAL#inA#ba#44 = 1 in
    let #LOCAL#inA#baa#45 = #LOCAL#inA#ba#44 in
    ADD(#LOCAL#inA#ba#44 ,
    #LOCAL#inA#baa#45) in
    let x = ADD(#A#a#43 , #B#b#46) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #Map#size#18 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
    let #Set#cardinal#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #Set#size#20 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
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
    let #Bytes#slice#36 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#37 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#38 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#39 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#40 = lambda (xs) return (#List#length#39@{a})@(xs)[@inline] in
    let #List#head_opt#41 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#42 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#43 = 1 in
    let #A_s#as#44 = 42 in
    let #B#x#45 = #A#a#43 in
    let #B#b#46 = #A_s#as#44 in
    let x = #A_s#as#44 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #Map#size#18 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#20 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
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
  let #Bytes#slice#36 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#37 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#38 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#39 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#40 = lambda (xs) return (#List#length#39@{a})@(xs)[@inline] in
  let #List#head_opt#41 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#42 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A_s#as#43 = 20 in
  let #A#s_as#44 = 22 in
  let x = ADD(#A_s#as#43 , #A#s_as#44) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let #Map#size#18 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#20 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
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
  let #Bytes#slice#36 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#37 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#38 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#39 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#40 = lambda (xs) return (#List#length#39@{a})@(xs)[@inline] in
  let #List#head_opt#41 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#42 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#a#43 = 1 in
  let #A#A_s#as#44 = 42 in
  let #A#A_s#as#45 = 3 in
  let x = #A#A_s#as#44 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #Map#size#18 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#20 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
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
  let #Bytes#slice#36 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#37 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#38 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#39 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#40 = lambda (xs) return (#List#length#39@{a})@(xs)[@inline] in
  let #List#head_opt#41 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#42 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#43 = 1 in
  let foo = let x = 20 in
  let #LOCAL#inFoo#x#44 = x in
  let #LOCAL#inFoo#y#45 = #Foo#x#43 in
  let #LOCAL#inFoo#z#46 = #LOCAL#inFoo#y#45 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#44 , #LOCAL#inFoo#y#45) , x) ,
  #LOCAL#inFoo#z#46) in
  let x = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let #Map#size#18 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#20 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
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
  let #Bytes#slice#36 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#37 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#38 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#39 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#40 = lambda (xs) return (#List#length#39@{a})@(xs)[@inline] in
  let #List#head_opt#41 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#42 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#v#43 = 40 in
  let #A#B#v#44 = ADD(#A#v#43 , 1) in
  let #A#B#C#v#45 = ADD(#A#B#v#44 , 1) in
  let x = #A#B#C#v#45 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let #Map#size#18 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#20 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
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
  let #Bytes#slice#36 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#37 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#38 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#39 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#40 = lambda (xs) return (#List#length#39@{a})@(xs)[@inline] in
  let #List#head_opt#41 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#42 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#43 = 41 in
  let x = 1 in
  let #TFoo#x#44 = x in
  let #TFoo#y#45 = #Foo#x#43 in
  let u = ADD(#TFoo#x#44 , #TFoo#y#45) in
  let x = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let #Map#size#18 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#20 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
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
  let #Bytes#slice#36 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#37 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#38 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#39 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#40 = lambda (xs) return (#List#length#39@{a})@(xs)[@inline] in
  let #List#head_opt#41 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#42 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#B#x#43 = 41 in
  let #A#B#x#44 = ADD(#A#B#x#43 , 1) in
  let x = #A#B#x#44 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #Map#size#18 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#20 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
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
  let #Bytes#slice#36 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#37 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#38 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#39 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#40 = lambda (xs) return (#List#length#39@{a})@(xs)[@inline] in
  let #List#head_opt#41 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#42 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#B#x#43 = 42 in
  let #A#B#x#44 = 2 in
  let #A#y#45 = #A#B#x#43 in
  let x = #A#y#45 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #Map#size#18 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#20 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
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
  let #Bytes#slice#36 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#37 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#38 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#39 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#40 = lambda (xs) return (#List#length#39@{a})@(xs)[@inline] in
  let #List#head_opt#41 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#42 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#43 = 19 in
  let #Foo#y#44 = 22 in
  let x = let x = 1 in
  let u = #Foo#x#43 in
  let v = #Foo#y#44 in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let #Map#size#18 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#19 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#20 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
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
  let #Bytes#slice#36 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#37 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#38 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#39 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#40 = lambda (xs) return (#List#length#39@{a})@(xs)[@inline] in
  let #List#head_opt#41 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#42 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #F#F#a#43 = 42 in
  let #F#F#x#44 = #F#F#a#43 in
  let x = #F#F#x#44 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #Map#size#21 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#23 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#24 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#25 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#26 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#27 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#28 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#29 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#30 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#31 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#32 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#33 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#34 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#35 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#36 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#37 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#38 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#39 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#40 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#41 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#42 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#43 = lambda (xs) return (#List#length#42@{a})@(xs)[@inline] in
  let #List#head_opt#44 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#14 ->
                                                  match ctor_proj#14 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#16 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#45 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#17 ->
                                                  match ctor_proj#17 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#19 ->
                                                 NONE()[@inline] in
  let #A#current_turn#46 = lambda (i) return ADD(i , +1) in
  let #A#other#47 = lambda (n) return let current_turn = (#A#current_turn#46)@(+1) in
  ASSERTION(EQ(n ,
  current_turn)) in
  let main = lambda (gen#12) return  match gen#12 with
                                      | ( _p , _s ) ->
                                      ( LIST_EMPTY() , (#A#other#47)@(+2) ) in
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
    let #String#length#25 = fun s -> (({ SIZE })@(s))[@inline] in
    let #String#size#26 = fun s -> (({ SIZE })@(s))[@inline] in
    let #String#sub#27 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #String#slice#28 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #String#concat#29 = fun p -> (({ UNPAIR ; CONCAT })@(p))[@inline] in
    let #Crypto#blake2b#30 = fun b -> (({ BLAKE2B })@(b))[@inline] in
    let #Crypto#sha256#31 = fun b -> (({ SHA256 })@(b))[@inline] in
    let #Crypto#sha512#32 = fun b -> (({ SHA512 })@(b))[@inline] in
    let #Crypto#sha3#33 = fun b -> (({ SHA3 })@(b))[@inline] in
    let #Crypto#keccak#34 = fun b -> (({ KECCAK })@(b))[@inline] in
    let #Crypto#hash_key#35 = fun k -> (({ HASH_KEY })@(k))[@inline] in
    let #Crypto#check#36 =
      fun gen#2 ->
      (let (gen#47, gen#48) = gen#2 in
       let (gen#49, gen#50) = gen#47 in
       let k = gen#49 in
       let s = gen#50 in
       let b = gen#48 in
       ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))[@inline] in
    let #Bytes#concat#37 = fun p -> (({ UNPAIR ; CONCAT })@(p))[@inline] in
    let #Bytes#sub#38 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #Bytes#slice#39 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #Bytes#length#41 = fun b -> (({ SIZE })@(b))[@inline] in
    let v = PAIR(L(1) , L("b")) in let #A#y#46 = v in let tm = #A#y#46 in L(unit) |}]
