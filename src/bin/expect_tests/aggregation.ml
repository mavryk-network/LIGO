open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
    let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
    let #Map#size#20 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
    let #Set#cardinal#21 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #Set#size#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #String#length#23 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#size#24 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#sub#25 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#slice#26 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#sub#27 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #String#concat#28 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Crypto#blake2b#29 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
    let #Crypto#sha256#30 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
    let #Crypto#sha512#31 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
    let #Crypto#sha3#32 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
    let #Crypto#keccak#33 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
    let #Crypto#hash_key#34 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
    let #Crypto#check#35 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
    let #Bytes#concat#36 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#37 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#slice#38 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#39 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#40 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#41 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#42 = lambda (xs) return (#List#length#41@{a})@(xs)[@inline] in
    let #List#head_opt#43 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#44 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#45 = 42 in
    let #B#b#46 = 1 in
    let x = #A#a#45 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
    let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
    let #Map#size#20 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
    let #Set#cardinal#21 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #Set#size#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #String#length#23 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#size#24 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#sub#25 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#slice#26 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#sub#27 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #String#concat#28 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Crypto#blake2b#29 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
    let #Crypto#sha256#30 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
    let #Crypto#sha512#31 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
    let #Crypto#sha3#32 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
    let #Crypto#keccak#33 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
    let #Crypto#hash_key#34 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
    let #Crypto#check#35 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
    let #Bytes#concat#36 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#37 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#slice#38 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#39 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#40 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#41 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#42 = lambda (xs) return (#List#length#41@{a})@(xs)[@inline] in
    let #List#head_opt#43 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#44 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#45 = 40 in
    let #B#b#48 = let #LOCAL#inA#ba#46 = 1 in
    let #LOCAL#inA#baa#47 = #LOCAL#inA#ba#46 in
    ADD(#LOCAL#inA#ba#46 ,
    #LOCAL#inA#baa#47) in
    let x = ADD(#A#a#45 , #B#b#48) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
    let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
    let #Map#size#20 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
    let #Set#cardinal#21 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #Set#size#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #String#length#23 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#size#24 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#sub#25 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#slice#26 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#sub#27 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #String#concat#28 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Crypto#blake2b#29 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
    let #Crypto#sha256#30 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
    let #Crypto#sha512#31 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
    let #Crypto#sha3#32 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
    let #Crypto#keccak#33 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
    let #Crypto#hash_key#34 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
    let #Crypto#check#35 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
    let #Bytes#concat#36 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#37 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#slice#38 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#39 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#40 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#41 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#42 = lambda (xs) return (#List#length#41@{a})@(xs)[@inline] in
    let #List#head_opt#43 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#44 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#45 = 1 in
    let #A_s#as#46 = 42 in
    let #B#x#47 = #A#a#45 in
    let #B#b#48 = #A_s#as#46 in
    let x = #A_s#as#46 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Map#size#20 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#21 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#23 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#24 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#25 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#26 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#27 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#28 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#29 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#30 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#31 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#32 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#33 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#34 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#35 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#36 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#37 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#38 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#39 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#40 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#41 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#42 = lambda (xs) return (#List#length#41@{a})@(xs)[@inline] in
  let #List#head_opt#43 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#44 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A_s#as#45 = 20 in
  let #A#s_as#46 = 22 in
  let x = ADD(#A_s#as#45 , #A#s_as#46) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Map#size#20 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#21 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#23 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#24 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#25 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#26 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#27 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#28 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#29 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#30 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#31 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#32 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#33 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#34 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#35 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#36 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#37 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#38 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#39 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#40 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#41 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#42 = lambda (xs) return (#List#length#41@{a})@(xs)[@inline] in
  let #List#head_opt#43 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#44 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#a#45 = 1 in
  let #A#A_s#as#46 = 42 in
  let #A#A_s#as#47 = 3 in
  let x = #A#A_s#as#46 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Map#size#20 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#21 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#23 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#24 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#25 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#26 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#27 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#28 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#29 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#30 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#31 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#32 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#33 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#34 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#35 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#36 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#37 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#38 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#39 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#40 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#41 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#42 = lambda (xs) return (#List#length#41@{a})@(xs)[@inline] in
  let #List#head_opt#43 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#44 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#45 = 1 in
  let foo = let x = 20 in
  let #LOCAL#inFoo#x#46 = x in
  let #LOCAL#inFoo#y#47 = #Foo#x#45 in
  let #LOCAL#inFoo#z#48 = #LOCAL#inFoo#y#47 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#46 , #LOCAL#inFoo#y#47) , x) ,
  #LOCAL#inFoo#z#48) in
  let x = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Map#size#20 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#21 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#23 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#24 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#25 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#26 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#27 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#28 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#29 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#30 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#31 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#32 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#33 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#34 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#35 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#36 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#37 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#38 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#39 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#40 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#41 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#42 = lambda (xs) return (#List#length#41@{a})@(xs)[@inline] in
  let #List#head_opt#43 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#44 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#v#45 = 40 in
  let #A#B#v#46 = ADD(#A#v#45 , 1) in
  let #A#B#C#v#47 = ADD(#A#B#v#46 , 1) in
  let x = #A#B#C#v#47 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Map#size#20 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#21 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#23 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#24 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#25 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#26 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#27 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#28 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#29 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#30 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#31 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#32 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#33 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#34 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#35 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#36 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#37 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#38 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#39 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#40 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#41 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#42 = lambda (xs) return (#List#length#41@{a})@(xs)[@inline] in
  let #List#head_opt#43 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#44 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#45 = 41 in
  let x = 1 in
  let #TFoo#x#46 = x in
  let #TFoo#y#47 = #Foo#x#45 in
  let u = ADD(#TFoo#x#46 , #TFoo#y#47) in
  let x = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Map#size#20 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#21 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#23 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#24 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#25 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#26 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#27 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#28 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#29 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#30 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#31 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#32 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#33 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#34 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#35 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#36 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#37 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#38 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#39 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#40 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#41 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#42 = lambda (xs) return (#List#length#41@{a})@(xs)[@inline] in
  let #List#head_opt#43 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#44 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#B#x#45 = 41 in
  let #A#B#x#46 = ADD(#A#B#x#45 , 1) in
  let x = #A#B#x#46 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Map#size#20 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#21 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#23 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#24 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#25 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#26 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#27 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#28 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#29 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#30 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#31 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#32 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#33 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#34 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#35 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#36 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#37 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#38 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#39 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#40 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#41 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#42 = lambda (xs) return (#List#length#41@{a})@(xs)[@inline] in
  let #List#head_opt#43 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#44 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#B#x#45 = 42 in
  let #A#B#x#46 = 2 in
  let #A#y#47 = #A#B#x#45 in
  let x = #A#y#47 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Map#size#20 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#21 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#23 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#24 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#25 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#26 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#27 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#28 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#29 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#30 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#31 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#32 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#33 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#34 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#35 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#36 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#37 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#38 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#39 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#40 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#41 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#42 = lambda (xs) return (#List#length#41@{a})@(xs)[@inline] in
  let #List#head_opt#43 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#44 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#45 = 19 in
  let #Foo#y#46 = 22 in
  let x = let x = 1 in
  let u = #Foo#x#45 in
  let v = #Foo#y#46 in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Map#size#20 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#21 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#22 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#23 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#24 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#25 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#26 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#27 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#28 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#29 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#30 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#31 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#32 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#33 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#34 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#35 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#36 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#37 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#38 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#39 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#40 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#41 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#42 = lambda (xs) return (#List#length#41@{a})@(xs)[@inline] in
  let #List#head_opt#43 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#44 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #F#F#a#45 = 42 in
  let #F#F#x#46 = #F#F#a#45 in
  let x = #F#F#x#46 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#21 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#22 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Map#size#23 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#24 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#25 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#26 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#27 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#28 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#29 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#30 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#31 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#32 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#33 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#34 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#35 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#36 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#37 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#38 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#39 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#40 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#41 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#42 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#43 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#44 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#45 = lambda (xs) return (#List#length#44@{a})@(xs)[@inline] in
  let #List#head_opt#46 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#14 ->
                                                  match ctor_proj#14 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#16 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#47 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#17 ->
                                                  match ctor_proj#17 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#19 ->
                                                 NONE()[@inline] in
  let #A#current_turn#48 = lambda (i) return ADD(i , +1) in
  let #A#other#49 = lambda (n) return let current_turn = (#A#current_turn#48)@(+1) in
  ASSERTION(EQ(n ,
  current_turn)) in
  let main = lambda (gen#12) return  match gen#12 with
                                      | ( _p , _s ) ->
                                      ( LIST_EMPTY() , (#A#other#49)@(+2) ) in
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
    let #String#length#31 = fun s -> (({ SIZE })@(s))[@inline] in
    let #String#size#32 = fun s -> (({ SIZE })@(s))[@inline] in
    let #String#sub#33 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #String#slice#34 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #String#concat#35 = fun p -> (({ UNPAIR ; CONCAT })@(p))[@inline] in
    let #Crypto#blake2b#36 = fun b -> (({ BLAKE2B })@(b))[@inline] in
    let #Crypto#sha256#37 = fun b -> (({ SHA256 })@(b))[@inline] in
    let #Crypto#sha512#38 = fun b -> (({ SHA512 })@(b))[@inline] in
    let #Crypto#sha3#39 = fun b -> (({ SHA3 })@(b))[@inline] in
    let #Crypto#keccak#40 = fun b -> (({ KECCAK })@(b))[@inline] in
    let #Crypto#hash_key#41 = fun k -> (({ HASH_KEY })@(k))[@inline] in
    let #Crypto#check#42 =
      fun gen#3 ->
      (let (gen#53, gen#54) = gen#3 in
       let (gen#55, gen#56) = gen#53 in
       let k = gen#55 in
       let s = gen#56 in
       let b = gen#54 in
       ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))[@inline] in
    let #Bytes#concat#43 = fun p -> (({ UNPAIR ; CONCAT })@(p))[@inline] in
    let #Bytes#sub#44 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #Bytes#slice#45 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #Bytes#length#47 = fun b -> (({ SIZE })@(b))[@inline] in
    let v = PAIR(L(1) , L("b")) in let #A#y#52 = v in let tm = #A#y#52 in L(unit) |}]
