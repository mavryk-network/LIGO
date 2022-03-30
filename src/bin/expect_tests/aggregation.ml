open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
    let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
    let #Big_map#find_opt#20 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
    let #Map#find_opt#21 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
    let #Map#size#22 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
    let #Set#cardinal#23 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #Set#size#24 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #String#length#25 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#size#26 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#sub#27 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#slice#28 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#sub#29 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #String#concat#30 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Crypto#blake2b#31 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
    let #Crypto#sha256#32 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
    let #Crypto#sha512#33 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
    let #Crypto#sha3#34 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
    let #Crypto#keccak#35 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
    let #Crypto#hash_key#36 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
    let #Crypto#check#37 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
    let #Bytes#concat#38 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#39 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#slice#40 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#41 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#42 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#43 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#44 = lambda (xs) return (#List#length#43@{a})@(xs)[@inline] in
    let #List#head_opt#45 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#46 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#47 = 42 in
    let #B#b#48 = 1 in
    let x = #A#a#47 in
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
    let #Big_map#find_opt#20 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
    let #Map#find_opt#21 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
    let #Map#size#22 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
    let #Set#cardinal#23 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #Set#size#24 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #String#length#25 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#size#26 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#sub#27 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#slice#28 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#sub#29 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #String#concat#30 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Crypto#blake2b#31 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
    let #Crypto#sha256#32 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
    let #Crypto#sha512#33 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
    let #Crypto#sha3#34 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
    let #Crypto#keccak#35 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
    let #Crypto#hash_key#36 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
    let #Crypto#check#37 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
    let #Bytes#concat#38 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#39 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#slice#40 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#41 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#42 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#43 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#44 = lambda (xs) return (#List#length#43@{a})@(xs)[@inline] in
    let #List#head_opt#45 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#46 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#47 = 40 in
    let #B#b#50 = let #LOCAL#inA#ba#48 = 1 in
    let #LOCAL#inA#baa#49 = #LOCAL#inA#ba#48 in
    ADD(#LOCAL#inA#ba#48 ,
    #LOCAL#inA#baa#49) in
    let x = ADD(#A#a#47 , #B#b#50) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
    let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
    let #Big_map#find_opt#20 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
    let #Map#find_opt#21 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
    let #Map#size#22 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
    let #Set#cardinal#23 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #Set#size#24 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #String#length#25 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#size#26 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
    let #String#sub#27 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#slice#28 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
    let #String#sub#29 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #String#concat#30 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Crypto#blake2b#31 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
    let #Crypto#sha256#32 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
    let #Crypto#sha512#33 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
    let #Crypto#sha3#34 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
    let #Crypto#keccak#35 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
    let #Crypto#hash_key#36 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
    let #Crypto#check#37 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
    let #Bytes#concat#38 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
    let #Bytes#sub#39 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#slice#40 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
    let #Bytes#pack#41 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
    let #Bytes#length#42 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
    let #List#length#43 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
    let #List#size#44 = lambda (xs) return (#List#length#43@{a})@(xs)[@inline] in
    let #List#head_opt#45 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#12 ->
                                                    match ctor_proj#12 with
                                                     | ( x , _#2 ) ->
                                                     SOME(x)
                                                 | Nil unit_proj#14 ->
                                                   NONE()[@inline] in
    let #List#tail_opt#46 = lambda (xs) return  match xs with
                                                 | Cons ctor_proj#15 ->
                                                    match ctor_proj#15 with
                                                     | ( _#3 , xs ) ->
                                                     SOME(xs)
                                                 | Nil unit_proj#17 ->
                                                   NONE()[@inline] in
    let #A#a#47 = 1 in
    let #A_s#as#48 = 42 in
    let #B#x#49 = #A#a#47 in
    let #B#b#50 = #A_s#as#48 in
    let x = #A_s#as#48 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Big_map#find_opt#20 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#find_opt#21 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#size#22 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#23 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#24 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#25 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#26 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#27 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#28 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#29 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#30 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#31 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#32 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#33 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#34 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#35 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#36 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#37 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#38 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#39 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#40 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#41 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#42 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#43 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#44 = lambda (xs) return (#List#length#43@{a})@(xs)[@inline] in
  let #List#head_opt#45 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#46 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A_s#as#47 = 20 in
  let #A#s_as#48 = 22 in
  let x = ADD(#A_s#as#47 , #A#s_as#48) in
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
  let #Big_map#find_opt#20 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#find_opt#21 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#size#22 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#23 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#24 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#25 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#26 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#27 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#28 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#29 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#30 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#31 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#32 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#33 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#34 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#35 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#36 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#37 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#38 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#39 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#40 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#41 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#42 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#43 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#44 = lambda (xs) return (#List#length#43@{a})@(xs)[@inline] in
  let #List#head_opt#45 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#46 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#a#47 = 1 in
  let #A#A_s#as#48 = 42 in
  let #A#A_s#as#49 = 3 in
  let x = #A#A_s#as#48 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Big_map#find_opt#20 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#find_opt#21 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#size#22 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#23 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#24 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#25 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#26 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#27 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#28 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#29 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#30 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#31 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#32 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#33 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#34 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#35 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#36 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#37 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#38 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#39 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#40 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#41 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#42 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#43 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#44 = lambda (xs) return (#List#length#43@{a})@(xs)[@inline] in
  let #List#head_opt#45 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#46 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#47 = 1 in
  let foo = let x = 20 in
  let #LOCAL#inFoo#x#48 = x in
  let #LOCAL#inFoo#y#49 = #Foo#x#47 in
  let #LOCAL#inFoo#z#50 = #LOCAL#inFoo#y#49 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#48 , #LOCAL#inFoo#y#49) , x) ,
  #LOCAL#inFoo#z#50) in
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
  let #Big_map#find_opt#20 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#find_opt#21 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#size#22 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#23 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#24 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#25 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#26 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#27 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#28 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#29 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#30 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#31 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#32 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#33 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#34 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#35 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#36 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#37 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#38 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#39 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#40 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#41 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#42 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#43 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#44 = lambda (xs) return (#List#length#43@{a})@(xs)[@inline] in
  let #List#head_opt#45 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#46 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#v#47 = 40 in
  let #A#B#v#48 = ADD(#A#v#47 , 1) in
  let #A#B#C#v#49 = ADD(#A#B#v#48 , 1) in
  let x = #A#B#C#v#49 in
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
  let #Big_map#find_opt#20 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#find_opt#21 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#size#22 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#23 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#24 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#25 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#26 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#27 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#28 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#29 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#30 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#31 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#32 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#33 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#34 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#35 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#36 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#37 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#38 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#39 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#40 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#41 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#42 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#43 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#44 = lambda (xs) return (#List#length#43@{a})@(xs)[@inline] in
  let #List#head_opt#45 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#46 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#47 = 41 in
  let x = 1 in
  let #TFoo#x#48 = x in
  let #TFoo#y#49 = #Foo#x#47 in
  let u = ADD(#TFoo#x#48 , #TFoo#y#49) in
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
  let #Big_map#find_opt#20 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#find_opt#21 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#size#22 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#23 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#24 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#25 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#26 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#27 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#28 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#29 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#30 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#31 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#32 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#33 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#34 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#35 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#36 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#37 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#38 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#39 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#40 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#41 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#42 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#43 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#44 = lambda (xs) return (#List#length#43@{a})@(xs)[@inline] in
  let #List#head_opt#45 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#46 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#B#x#47 = 41 in
  let #A#B#x#48 = ADD(#A#B#x#47 , 1) in
  let x = #A#B#x#48 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Big_map#find_opt#20 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#find_opt#21 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#size#22 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#23 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#24 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#25 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#26 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#27 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#28 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#29 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#30 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#31 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#32 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#33 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#34 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#35 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#36 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#37 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#38 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#39 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#40 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#41 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#42 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#43 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#44 = lambda (xs) return (#List#length#43@{a})@(xs)[@inline] in
  let #List#head_opt#45 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#46 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #A#B#x#47 = 42 in
  let #A#B#x#48 = 2 in
  let #A#y#49 = #A#B#x#47 in
  let x = #A#y#49 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#18 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#19 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Big_map#find_opt#20 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#find_opt#21 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#size#22 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#23 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#24 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#25 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#26 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#27 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#28 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#29 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#30 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#31 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#32 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#33 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#34 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#35 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#36 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#37 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#38 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#39 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#40 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#41 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#42 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#43 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#44 = lambda (xs) return (#List#length#43@{a})@(xs)[@inline] in
  let #List#head_opt#45 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#46 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #Foo#x#47 = 19 in
  let #Foo#y#48 = 22 in
  let x = let x = 1 in
  let u = #Foo#x#47 in
  let v = #Foo#y#48 in
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
  let #Big_map#find_opt#20 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#find_opt#21 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#size#22 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#23 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#24 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#25 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#26 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#27 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#28 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#29 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#30 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#31 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#32 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#33 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#34 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#35 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#36 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#37 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#38 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#39 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#40 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#41 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#42 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#43 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#44 = lambda (xs) return (#List#length#43@{a})@(xs)[@inline] in
  let #List#head_opt#45 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#12 ->
                                                  match ctor_proj#12 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#14 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#46 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#15 ->
                                                  match ctor_proj#15 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#17 ->
                                                 NONE()[@inline] in
  let #F#F#a#47 = 42 in
  let #F#F#x#48 = #F#F#a#47 in
  let x = #F#F#x#48 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #Option#unopt#21 = lambda (o) return ([%Michelson {| { IF_NONE { PUSH string "option is None" ; FAILWITH } {} } |}])@(o)[@inline] in
  let #Option#unopt_with_error#22 = lambda (o) return lambda (s) return ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { SWAP ; DROP } } |}])@(( o , s ))[@inline] in
  let #Big_map#find_opt#23 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#find_opt#24 = lambda (k) return lambda (m) return ([%Michelson {| { UNPAIR ; GET } |}])@(( k , m ))[@inline] in
  let #Map#size#25 = lambda (m) return ([%Michelson {| { SIZE } |}])@(m)[@inline] in
  let #Set#cardinal#26 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #Set#size#27 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #String#length#28 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#size#29 = lambda (s) return ([%Michelson {| { SIZE } |}])@(s)[@inline] in
  let #String#sub#30 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#slice#31 = lambda (sli) return ([%Michelson {| { UNPAIR  ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(sli)[@inline] in
  let #String#sub#32 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #String#concat#33 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Crypto#blake2b#34 = lambda (b) return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline] in
  let #Crypto#sha256#35 = lambda (b) return ([%Michelson {| { SHA256 } |}])@(b)[@inline] in
  let #Crypto#sha512#36 = lambda (b) return ([%Michelson {| { SHA512 } |}])@(b)[@inline] in
  let #Crypto#sha3#37 = lambda (b) return ([%Michelson {| { SHA3 } |}])@(b)[@inline] in
  let #Crypto#keccak#38 = lambda (b) return ([%Michelson {| { KECCAK } |}])@(b)[@inline] in
  let #Crypto#hash_key#39 = lambda (k) return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline] in
  let #Crypto#check#40 = lambda (k) return lambda (s) return lambda (b) return ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline] in
  let #Bytes#concat#41 = lambda (b) return lambda (c) return ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b , c ))[@inline] in
  let #Bytes#sub#42 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#slice#43 = lambda (start) return lambda (length) return lambda (input) return ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(( start , length , input ))[@inline] in
  let #Bytes#pack#44 = lambda (x) return ([%Michelson {| { PACK } |}])@(x)[@inline] in
  let #Bytes#length#45 = lambda (b) return ([%Michelson {| { SIZE } |}])@(b)[@inline] in
  let #List#length#46 = lambda (xs) return ([%Michelson {| { SIZE } |}])@(xs)[@inline] in
  let #List#size#47 = lambda (xs) return (#List#length#46@{a})@(xs)[@inline] in
  let #List#head_opt#48 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#14 ->
                                                  match ctor_proj#14 with
                                                   | ( x , _#2 ) ->
                                                   SOME(x)
                                               | Nil unit_proj#16 ->
                                                 NONE()[@inline] in
  let #List#tail_opt#49 = lambda (xs) return  match xs with
                                               | Cons ctor_proj#17 ->
                                                  match ctor_proj#17 with
                                                   | ( _#3 , xs ) ->
                                                   SOME(xs)
                                               | Nil unit_proj#19 ->
                                                 NONE()[@inline] in
  let #A#current_turn#50 = lambda (i) return ADD(i , +1) in
  let #A#other#51 = lambda (n) return let current_turn = (#A#current_turn#50)@(+1) in
  ASSERTION(EQ(n ,
  current_turn)) in
  let main = lambda (gen#12) return  match gen#12 with
                                      | ( _p , _s ) ->
                                      ( LIST_EMPTY() , (#A#other#51)@(+2) ) in
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
    let #String#length#41 = fun s -> (({ SIZE })@(s))[@inline] in
    let #String#size#42 = fun s -> (({ SIZE })@(s))[@inline] in
    let #String#sub#43 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #String#slice#44 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #String#concat#45 = fun p -> (({ UNPAIR ; CONCAT })@(p))[@inline] in
    let #Crypto#blake2b#46 = fun b -> (({ BLAKE2B })@(b))[@inline] in
    let #Crypto#sha256#47 = fun b -> (({ SHA256 })@(b))[@inline] in
    let #Crypto#sha512#48 = fun b -> (({ SHA512 })@(b))[@inline] in
    let #Crypto#sha3#49 = fun b -> (({ SHA3 })@(b))[@inline] in
    let #Crypto#keccak#50 = fun b -> (({ KECCAK })@(b))[@inline] in
    let #Crypto#hash_key#51 = fun k -> (({ HASH_KEY })@(k))[@inline] in
    let #Crypto#check#52 =
      fun gen#5 ->
      (let (gen#63, gen#64) = gen#5 in
       let (gen#65, gen#66) = gen#63 in
       let k = gen#65 in
       let s = gen#66 in
       let b = gen#64 in
       ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))[@inline] in
    let #Bytes#concat#53 = fun p -> (({ UNPAIR ; CONCAT })@(p))[@inline] in
    let #Bytes#sub#54 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #Bytes#slice#55 =
      fun sli ->
      (({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
    let #Bytes#length#57 = fun b -> (({ SIZE })@(b))[@inline] in
    let v = PAIR(L(1) , L("b")) in let #A#y#62 = v in let tm = #A#y#62 in L(unit) |}]
