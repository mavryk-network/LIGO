open Cli_expect

let contract basename =
  "../../test/contracts/build/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "cycle_A.mligo" ] ;
  [%expect {|
    `-- 3 -- ../../test/contracts/build/cycle_A.mligo
        `-- 2 -- ../../test/contracts/build/cycle_B.mligo
            `-- 1 -- ../../test/contracts/build/cycle_C.mligo
                `-- 3 -- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "cycle_A.mligo"; "--format" ; "json" ] ;
  [%expect {|
    {
      "root": "../../test/contracts/build/cycle_A.mligo",
      "child": {
        "file": "../../test/contracts/build/cycle_B.mligo",
        "child": {
          "file": "../../test/contracts/build/cycle_C.mligo",
          "child": {
            "file": "../../test/contracts/build/cycle_A.mligo",
            "child": { "file": "../../test/contracts/build/cycle_B.mligo" }
          }
        }
      }
    } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "D.mligo" ] ;
  [%expect {|
    `-- 7 -- ../../test/contracts/build/D.mligo
        |-- 5 -- ../../test/contracts/build/C.mligo
        |   |-- 1 -- ../../test/contracts/build/A.mligo
        |   `-- 2 -- ../../test/contracts/build/B.mligo
        |       `-- 1 -- ../../test/contracts/build/A.mligo
        `-- 6 -- ../../test/contracts/build/E.mligo
            |-- 3 -- ../../test/contracts/build/F.mligo
            `-- 4 -- ../../test/contracts/build/G.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "D.mligo"; "--format" ; "json" ] ;
  [%expect {|
    {
      "root": "../../test/contracts/build/D.mligo",
      "child": {
        "file": "../../test/contracts/build/C.mligo",
        "child": { "file": "../../test/contracts/build/A.mligo" },
        "child": {
          "file": "../../test/contracts/build/B.mligo",
          "child": { "file": "../../test/contracts/build/A.mligo" }
        }
      },
      "child": {
        "file": "../../test/contracts/build/E.mligo",
        "child": { "file": "../../test/contracts/build/F.mligo" },
        "child": { "file": "../../test/contracts/build/G.mligo" }
      }
    } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "B.mligo" ; "-e" ; "f" ] ;
  [%expect{|
    { parameter unit ;
      storage int ;
      code { PUSH int 1 ;
             PUSH int 42 ;
             SWAP ;
             DUP ;
             DUG 2 ;
             ADD ;
             DIG 2 ;
             CDR ;
             SWAP ;
             DUG 2 ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "D.mligo" ] ;
  [%expect {|
    const toto = ADD(E.toto , C.B.A.toto)
    const fb = record[tata -> 2 , tete -> 3 , titi -> 1 , toto -> toto]
    const main =
      lambda (gen#76) return  match gen#76 with
                               | ( p , s ) ->
                               let s = ADD(ADD(p , s) ,
                               toto) in ( LIST_EMPTY() , s ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let #../../test/contracts/build/A.mligo#String#length#126 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/A.mligo#String#size#127 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/A.mligo#String#sub#128 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/A.mligo#String#slice#129 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/A.mligo#String#sub#130 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/A.mligo#String#concat#131 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#blake2b#132 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha256#133 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha512#134 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha3#135 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#keccak#136 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#hash_key#137 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#check#138 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#concat#139 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#sub#140 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#slice#141 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#length#143 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#toto#148 = L(1) in
let #../../test/contracts/build/B.mligo#String#length#154 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/B.mligo#String#size#155 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#156 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/B.mligo#String#slice#157 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#158 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/B.mligo#String#concat#159 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#blake2b#160 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha256#161 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha512#162 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha3#163 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#keccak#164 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#hash_key#165 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#check#166 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#concat#167 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#sub#168 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#slice#169 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#length#171 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#toto#176 = L(32) in
let #../../test/contracts/build/B.mligo#titi#177 =
  ADD(#../../test/contracts/build/A.mligo#toto#148 , L(42)) in
let #../../test/contracts/build/B.mligo#f#178 =
  fun gen#22 ->
  (let (gen#320, gen#321) = gen#22 in
   let gen#23 = gen#320 in
   let x = gen#321 in
   let x =
     ADD(ADD(x , #../../test/contracts/build/A.mligo#toto#148) ,
         #../../test/contracts/build/B.mligo#titi#177) in
   PAIR(LIST_EMPTY() , x)) in
let #../../test/contracts/build/F.mligo#String#length#184 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/F.mligo#String#size#185 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#186 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/F.mligo#String#slice#187 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#188 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/F.mligo#String#concat#189 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#blake2b#190 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha256#191 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha512#192 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha3#193 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#keccak#194 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#hash_key#195 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#check#196 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#concat#197 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#sub#198 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#slice#199 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#length#201 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#toto#206 = L(44) in
let #../../test/contracts/build/G.mligo#String#length#212 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/G.mligo#String#size#213 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#214 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/G.mligo#String#slice#215 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#216 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/G.mligo#String#concat#217 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#blake2b#218 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha256#219 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha512#220 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha3#221 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#keccak#222 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#hash_key#223 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#check#224 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#concat#225 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#sub#226 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#slice#227 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#length#229 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#toto#234 = L(43) in
let #../../test/contracts/build/C.mligo#String#length#240 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/C.mligo#String#size#241 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#242 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/C.mligo#String#slice#243 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#244 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/C.mligo#String#concat#245 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#blake2b#246 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha256#247 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha512#248 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha3#249 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#keccak#250 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#hash_key#251 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#check#252 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#concat#253 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#sub#254 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#slice#255 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#length#257 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#tata#262 =
  ADD(#../../test/contracts/build/A.mligo#toto#148 ,
      #../../test/contracts/build/B.mligo#titi#177) in
let #../../test/contracts/build/C.mligo#foo#263 =
  (#../../test/contracts/build/B.mligo#f#178)@(PAIR(L(unit) , L(3))) in
let #../../test/contracts/build/E.mligo#String#length#269 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/E.mligo#String#size#270 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#271 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/E.mligo#String#slice#272 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#273 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/E.mligo#String#concat#274 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#blake2b#275 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha256#276 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha512#277 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha3#278 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#keccak#279 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#hash_key#280 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#check#281 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#concat#282 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#sub#283 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#slice#284 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#length#286 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#toto#291 = L(10) in
let #../../test/contracts/build/E.mligo#foo#292 = L("bar") in
let #String#length#298 = fun s -> (({ SIZE })@(s))[@inline] in
let #String#size#299 = fun s -> (({ SIZE })@(s))[@inline] in
let #String#sub#300 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #String#slice#301 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #String#sub#302 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #String#concat#303 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #Crypto#blake2b#304 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let #Crypto#sha256#305 = fun b -> (({ SHA256 })@(b))[@inline] in
let #Crypto#sha512#306 = fun b -> (({ SHA512 })@(b))[@inline] in
let #Crypto#sha3#307 = fun b -> (({ SHA3 })@(b))[@inline] in
let #Crypto#keccak#308 = fun b -> (({ KECCAK })@(b))[@inline] in
let #Crypto#hash_key#309 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let #Crypto#check#310 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #Bytes#concat#311 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #Bytes#sub#312 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #Bytes#slice#313 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #Bytes#length#315 = fun b -> (({ SIZE })@(b))[@inline] in
let toto =
  ADD(#../../test/contracts/build/E.mligo#toto#291 ,
      #../../test/contracts/build/A.mligo#toto#148) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#75 ->
  (let (gen#322, gen#323) = gen#75 in
   let p = gen#322 in
   let s = gen#323 in
   let s = ADD(ADD(p , s) , toto) in PAIR(LIST_EMPTY() , s)) in
L(unit) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "D.mligo" ] ;
  [%expect{|
    { parameter int ;
      storage int ;
      code { PUSH int 1 ;
             PUSH int 10 ;
             ADD ;
             SWAP ;
             UNPAIR ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; contract "cycle_A.mligo" ] ;
  [%expect {|
    Dependency cycle detected :
     `-- 3 -- ../../test/contracts/build/cycle_A.mligo
        `-- 2 -- ../../test/contracts/build/cycle_B.mligo
            `-- 1 -- ../../test/contracts/build/cycle_C.mligo
                `-- 3 -- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "type_B.mligo" ] ;
  [%expect {|
    File "../../test/contracts/build/type_B.mligo", line 5, characters 5-6:
      4 | 	let s = s + 1 in
      5 | 	let p = p ^ "titi" in
      6 | 	([] : operation list), s
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    { parameter string ;
      storage int ;
      code { UNPAIR ;
             PUSH int 1 ;
             DIG 2 ;
             ADD ;
             PUSH string "titi" ;
             DIG 2 ;
             CONCAT ;
             DROP ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "tata" ; "--init-file" ; contract "C.mligo" ] ;
  [%expect {| 44 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ;  contract "C1.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; contract "C_test.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "Xmain.mligo" ] ;
  [%expect {|
    { 1 ; 2 ; 3 } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "Xmain.mligo"; "--format" ; "dev" ] ;
  [%expect {|
    `-- 4 -- ../../test/contracts/build/Xmain.mligo
        |-- 3 -- ../../test/contracts/build/Xfoo.mligo
        |   |-- 1 -- ../../test/contracts/build/Xlist.mligo
        |   `-- 2 -- ../../test/contracts/build/Xset.mligo
        `-- 1 -- ../../test/contracts/build/Xlist.mligo |}]

let%expect_test _ =
  run_ligo_bad ["run"; "interpret"; "--init-file"; contract "module_scoping_bug.mligo" ; "x"; ] ;
  [%expect {|
    File "../../test/contracts/build/module_scoping_bug.mligo", line 24, characters 10-11:
     23 |
     24 | let x = B.A.a

    Module "A" not found. |}]
