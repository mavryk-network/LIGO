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
let #../../test/contracts/build/A.mligo#String#length#128 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/A.mligo#String#size#129 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/A.mligo#String#sub#130 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/A.mligo#String#slice#131 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/A.mligo#String#sub#132 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/A.mligo#String#concat#133 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#blake2b#134 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha256#135 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha512#136 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha3#137 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#keccak#138 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#hash_key#139 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#check#140 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#concat#141 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#sub#142 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#slice#143 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#length#145 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#toto#150 = L(1) in
let #../../test/contracts/build/B.mligo#String#length#158 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/B.mligo#String#size#159 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#160 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/B.mligo#String#slice#161 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#162 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/B.mligo#String#concat#163 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#blake2b#164 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha256#165 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha512#166 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha3#167 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#keccak#168 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#hash_key#169 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#check#170 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#concat#171 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#sub#172 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#slice#173 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#length#175 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#toto#180 = L(32) in
let #../../test/contracts/build/B.mligo#titi#181 =
  ADD(#../../test/contracts/build/A.mligo#toto#150 , L(42)) in
let #../../test/contracts/build/B.mligo#f#182 =
  fun gen#22 ->
  (let (gen#334, gen#335) = gen#22 in
   let gen#23 = gen#334 in
   let x = gen#335 in
   let x =
     ADD(ADD(x , #../../test/contracts/build/A.mligo#toto#150) ,
         #../../test/contracts/build/B.mligo#titi#181) in
   PAIR(LIST_EMPTY() , x)) in
let #../../test/contracts/build/F.mligo#String#length#190 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/F.mligo#String#size#191 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#192 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/F.mligo#String#slice#193 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#194 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/F.mligo#String#concat#195 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#blake2b#196 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha256#197 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha512#198 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha3#199 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#keccak#200 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#hash_key#201 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#check#202 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#concat#203 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#sub#204 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#slice#205 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#length#207 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#toto#212 = L(44) in
let #../../test/contracts/build/G.mligo#String#length#220 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/G.mligo#String#size#221 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#222 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/G.mligo#String#slice#223 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#224 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/G.mligo#String#concat#225 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#blake2b#226 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha256#227 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha512#228 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha3#229 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#keccak#230 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#hash_key#231 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#check#232 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#concat#233 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#sub#234 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#slice#235 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#length#237 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#toto#242 = L(43) in
let #../../test/contracts/build/C.mligo#String#length#250 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/C.mligo#String#size#251 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#252 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/C.mligo#String#slice#253 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#254 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/C.mligo#String#concat#255 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#blake2b#256 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha256#257 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha512#258 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha3#259 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#keccak#260 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#hash_key#261 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#check#262 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#concat#263 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#sub#264 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#slice#265 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#length#267 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#tata#272 =
  ADD(#../../test/contracts/build/A.mligo#toto#150 ,
      #../../test/contracts/build/B.mligo#titi#181) in
let #../../test/contracts/build/C.mligo#foo#273 =
  (#../../test/contracts/build/B.mligo#f#182)@(PAIR(L(unit) , L(3))) in
let #../../test/contracts/build/E.mligo#String#length#281 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/E.mligo#String#size#282 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#283 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/E.mligo#String#slice#284 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#285 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/E.mligo#String#concat#286 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#blake2b#287 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha256#288 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha512#289 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha3#290 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#keccak#291 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#hash_key#292 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#check#293 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#concat#294 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#sub#295 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#slice#296 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#length#298 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#toto#303 = L(10) in
let #../../test/contracts/build/E.mligo#foo#304 = L("bar") in
let #String#length#312 = fun s -> (({ SIZE })@(s))[@inline] in
let #String#size#313 = fun s -> (({ SIZE })@(s))[@inline] in
let #String#sub#314 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #String#slice#315 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #String#sub#316 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #String#concat#317 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #Crypto#blake2b#318 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let #Crypto#sha256#319 = fun b -> (({ SHA256 })@(b))[@inline] in
let #Crypto#sha512#320 = fun b -> (({ SHA512 })@(b))[@inline] in
let #Crypto#sha3#321 = fun b -> (({ SHA3 })@(b))[@inline] in
let #Crypto#keccak#322 = fun b -> (({ KECCAK })@(b))[@inline] in
let #Crypto#hash_key#323 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let #Crypto#check#324 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #Bytes#concat#325 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #Bytes#sub#326 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #Bytes#slice#327 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #Bytes#length#329 = fun b -> (({ SIZE })@(b))[@inline] in
let toto =
  ADD(#../../test/contracts/build/E.mligo#toto#303 ,
      #../../test/contracts/build/A.mligo#toto#150) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#75 ->
  (let (gen#336, gen#337) = gen#75 in
   let p = gen#336 in
   let s = gen#337 in
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
