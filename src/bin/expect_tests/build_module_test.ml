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
let #../../test/contracts/build/A.mligo#String#length#124 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/A.mligo#String#size#125 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/A.mligo#String#sub#126 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/A.mligo#String#slice#127 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/A.mligo#String#sub#128 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/A.mligo#String#concat#129 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#blake2b#130 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha256#131 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha512#132 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha3#133 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#keccak#134 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#hash_key#135 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#check#136 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#concat#137 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#sub#138 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#slice#139 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#length#141 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#toto#146 = L(1) in
let #../../test/contracts/build/B.mligo#String#length#150 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/B.mligo#String#size#151 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#152 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/B.mligo#String#slice#153 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#154 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/B.mligo#String#concat#155 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#blake2b#156 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha256#157 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha512#158 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha3#159 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#keccak#160 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#hash_key#161 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#check#162 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#concat#163 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#sub#164 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#slice#165 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#length#167 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#toto#172 = L(32) in
let #../../test/contracts/build/B.mligo#titi#173 =
  ADD(#../../test/contracts/build/A.mligo#toto#146 , L(42)) in
let #../../test/contracts/build/B.mligo#f#174 =
  fun gen#22 ->
  (let (gen#306, gen#307) = gen#22 in
   let gen#23 = gen#306 in
   let x = gen#307 in
   let x =
     ADD(ADD(x , #../../test/contracts/build/A.mligo#toto#146) ,
         #../../test/contracts/build/B.mligo#titi#173) in
   PAIR(LIST_EMPTY() , x)) in
let #../../test/contracts/build/F.mligo#String#length#178 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/F.mligo#String#size#179 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#180 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/F.mligo#String#slice#181 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#182 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/F.mligo#String#concat#183 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#blake2b#184 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha256#185 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha512#186 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha3#187 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#keccak#188 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#hash_key#189 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#check#190 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#concat#191 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#sub#192 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#slice#193 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#length#195 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#toto#200 = L(44) in
let #../../test/contracts/build/G.mligo#String#length#204 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/G.mligo#String#size#205 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#206 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/G.mligo#String#slice#207 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#208 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/G.mligo#String#concat#209 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#blake2b#210 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha256#211 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha512#212 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha3#213 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#keccak#214 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#hash_key#215 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#check#216 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#concat#217 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#sub#218 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#slice#219 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#length#221 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#toto#226 = L(43) in
let #../../test/contracts/build/C.mligo#String#length#230 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/C.mligo#String#size#231 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#232 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/C.mligo#String#slice#233 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#234 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/C.mligo#String#concat#235 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#blake2b#236 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha256#237 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha512#238 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha3#239 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#keccak#240 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#hash_key#241 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#check#242 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#concat#243 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#sub#244 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#slice#245 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#length#247 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#tata#252 =
  ADD(#../../test/contracts/build/A.mligo#toto#146 ,
      #../../test/contracts/build/B.mligo#titi#173) in
let #../../test/contracts/build/C.mligo#foo#253 =
  (#../../test/contracts/build/B.mligo#f#174)@(PAIR(L(unit) , L(3))) in
let #../../test/contracts/build/E.mligo#String#length#257 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/E.mligo#String#size#258 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#259 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/E.mligo#String#slice#260 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#261 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/E.mligo#String#concat#262 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#blake2b#263 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha256#264 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha512#265 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha3#266 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#keccak#267 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#hash_key#268 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#check#269 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#concat#270 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#sub#271 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#slice#272 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#length#274 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#toto#279 = L(10) in
let #../../test/contracts/build/E.mligo#foo#280 = L("bar") in
let #String#length#284 = fun s -> (({ SIZE })@(s))[@inline] in
let #String#size#285 = fun s -> (({ SIZE })@(s))[@inline] in
let #String#sub#286 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #String#slice#287 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #String#sub#288 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #String#concat#289 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #Crypto#blake2b#290 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let #Crypto#sha256#291 = fun b -> (({ SHA256 })@(b))[@inline] in
let #Crypto#sha512#292 = fun b -> (({ SHA512 })@(b))[@inline] in
let #Crypto#sha3#293 = fun b -> (({ SHA3 })@(b))[@inline] in
let #Crypto#keccak#294 = fun b -> (({ KECCAK })@(b))[@inline] in
let #Crypto#hash_key#295 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let #Crypto#check#296 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #Bytes#concat#297 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #Bytes#sub#298 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #Bytes#slice#299 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #Bytes#length#301 = fun b -> (({ SIZE })@(b))[@inline] in
let toto =
  ADD(#../../test/contracts/build/E.mligo#toto#279 ,
      #../../test/contracts/build/A.mligo#toto#146) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#75 ->
  (let (gen#308, gen#309) = gen#75 in
   let p = gen#308 in
   let s = gen#309 in
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
