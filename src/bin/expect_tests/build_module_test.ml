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
let #../../test/contracts/build/A.mligo#String#length#121 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/A.mligo#String#size#122 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/A.mligo#String#sub#123 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/A.mligo#String#slice#124 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/A.mligo#String#sub#125 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/A.mligo#String#concat#126 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#blake2b#127 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha256#128 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha512#129 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha3#130 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#keccak#131 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#hash_key#132 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#check#133 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#concat#134 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#sub#135 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#length#137 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#toto#142 = L(1) in
let #../../test/contracts/build/B.mligo#String#length#143 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/B.mligo#String#size#144 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#145 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/B.mligo#String#slice#146 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#147 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/B.mligo#String#concat#148 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#blake2b#149 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha256#150 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha512#151 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha3#152 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#keccak#153 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#hash_key#154 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#check#155 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#concat#156 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#sub#157 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#length#159 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#toto#164 = L(32) in
let #../../test/contracts/build/B.mligo#titi#165 =
  ADD(#../../test/contracts/build/A.mligo#toto#142 , L(42)) in
let #../../test/contracts/build/B.mligo#f#166 =
  fun gen#22 ->
  (let (gen#278, gen#279) = gen#22 in
   let gen#23 = gen#278 in
   let x = gen#279 in
   let x =
     ADD(ADD(x , #../../test/contracts/build/A.mligo#toto#142) ,
         #../../test/contracts/build/B.mligo#titi#165) in
   PAIR(LIST_EMPTY() , x)) in
let #../../test/contracts/build/F.mligo#String#length#167 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/F.mligo#String#size#168 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#169 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/F.mligo#String#slice#170 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#171 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/F.mligo#String#concat#172 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#blake2b#173 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha256#174 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha512#175 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha3#176 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#keccak#177 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#hash_key#178 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#check#179 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#concat#180 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#sub#181 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#length#183 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#toto#188 = L(44) in
let #../../test/contracts/build/G.mligo#String#length#189 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/G.mligo#String#size#190 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#191 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/G.mligo#String#slice#192 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#193 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/G.mligo#String#concat#194 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#blake2b#195 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha256#196 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha512#197 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha3#198 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#keccak#199 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#hash_key#200 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#check#201 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#concat#202 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#sub#203 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#length#205 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#toto#210 = L(43) in
let #../../test/contracts/build/C.mligo#String#length#211 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/C.mligo#String#size#212 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#213 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/C.mligo#String#slice#214 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#215 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/C.mligo#String#concat#216 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#blake2b#217 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha256#218 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha512#219 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha3#220 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#keccak#221 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#hash_key#222 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#check#223 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#concat#224 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#sub#225 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#length#227 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#tata#232 =
  ADD(#../../test/contracts/build/A.mligo#toto#142 ,
      #../../test/contracts/build/B.mligo#titi#165) in
let #../../test/contracts/build/C.mligo#foo#233 =
  (#../../test/contracts/build/B.mligo#f#166)@(PAIR(L(unit) , L(3))) in
let #../../test/contracts/build/E.mligo#String#length#234 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/E.mligo#String#size#235 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#236 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/E.mligo#String#slice#237 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#238 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/E.mligo#String#concat#239 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#blake2b#240 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha256#241 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha512#242 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha3#243 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#keccak#244 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#hash_key#245 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#check#246 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#concat#247 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#sub#248 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#length#250 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#toto#255 = L(10) in
let #../../test/contracts/build/E.mligo#foo#256 = L("bar") in
let #String#length#257 = fun s -> (({ SIZE })@(s))[@inline] in
let #String#size#258 = fun s -> (({ SIZE })@(s))[@inline] in
let #String#sub#259 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #String#slice#260 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #String#sub#261 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #String#concat#262 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #Crypto#blake2b#263 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let #Crypto#sha256#264 = fun b -> (({ SHA256 })@(b))[@inline] in
let #Crypto#sha512#265 = fun b -> (({ SHA512 })@(b))[@inline] in
let #Crypto#sha3#266 = fun b -> (({ SHA3 })@(b))[@inline] in
let #Crypto#keccak#267 = fun b -> (({ KECCAK })@(b))[@inline] in
let #Crypto#hash_key#268 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let #Crypto#check#269 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #Bytes#concat#270 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #Bytes#sub#271 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #Bytes#length#273 = fun b -> (({ SIZE })@(b))[@inline] in
let toto =
  ADD(#../../test/contracts/build/E.mligo#toto#255 ,
      #../../test/contracts/build/A.mligo#toto#142) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#75 ->
  (let (gen#280, gen#281) = gen#75 in
   let p = gen#280 in
   let s = gen#281 in
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
      code { CDR ; PUSH int 1 ; ADD ; NIL operation ; PAIR } } |}]

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
