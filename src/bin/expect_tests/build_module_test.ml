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
let #../../test/contracts/build/A.mligo#Bytes#length#140 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#toto#145 = L(1) in
let #../../test/contracts/build/B.mligo#String#length#149 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/B.mligo#String#size#150 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#151 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/B.mligo#String#slice#152 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#153 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/B.mligo#String#concat#154 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#blake2b#155 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha256#156 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha512#157 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha3#158 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#keccak#159 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#hash_key#160 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#check#161 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#concat#162 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#sub#163 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#length#165 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#toto#170 = L(32) in
let #../../test/contracts/build/B.mligo#titi#171 =
  ADD(#../../test/contracts/build/A.mligo#toto#145 , L(42)) in
let #../../test/contracts/build/B.mligo#f#172 =
  fun gen#22 ->
  (let (gen#299, gen#300) = gen#22 in
   let gen#23 = gen#299 in
   let x = gen#300 in
   let x =
     ADD(ADD(x , #../../test/contracts/build/A.mligo#toto#145) ,
         #../../test/contracts/build/B.mligo#titi#171) in
   PAIR(LIST_EMPTY() , x)) in
let #../../test/contracts/build/F.mligo#String#length#176 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/F.mligo#String#size#177 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#178 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/F.mligo#String#slice#179 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#180 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/F.mligo#String#concat#181 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#blake2b#182 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha256#183 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha512#184 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha3#185 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#keccak#186 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#hash_key#187 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#check#188 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#concat#189 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#sub#190 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#length#192 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#toto#197 = L(44) in
let #../../test/contracts/build/G.mligo#String#length#201 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/G.mligo#String#size#202 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#203 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/G.mligo#String#slice#204 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#205 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/G.mligo#String#concat#206 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#blake2b#207 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha256#208 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha512#209 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha3#210 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#keccak#211 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#hash_key#212 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#check#213 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#concat#214 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#sub#215 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#length#217 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#toto#222 = L(43) in
let #../../test/contracts/build/C.mligo#String#length#226 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/C.mligo#String#size#227 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#228 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/C.mligo#String#slice#229 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#230 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/C.mligo#String#concat#231 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#blake2b#232 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha256#233 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha512#234 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha3#235 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#keccak#236 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#hash_key#237 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#check#238 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#concat#239 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#sub#240 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#length#242 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#tata#247 =
  ADD(#../../test/contracts/build/A.mligo#toto#145 ,
      #../../test/contracts/build/B.mligo#titi#171) in
let #../../test/contracts/build/C.mligo#foo#248 =
  (#../../test/contracts/build/B.mligo#f#172)@(PAIR(L(unit) , L(3))) in
let #../../test/contracts/build/E.mligo#String#length#252 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/E.mligo#String#size#253 =
  fun s -> (({ SIZE })@(s))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#254 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/E.mligo#String#slice#255 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#256 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/E.mligo#String#concat#257 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#blake2b#258 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha256#259 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha512#260 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha3#261 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#keccak#262 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#hash_key#263 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#check#264 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#concat#265 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#sub#266 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#length#268 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#toto#273 = L(10) in
let #../../test/contracts/build/E.mligo#foo#274 = L("bar") in
let #String#length#278 = fun s -> (({ SIZE })@(s))[@inline] in
let #String#size#279 = fun s -> (({ SIZE })@(s))[@inline] in
let #String#sub#280 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #String#slice#281 =
  fun sli ->
  (({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(sli))[@inline] in
let #String#sub#282 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #String#concat#283 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #Crypto#blake2b#284 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let #Crypto#sha256#285 = fun b -> (({ SHA256 })@(b))[@inline] in
let #Crypto#sha512#286 = fun b -> (({ SHA512 })@(b))[@inline] in
let #Crypto#sha3#287 = fun b -> (({ SHA3 })@(b))[@inline] in
let #Crypto#keccak#288 = fun b -> (({ KECCAK })@(b))[@inline] in
let #Crypto#hash_key#289 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let #Crypto#check#290 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #Bytes#concat#291 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c))))[@inline] in
let #Bytes#sub#292 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input)))))[@inline] in
let #Bytes#length#294 = fun b -> (({ SIZE })@(b))[@inline] in
let toto =
  ADD(#../../test/contracts/build/E.mligo#toto#273 ,
      #../../test/contracts/build/A.mligo#toto#145) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#75 ->
  (let (gen#301, gen#302) = gen#75 in
   let p = gen#301 in
   let s = gen#302 in
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
