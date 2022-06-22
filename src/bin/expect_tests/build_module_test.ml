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
             DUP 2 ;
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
      lambda (gen#5 : ( int * int )) return  match gen#5 with
                                              | ( p , s ) ->
                                              let s = ADD(ADD(p , s) ,
                                              toto) in ( LIST_EMPTY() , s ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let ballec#94 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let ballec#95 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let ballec#96 = ({ DROP ; NOW })@(L(unit))[@inline] in
let ballec#97 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let ballec#98 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let ballec#99 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let ballec#100 = SELF_ADDRESS()[@inline] in
let ballec#101 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let ballec#102 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let ballec#103 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let ballec#104 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let ballec#105 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let ballec#106 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let ballec#107 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let ballec#108 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let ballec#109 = fun _u -> (SELF_ADDRESS())[@inline] in
let ballec#110 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let ballec#111 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let ballec#112 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let ballec#113 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let ballec#114 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let ballec#116 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let ballec#122 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let ballec#123 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let ballec#127 = fun o -> (SET_DELEGATE(o))[@inline] in
let ballec#128 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let ballec#129 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let ballec#130 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let ballec#171 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#172 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#173 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#176 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#177 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#180 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#181 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let ballec#182 = fun b -> (({ SHA256 })@(b))[@inline] in
let ballec#183 = fun b -> (({ SHA512 })@(b))[@inline] in
let ballec#184 = fun b -> (({ SHA3 })@(b))[@inline] in
let ballec#185 = fun b -> (({ KECCAK })@(b))[@inline] in
let ballec#186 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let ballec#187 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let ballec#188 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let ballec#189 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let ballec#194 = fun i -> (({ ABS })@(i))[@inline] in
let ballec#195 = fun i -> (({ ISNAT })@(i))[@inline] in
let ballec#196 = TRUE()[@inline] in
let ballec#197 = FALSE()[@inline] in
let ballec#198 = UNIT()[@inline] in
let poly_ballec_105 = { FAILWITH }[@inline] in
let poly_ballec_104 = { FAILWITH }[@inline] in
let poly_ballec_103 = { FAILWITH }[@inline] in
let poly_ballec_102 = { FAILWITH }[@inline] in
let poly_ballec_101 = { FAILWITH }[@inline] in
let poly_ballec_100 = { FAILWITH }[@inline] in
let poly_ballec_99 = { FAILWITH }[@inline] in
let poly_ballec_98 = { FAILWITH }[@inline] in
let poly_ballec_97 = { FAILWITH }[@inline] in
let poly_ballec_96 = { FAILWITH }[@inline] in
let poly_ballec_95 = { FAILWITH }[@inline] in
let poly_ballec_94 = { FAILWITH }[@inline] in
let poly_ballec_93 = { FAILWITH }[@inline] in
let poly_ballec_92 = { FAILWITH }[@inline] in
let poly_ballec_91 = { FAILWITH }[@inline] in
let ballec#204 =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_ballec_105)@(L("TEST MODE")))))))[@inline] in
let ballec#206 = fun _a -> ((poly_ballec_95)@(L("TEST MODE")))[@inline] in
let ballec#207 = fun _a -> ((poly_ballec_95)@(L("TEST MODE")))[@inline] in
let ballec#208 = fun _bp -> ((poly_ballec_95)@(L("TEST MODE")))[@inline] in
let ballec#209 =
  fun _a -> (fun _s -> (fun _t -> ((poly_ballec_95)@(L("TEST MODE")))))[@inline] in
let ballec#210 =
  fun _a -> (fun _s -> (fun _t -> ((poly_ballec_102)@(L("TEST MODE")))))[@inline] in
let ballec#214 = fun _a -> ((poly_ballec_95)@(L("TEST MODE")))[@inline] in
let ballec#215 = fun _a -> ((poly_ballec_104)@(L("TEST MODE")))[@inline] in
let ballec#216 =
  fun _m1 -> (fun _m2 -> ((poly_ballec_103)@(L("TEST MODE"))))[@inline] in
let ballec#218 =
  fun _n -> (fun _l -> ((poly_ballec_95)@(L("TEST MODE"))))[@inline] in
let ballec#219 =
  fun _t -> (fun _n -> (fun _l -> ((poly_ballec_95)@(L("TEST MODE")))))[@inline] in
let ballec#220 = fun _kh -> ((poly_ballec_102)@(L("TEST MODE")))[@inline] in
let ballec#221 = (poly_ballec_102)@(L("TEST MODE"))[@inline] in
let ballec#223 = fun _i -> ((poly_ballec_94)@(L("TEST MODE")))[@inline] in
let ballec#224 = fun _i -> ((poly_ballec_94)@(L("TEST MODE")))[@inline] in
let ballec#226 = fun _u -> ((poly_ballec_101)@(L("TEST MODE")))[@inline] in
let ballec#229 =
  fun _s -> (fun _m -> ((poly_ballec_100)@(L("TEST MODE"))))[@inline] in
let ballec#236 =
  fun _s -> (fun _k -> ((poly_ballec_95)@(L("TEST MODE"))))[@inline] in
let ballec#237 = fun _u -> ((poly_ballec_99)@(L("TEST MODE")))[@inline] in
let ballec#238 =
  fun _p -> (fun _o -> ((poly_ballec_95)@(L("TEST MODE"))))[@inline] in
let ballec#239 = fun _n -> ((poly_ballec_95)@(L("TEST MODE")))[@inline] in
let ballec#240 = fun _kh -> ((poly_ballec_95)@(L("TEST MODE")))[@inline] in
let ballec#241 = fun _m -> ((poly_ballec_98)@(L("TEST MODE")))[@inline] in
let ballec#246 =
  fun _b -> (fun _n -> ((poly_ballec_97)@(L("TEST MODE"))))[@inline] in
let ballec#247 =
  fun _c -> (fun _n -> ((poly_ballec_96)@(L("TEST MODE"))))[@inline] in
let ballec#248 = fun _s -> ((poly_ballec_95)@(L("TEST MODE")))[@inline] in
let ballec#249 =
  fun _u -> ((poly_ballec_95)@(L("TEST_POP_CONTEXT")))[@inline] in
let ballec#250 =
  fun _u -> ((poly_ballec_95)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let ballec#251 =
  fun _u -> ((poly_ballec_95)@(L("TEST_DROP_CONTEXT")))[@inline] in
let ballec#252 =
  fun _fn -> ((poly_ballec_95)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let ballec#253 =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_ballec_95)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let ballec#255 =
  fun _c -> (fun _s -> (fun _t -> ((poly_ballec_94)@(L("TEST_ORIGINATE")))))[@inline] in
let ballec#256 = fun _c -> ((poly_ballec_93)@(L("TEST_SIZE")))[@inline] in
let ballec#257 =
  fun _n -> ((poly_ballec_92)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let ballec#258 =
  fun _sk -> (fun _d -> ((poly_ballec_91)@(L("TEST_SIGN"))))[@inline] in
let ballec#262 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let ballec#263 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let ballec#264 = ({ DROP ; NOW })@(L(unit))[@inline] in
let ballec#265 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let ballec#266 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let ballec#267 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let ballec#268 = SELF_ADDRESS()[@inline] in
let ballec#269 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let ballec#270 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let ballec#271 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let ballec#272 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let ballec#273 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let ballec#274 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let ballec#275 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let ballec#276 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let ballec#277 = fun _u -> (SELF_ADDRESS())[@inline] in
let ballec#278 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let ballec#279 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let ballec#280 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let ballec#282 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let ballec#288 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let ballec#289 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let ballec#293 = fun o -> (SET_DELEGATE(o))[@inline] in
let ballec#294 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let ballec#295 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let ballec#296 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let ballec#337 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#338 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#339 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#342 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#343 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#346 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#347 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let ballec#348 = fun b -> (({ SHA256 })@(b))[@inline] in
let ballec#349 = fun b -> (({ SHA512 })@(b))[@inline] in
let ballec#350 = fun b -> (({ SHA3 })@(b))[@inline] in
let ballec#351 = fun b -> (({ KECCAK })@(b))[@inline] in
let ballec#352 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let ballec#353 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let ballec#354 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let ballec#355 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let ballec#360 = fun i -> (({ ABS })@(i))[@inline] in
let ballec#361 = fun i -> (({ ISNAT })@(i))[@inline] in
let ballec#362 = TRUE()[@inline] in
let ballec#363 = FALSE()[@inline] in
let ballec#364 = UNIT()[@inline] in
let ballec#368 = L(1) in
let ballec#372 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let ballec#373 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let ballec#374 = ({ DROP ; NOW })@(L(unit))[@inline] in
let ballec#375 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let ballec#376 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let ballec#377 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let ballec#378 = SELF_ADDRESS()[@inline] in
let ballec#379 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let ballec#380 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let ballec#381 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let ballec#382 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let ballec#383 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let ballec#384 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let ballec#385 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let ballec#386 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let ballec#387 = fun _u -> (SELF_ADDRESS())[@inline] in
let ballec#388 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let ballec#389 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let ballec#390 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let ballec#391 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let ballec#392 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let ballec#394 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let ballec#400 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let ballec#401 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let ballec#405 = fun o -> (SET_DELEGATE(o))[@inline] in
let ballec#406 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let ballec#407 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let ballec#408 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let ballec#449 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#450 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#451 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#454 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#455 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#458 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#459 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let ballec#460 = fun b -> (({ SHA256 })@(b))[@inline] in
let ballec#461 = fun b -> (({ SHA512 })@(b))[@inline] in
let ballec#462 = fun b -> (({ SHA3 })@(b))[@inline] in
let ballec#463 = fun b -> (({ KECCAK })@(b))[@inline] in
let ballec#464 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let ballec#465 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let ballec#466 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let ballec#467 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let ballec#472 = fun i -> (({ ABS })@(i))[@inline] in
let ballec#473 = fun i -> (({ ISNAT })@(i))[@inline] in
let ballec#474 = TRUE()[@inline] in
let ballec#475 = FALSE()[@inline] in
let ballec#476 = UNIT()[@inline] in
let poly_ballec_90 = { FAILWITH }[@inline] in
let poly_ballec_89 = { FAILWITH }[@inline] in
let poly_ballec_88 = { FAILWITH }[@inline] in
let poly_ballec_87 = { FAILWITH }[@inline] in
let poly_ballec_86 = { FAILWITH }[@inline] in
let poly_ballec_85 = { FAILWITH }[@inline] in
let poly_ballec_84 = { FAILWITH }[@inline] in
let poly_ballec_83 = { FAILWITH }[@inline] in
let poly_ballec_82 = { FAILWITH }[@inline] in
let poly_ballec_81 = { FAILWITH }[@inline] in
let poly_ballec_80 = { FAILWITH }[@inline] in
let poly_ballec_79 = { FAILWITH }[@inline] in
let poly_ballec_78 = { FAILWITH }[@inline] in
let poly_ballec_77 = { FAILWITH }[@inline] in
let poly_ballec_76 = { FAILWITH }[@inline] in
let ballec#482 =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_ballec_90)@(L("TEST MODE")))))))[@inline] in
let ballec#484 = fun _a -> ((poly_ballec_80)@(L("TEST MODE")))[@inline] in
let ballec#485 = fun _a -> ((poly_ballec_80)@(L("TEST MODE")))[@inline] in
let ballec#486 = fun _bp -> ((poly_ballec_80)@(L("TEST MODE")))[@inline] in
let ballec#487 =
  fun _a -> (fun _s -> (fun _t -> ((poly_ballec_80)@(L("TEST MODE")))))[@inline] in
let ballec#488 =
  fun _a -> (fun _s -> (fun _t -> ((poly_ballec_87)@(L("TEST MODE")))))[@inline] in
let ballec#492 = fun _a -> ((poly_ballec_80)@(L("TEST MODE")))[@inline] in
let ballec#493 = fun _a -> ((poly_ballec_89)@(L("TEST MODE")))[@inline] in
let ballec#494 =
  fun _m1 -> (fun _m2 -> ((poly_ballec_88)@(L("TEST MODE"))))[@inline] in
let ballec#496 =
  fun _n -> (fun _l -> ((poly_ballec_80)@(L("TEST MODE"))))[@inline] in
let ballec#497 =
  fun _t -> (fun _n -> (fun _l -> ((poly_ballec_80)@(L("TEST MODE")))))[@inline] in
let ballec#498 = fun _kh -> ((poly_ballec_87)@(L("TEST MODE")))[@inline] in
let ballec#499 = (poly_ballec_87)@(L("TEST MODE"))[@inline] in
let ballec#501 = fun _i -> ((poly_ballec_79)@(L("TEST MODE")))[@inline] in
let ballec#502 = fun _i -> ((poly_ballec_79)@(L("TEST MODE")))[@inline] in
let ballec#504 = fun _u -> ((poly_ballec_86)@(L("TEST MODE")))[@inline] in
let ballec#507 =
  fun _s -> (fun _m -> ((poly_ballec_85)@(L("TEST MODE"))))[@inline] in
let ballec#514 =
  fun _s -> (fun _k -> ((poly_ballec_80)@(L("TEST MODE"))))[@inline] in
let ballec#515 = fun _u -> ((poly_ballec_84)@(L("TEST MODE")))[@inline] in
let ballec#516 =
  fun _p -> (fun _o -> ((poly_ballec_80)@(L("TEST MODE"))))[@inline] in
let ballec#517 = fun _n -> ((poly_ballec_80)@(L("TEST MODE")))[@inline] in
let ballec#518 = fun _kh -> ((poly_ballec_80)@(L("TEST MODE")))[@inline] in
let ballec#519 = fun _m -> ((poly_ballec_83)@(L("TEST MODE")))[@inline] in
let ballec#524 =
  fun _b -> (fun _n -> ((poly_ballec_82)@(L("TEST MODE"))))[@inline] in
let ballec#525 =
  fun _c -> (fun _n -> ((poly_ballec_81)@(L("TEST MODE"))))[@inline] in
let ballec#526 = fun _s -> ((poly_ballec_80)@(L("TEST MODE")))[@inline] in
let ballec#527 =
  fun _u -> ((poly_ballec_80)@(L("TEST_POP_CONTEXT")))[@inline] in
let ballec#528 =
  fun _u -> ((poly_ballec_80)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let ballec#529 =
  fun _u -> ((poly_ballec_80)@(L("TEST_DROP_CONTEXT")))[@inline] in
let ballec#530 =
  fun _fn -> ((poly_ballec_80)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let ballec#531 =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_ballec_80)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let ballec#533 =
  fun _c -> (fun _s -> (fun _t -> ((poly_ballec_79)@(L("TEST_ORIGINATE")))))[@inline] in
let ballec#534 = fun _c -> ((poly_ballec_78)@(L("TEST_SIZE")))[@inline] in
let ballec#535 =
  fun _n -> ((poly_ballec_77)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let ballec#536 =
  fun _sk -> (fun _d -> ((poly_ballec_76)@(L("TEST_SIGN"))))[@inline] in
let ballec#540 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let ballec#541 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let ballec#542 = ({ DROP ; NOW })@(L(unit))[@inline] in
let ballec#543 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let ballec#544 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let ballec#545 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let ballec#546 = SELF_ADDRESS()[@inline] in
let ballec#547 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let ballec#548 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let ballec#549 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let ballec#550 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let ballec#551 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let ballec#552 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let ballec#553 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let ballec#554 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let ballec#555 = fun _u -> (SELF_ADDRESS())[@inline] in
let ballec#556 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let ballec#557 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let ballec#558 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let ballec#560 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let ballec#566 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let ballec#567 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let ballec#571 = fun o -> (SET_DELEGATE(o))[@inline] in
let ballec#572 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let ballec#573 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let ballec#574 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let ballec#615 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#616 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#617 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#620 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#621 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#624 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#625 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let ballec#626 = fun b -> (({ SHA256 })@(b))[@inline] in
let ballec#627 = fun b -> (({ SHA512 })@(b))[@inline] in
let ballec#628 = fun b -> (({ SHA3 })@(b))[@inline] in
let ballec#629 = fun b -> (({ KECCAK })@(b))[@inline] in
let ballec#630 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let ballec#631 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let ballec#632 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let ballec#633 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let ballec#638 = fun i -> (({ ABS })@(i))[@inline] in
let ballec#639 = fun i -> (({ ISNAT })@(i))[@inline] in
let ballec#640 = TRUE()[@inline] in
let ballec#641 = FALSE()[@inline] in
let ballec#642 = UNIT()[@inline] in
let ballec#646 = L(32) in
let ballec#647 = ADD(ballec#368 , L(42)) in
let ballec#648 =
  fun gen#3428 ->
  (let (gen#6975, gen#6976) = gen#3428 in
   let gen#3429 = gen#6975 in
   let x = gen#6976 in
   let x = ADD(ADD(x , ballec#368) , ballec#647) in PAIR(LIST_EMPTY() , x)) in
let ballec#652 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let ballec#653 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let ballec#654 = ({ DROP ; NOW })@(L(unit))[@inline] in
let ballec#655 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let ballec#656 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let ballec#657 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let ballec#658 = SELF_ADDRESS()[@inline] in
let ballec#659 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let ballec#660 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let ballec#661 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let ballec#662 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let ballec#663 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let ballec#664 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let ballec#665 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let ballec#666 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let ballec#667 = fun _u -> (SELF_ADDRESS())[@inline] in
let ballec#668 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let ballec#669 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let ballec#670 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let ballec#671 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let ballec#672 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let ballec#674 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let ballec#680 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let ballec#681 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let ballec#685 = fun o -> (SET_DELEGATE(o))[@inline] in
let ballec#686 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let ballec#687 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let ballec#688 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let ballec#729 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#730 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#731 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#734 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#735 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#738 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#739 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let ballec#740 = fun b -> (({ SHA256 })@(b))[@inline] in
let ballec#741 = fun b -> (({ SHA512 })@(b))[@inline] in
let ballec#742 = fun b -> (({ SHA3 })@(b))[@inline] in
let ballec#743 = fun b -> (({ KECCAK })@(b))[@inline] in
let ballec#744 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let ballec#745 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let ballec#746 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let ballec#747 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let ballec#752 = fun i -> (({ ABS })@(i))[@inline] in
let ballec#753 = fun i -> (({ ISNAT })@(i))[@inline] in
let ballec#754 = TRUE()[@inline] in
let ballec#755 = FALSE()[@inline] in
let ballec#756 = UNIT()[@inline] in
let poly_ballec_75 = { FAILWITH }[@inline] in
let poly_ballec_74 = { FAILWITH }[@inline] in
let poly_ballec_73 = { FAILWITH }[@inline] in
let poly_ballec_72 = { FAILWITH }[@inline] in
let poly_ballec_71 = { FAILWITH }[@inline] in
let poly_ballec_70 = { FAILWITH }[@inline] in
let poly_ballec_69 = { FAILWITH }[@inline] in
let poly_ballec_68 = { FAILWITH }[@inline] in
let poly_ballec_67 = { FAILWITH }[@inline] in
let poly_ballec_66 = { FAILWITH }[@inline] in
let poly_ballec_65 = { FAILWITH }[@inline] in
let poly_ballec_64 = { FAILWITH }[@inline] in
let poly_ballec_63 = { FAILWITH }[@inline] in
let poly_ballec_62 = { FAILWITH }[@inline] in
let poly_ballec_61 = { FAILWITH }[@inline] in
let ballec#762 =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_ballec_75)@(L("TEST MODE")))))))[@inline] in
let ballec#764 = fun _a -> ((poly_ballec_65)@(L("TEST MODE")))[@inline] in
let ballec#765 = fun _a -> ((poly_ballec_65)@(L("TEST MODE")))[@inline] in
let ballec#766 = fun _bp -> ((poly_ballec_65)@(L("TEST MODE")))[@inline] in
let ballec#767 =
  fun _a -> (fun _s -> (fun _t -> ((poly_ballec_65)@(L("TEST MODE")))))[@inline] in
let ballec#768 =
  fun _a -> (fun _s -> (fun _t -> ((poly_ballec_72)@(L("TEST MODE")))))[@inline] in
let ballec#772 = fun _a -> ((poly_ballec_65)@(L("TEST MODE")))[@inline] in
let ballec#773 = fun _a -> ((poly_ballec_74)@(L("TEST MODE")))[@inline] in
let ballec#774 =
  fun _m1 -> (fun _m2 -> ((poly_ballec_73)@(L("TEST MODE"))))[@inline] in
let ballec#776 =
  fun _n -> (fun _l -> ((poly_ballec_65)@(L("TEST MODE"))))[@inline] in
let ballec#777 =
  fun _t -> (fun _n -> (fun _l -> ((poly_ballec_65)@(L("TEST MODE")))))[@inline] in
let ballec#778 = fun _kh -> ((poly_ballec_72)@(L("TEST MODE")))[@inline] in
let ballec#779 = (poly_ballec_72)@(L("TEST MODE"))[@inline] in
let ballec#781 = fun _i -> ((poly_ballec_64)@(L("TEST MODE")))[@inline] in
let ballec#782 = fun _i -> ((poly_ballec_64)@(L("TEST MODE")))[@inline] in
let ballec#784 = fun _u -> ((poly_ballec_71)@(L("TEST MODE")))[@inline] in
let ballec#787 =
  fun _s -> (fun _m -> ((poly_ballec_70)@(L("TEST MODE"))))[@inline] in
let ballec#794 =
  fun _s -> (fun _k -> ((poly_ballec_65)@(L("TEST MODE"))))[@inline] in
let ballec#795 = fun _u -> ((poly_ballec_69)@(L("TEST MODE")))[@inline] in
let ballec#796 =
  fun _p -> (fun _o -> ((poly_ballec_65)@(L("TEST MODE"))))[@inline] in
let ballec#797 = fun _n -> ((poly_ballec_65)@(L("TEST MODE")))[@inline] in
let ballec#798 = fun _kh -> ((poly_ballec_65)@(L("TEST MODE")))[@inline] in
let ballec#799 = fun _m -> ((poly_ballec_68)@(L("TEST MODE")))[@inline] in
let ballec#804 =
  fun _b -> (fun _n -> ((poly_ballec_67)@(L("TEST MODE"))))[@inline] in
let ballec#805 =
  fun _c -> (fun _n -> ((poly_ballec_66)@(L("TEST MODE"))))[@inline] in
let ballec#806 = fun _s -> ((poly_ballec_65)@(L("TEST MODE")))[@inline] in
let ballec#807 =
  fun _u -> ((poly_ballec_65)@(L("TEST_POP_CONTEXT")))[@inline] in
let ballec#808 =
  fun _u -> ((poly_ballec_65)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let ballec#809 =
  fun _u -> ((poly_ballec_65)@(L("TEST_DROP_CONTEXT")))[@inline] in
let ballec#810 =
  fun _fn -> ((poly_ballec_65)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let ballec#811 =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_ballec_65)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let ballec#813 =
  fun _c -> (fun _s -> (fun _t -> ((poly_ballec_64)@(L("TEST_ORIGINATE")))))[@inline] in
let ballec#814 = fun _c -> ((poly_ballec_63)@(L("TEST_SIZE")))[@inline] in
let ballec#815 =
  fun _n -> ((poly_ballec_62)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let ballec#816 =
  fun _sk -> (fun _d -> ((poly_ballec_61)@(L("TEST_SIGN"))))[@inline] in
let ballec#820 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let ballec#821 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let ballec#822 = ({ DROP ; NOW })@(L(unit))[@inline] in
let ballec#823 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let ballec#824 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let ballec#825 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let ballec#826 = SELF_ADDRESS()[@inline] in
let ballec#827 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let ballec#828 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let ballec#829 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let ballec#830 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let ballec#831 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let ballec#832 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let ballec#833 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let ballec#834 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let ballec#835 = fun _u -> (SELF_ADDRESS())[@inline] in
let ballec#836 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let ballec#837 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let ballec#838 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let ballec#840 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let ballec#846 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let ballec#847 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let ballec#851 = fun o -> (SET_DELEGATE(o))[@inline] in
let ballec#852 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let ballec#853 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let ballec#854 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let ballec#895 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#896 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#897 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#900 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#901 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#904 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#905 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let ballec#906 = fun b -> (({ SHA256 })@(b))[@inline] in
let ballec#907 = fun b -> (({ SHA512 })@(b))[@inline] in
let ballec#908 = fun b -> (({ SHA3 })@(b))[@inline] in
let ballec#909 = fun b -> (({ KECCAK })@(b))[@inline] in
let ballec#910 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let ballec#911 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let ballec#912 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let ballec#913 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let ballec#918 = fun i -> (({ ABS })@(i))[@inline] in
let ballec#919 = fun i -> (({ ISNAT })@(i))[@inline] in
let ballec#920 = TRUE()[@inline] in
let ballec#921 = FALSE()[@inline] in
let ballec#922 = UNIT()[@inline] in
let ballec#926 = L(44) in
let ballec#930 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let ballec#931 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let ballec#932 = ({ DROP ; NOW })@(L(unit))[@inline] in
let ballec#933 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let ballec#934 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let ballec#935 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let ballec#936 = SELF_ADDRESS()[@inline] in
let ballec#937 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let ballec#938 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let ballec#939 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let ballec#940 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let ballec#941 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let ballec#942 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let ballec#943 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let ballec#944 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let ballec#945 = fun _u -> (SELF_ADDRESS())[@inline] in
let ballec#946 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let ballec#947 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let ballec#948 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let ballec#949 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let ballec#950 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let ballec#952 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let ballec#958 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let ballec#959 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let ballec#963 = fun o -> (SET_DELEGATE(o))[@inline] in
let ballec#964 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let ballec#965 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let ballec#966 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let ballec#1007 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1008 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1009 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#1012 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1013 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1016 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#1017 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let ballec#1018 = fun b -> (({ SHA256 })@(b))[@inline] in
let ballec#1019 = fun b -> (({ SHA512 })@(b))[@inline] in
let ballec#1020 = fun b -> (({ SHA3 })@(b))[@inline] in
let ballec#1021 = fun b -> (({ KECCAK })@(b))[@inline] in
let ballec#1022 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let ballec#1023 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let ballec#1024 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let ballec#1025 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let ballec#1030 = fun i -> (({ ABS })@(i))[@inline] in
let ballec#1031 = fun i -> (({ ISNAT })@(i))[@inline] in
let ballec#1032 = TRUE()[@inline] in
let ballec#1033 = FALSE()[@inline] in
let ballec#1034 = UNIT()[@inline] in
let poly_ballec_60 = { FAILWITH }[@inline] in
let poly_ballec_59 = { FAILWITH }[@inline] in
let poly_ballec_58 = { FAILWITH }[@inline] in
let poly_ballec_57 = { FAILWITH }[@inline] in
let poly_ballec_56 = { FAILWITH }[@inline] in
let poly_ballec_55 = { FAILWITH }[@inline] in
let poly_ballec_54 = { FAILWITH }[@inline] in
let poly_ballec_53 = { FAILWITH }[@inline] in
let poly_ballec_52 = { FAILWITH }[@inline] in
let poly_ballec_51 = { FAILWITH }[@inline] in
let poly_ballec_50 = { FAILWITH }[@inline] in
let poly_ballec_49 = { FAILWITH }[@inline] in
let poly_ballec_48 = { FAILWITH }[@inline] in
let poly_ballec_47 = { FAILWITH }[@inline] in
let poly_ballec_46 = { FAILWITH }[@inline] in
let ballec#1040 =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_ballec_60)@(L("TEST MODE")))))))[@inline] in
let ballec#1042 = fun _a -> ((poly_ballec_50)@(L("TEST MODE")))[@inline] in
let ballec#1043 = fun _a -> ((poly_ballec_50)@(L("TEST MODE")))[@inline] in
let ballec#1044 = fun _bp -> ((poly_ballec_50)@(L("TEST MODE")))[@inline] in
let ballec#1045 =
  fun _a -> (fun _s -> (fun _t -> ((poly_ballec_50)@(L("TEST MODE")))))[@inline] in
let ballec#1046 =
  fun _a -> (fun _s -> (fun _t -> ((poly_ballec_57)@(L("TEST MODE")))))[@inline] in
let ballec#1050 = fun _a -> ((poly_ballec_50)@(L("TEST MODE")))[@inline] in
let ballec#1051 = fun _a -> ((poly_ballec_59)@(L("TEST MODE")))[@inline] in
let ballec#1052 =
  fun _m1 -> (fun _m2 -> ((poly_ballec_58)@(L("TEST MODE"))))[@inline] in
let ballec#1054 =
  fun _n -> (fun _l -> ((poly_ballec_50)@(L("TEST MODE"))))[@inline] in
let ballec#1055 =
  fun _t -> (fun _n -> (fun _l -> ((poly_ballec_50)@(L("TEST MODE")))))[@inline] in
let ballec#1056 = fun _kh -> ((poly_ballec_57)@(L("TEST MODE")))[@inline] in
let ballec#1057 = (poly_ballec_57)@(L("TEST MODE"))[@inline] in
let ballec#1059 = fun _i -> ((poly_ballec_49)@(L("TEST MODE")))[@inline] in
let ballec#1060 = fun _i -> ((poly_ballec_49)@(L("TEST MODE")))[@inline] in
let ballec#1062 = fun _u -> ((poly_ballec_56)@(L("TEST MODE")))[@inline] in
let ballec#1065 =
  fun _s -> (fun _m -> ((poly_ballec_55)@(L("TEST MODE"))))[@inline] in
let ballec#1072 =
  fun _s -> (fun _k -> ((poly_ballec_50)@(L("TEST MODE"))))[@inline] in
let ballec#1073 = fun _u -> ((poly_ballec_54)@(L("TEST MODE")))[@inline] in
let ballec#1074 =
  fun _p -> (fun _o -> ((poly_ballec_50)@(L("TEST MODE"))))[@inline] in
let ballec#1075 = fun _n -> ((poly_ballec_50)@(L("TEST MODE")))[@inline] in
let ballec#1076 = fun _kh -> ((poly_ballec_50)@(L("TEST MODE")))[@inline] in
let ballec#1077 = fun _m -> ((poly_ballec_53)@(L("TEST MODE")))[@inline] in
let ballec#1082 =
  fun _b -> (fun _n -> ((poly_ballec_52)@(L("TEST MODE"))))[@inline] in
let ballec#1083 =
  fun _c -> (fun _n -> ((poly_ballec_51)@(L("TEST MODE"))))[@inline] in
let ballec#1084 = fun _s -> ((poly_ballec_50)@(L("TEST MODE")))[@inline] in
let ballec#1085 =
  fun _u -> ((poly_ballec_50)@(L("TEST_POP_CONTEXT")))[@inline] in
let ballec#1086 =
  fun _u -> ((poly_ballec_50)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let ballec#1087 =
  fun _u -> ((poly_ballec_50)@(L("TEST_DROP_CONTEXT")))[@inline] in
let ballec#1088 =
  fun _fn -> ((poly_ballec_50)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let ballec#1089 =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_ballec_50)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let ballec#1091 =
  fun _c -> (fun _s -> (fun _t -> ((poly_ballec_49)@(L("TEST_ORIGINATE")))))[@inline] in
let ballec#1092 = fun _c -> ((poly_ballec_48)@(L("TEST_SIZE")))[@inline] in
let ballec#1093 =
  fun _n -> ((poly_ballec_47)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let ballec#1094 =
  fun _sk -> (fun _d -> ((poly_ballec_46)@(L("TEST_SIGN"))))[@inline] in
let ballec#1098 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let ballec#1099 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let ballec#1100 = ({ DROP ; NOW })@(L(unit))[@inline] in
let ballec#1101 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let ballec#1102 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let ballec#1103 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let ballec#1104 = SELF_ADDRESS()[@inline] in
let ballec#1105 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let ballec#1106 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let ballec#1107 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let ballec#1108 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let ballec#1109 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let ballec#1110 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let ballec#1111 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let ballec#1112 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let ballec#1113 = fun _u -> (SELF_ADDRESS())[@inline] in
let ballec#1114 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let ballec#1115 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let ballec#1116 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let ballec#1118 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let ballec#1124 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let ballec#1125 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let ballec#1129 = fun o -> (SET_DELEGATE(o))[@inline] in
let ballec#1130 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let ballec#1131 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let ballec#1132 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let ballec#1173 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1174 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1175 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#1178 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1179 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1182 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#1183 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let ballec#1184 = fun b -> (({ SHA256 })@(b))[@inline] in
let ballec#1185 = fun b -> (({ SHA512 })@(b))[@inline] in
let ballec#1186 = fun b -> (({ SHA3 })@(b))[@inline] in
let ballec#1187 = fun b -> (({ KECCAK })@(b))[@inline] in
let ballec#1188 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let ballec#1189 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let ballec#1190 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let ballec#1191 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let ballec#1196 = fun i -> (({ ABS })@(i))[@inline] in
let ballec#1197 = fun i -> (({ ISNAT })@(i))[@inline] in
let ballec#1198 = TRUE()[@inline] in
let ballec#1199 = FALSE()[@inline] in
let ballec#1200 = UNIT()[@inline] in
let ballec#1204 = L(43) in
let ballec#1208 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let ballec#1209 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let ballec#1210 = ({ DROP ; NOW })@(L(unit))[@inline] in
let ballec#1211 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let ballec#1212 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let ballec#1213 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let ballec#1214 = SELF_ADDRESS()[@inline] in
let ballec#1215 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let ballec#1216 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let ballec#1217 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let ballec#1218 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let ballec#1219 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let ballec#1220 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let ballec#1221 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let ballec#1222 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let ballec#1223 = fun _u -> (SELF_ADDRESS())[@inline] in
let ballec#1224 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let ballec#1225 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let ballec#1226 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let ballec#1227 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let ballec#1228 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let ballec#1230 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let ballec#1236 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let ballec#1237 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let ballec#1241 = fun o -> (SET_DELEGATE(o))[@inline] in
let ballec#1242 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let ballec#1243 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let ballec#1244 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let ballec#1285 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1286 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1287 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#1290 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1291 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1294 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#1295 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let ballec#1296 = fun b -> (({ SHA256 })@(b))[@inline] in
let ballec#1297 = fun b -> (({ SHA512 })@(b))[@inline] in
let ballec#1298 = fun b -> (({ SHA3 })@(b))[@inline] in
let ballec#1299 = fun b -> (({ KECCAK })@(b))[@inline] in
let ballec#1300 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let ballec#1301 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let ballec#1302 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let ballec#1303 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let ballec#1308 = fun i -> (({ ABS })@(i))[@inline] in
let ballec#1309 = fun i -> (({ ISNAT })@(i))[@inline] in
let ballec#1310 = TRUE()[@inline] in
let ballec#1311 = FALSE()[@inline] in
let ballec#1312 = UNIT()[@inline] in
let poly_ballec_45 = { FAILWITH }[@inline] in
let poly_ballec_44 = { FAILWITH }[@inline] in
let poly_ballec_43 = { FAILWITH }[@inline] in
let poly_ballec_42 = { FAILWITH }[@inline] in
let poly_ballec_41 = { FAILWITH }[@inline] in
let poly_ballec_40 = { FAILWITH }[@inline] in
let poly_ballec_39 = { FAILWITH }[@inline] in
let poly_ballec_38 = { FAILWITH }[@inline] in
let poly_ballec_37 = { FAILWITH }[@inline] in
let poly_ballec_36 = { FAILWITH }[@inline] in
let poly_ballec_35 = { FAILWITH }[@inline] in
let poly_ballec_34 = { FAILWITH }[@inline] in
let poly_ballec_33 = { FAILWITH }[@inline] in
let poly_ballec_32 = { FAILWITH }[@inline] in
let poly_ballec_31 = { FAILWITH }[@inline] in
let ballec#1318 =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_ballec_45)@(L("TEST MODE")))))))[@inline] in
let ballec#1320 = fun _a -> ((poly_ballec_35)@(L("TEST MODE")))[@inline] in
let ballec#1321 = fun _a -> ((poly_ballec_35)@(L("TEST MODE")))[@inline] in
let ballec#1322 = fun _bp -> ((poly_ballec_35)@(L("TEST MODE")))[@inline] in
let ballec#1323 =
  fun _a -> (fun _s -> (fun _t -> ((poly_ballec_35)@(L("TEST MODE")))))[@inline] in
let ballec#1324 =
  fun _a -> (fun _s -> (fun _t -> ((poly_ballec_42)@(L("TEST MODE")))))[@inline] in
let ballec#1328 = fun _a -> ((poly_ballec_35)@(L("TEST MODE")))[@inline] in
let ballec#1329 = fun _a -> ((poly_ballec_44)@(L("TEST MODE")))[@inline] in
let ballec#1330 =
  fun _m1 -> (fun _m2 -> ((poly_ballec_43)@(L("TEST MODE"))))[@inline] in
let ballec#1332 =
  fun _n -> (fun _l -> ((poly_ballec_35)@(L("TEST MODE"))))[@inline] in
let ballec#1333 =
  fun _t -> (fun _n -> (fun _l -> ((poly_ballec_35)@(L("TEST MODE")))))[@inline] in
let ballec#1334 = fun _kh -> ((poly_ballec_42)@(L("TEST MODE")))[@inline] in
let ballec#1335 = (poly_ballec_42)@(L("TEST MODE"))[@inline] in
let ballec#1337 = fun _i -> ((poly_ballec_34)@(L("TEST MODE")))[@inline] in
let ballec#1338 = fun _i -> ((poly_ballec_34)@(L("TEST MODE")))[@inline] in
let ballec#1340 = fun _u -> ((poly_ballec_41)@(L("TEST MODE")))[@inline] in
let ballec#1343 =
  fun _s -> (fun _m -> ((poly_ballec_40)@(L("TEST MODE"))))[@inline] in
let ballec#1350 =
  fun _s -> (fun _k -> ((poly_ballec_35)@(L("TEST MODE"))))[@inline] in
let ballec#1351 = fun _u -> ((poly_ballec_39)@(L("TEST MODE")))[@inline] in
let ballec#1352 =
  fun _p -> (fun _o -> ((poly_ballec_35)@(L("TEST MODE"))))[@inline] in
let ballec#1353 = fun _n -> ((poly_ballec_35)@(L("TEST MODE")))[@inline] in
let ballec#1354 = fun _kh -> ((poly_ballec_35)@(L("TEST MODE")))[@inline] in
let ballec#1355 = fun _m -> ((poly_ballec_38)@(L("TEST MODE")))[@inline] in
let ballec#1360 =
  fun _b -> (fun _n -> ((poly_ballec_37)@(L("TEST MODE"))))[@inline] in
let ballec#1361 =
  fun _c -> (fun _n -> ((poly_ballec_36)@(L("TEST MODE"))))[@inline] in
let ballec#1362 = fun _s -> ((poly_ballec_35)@(L("TEST MODE")))[@inline] in
let ballec#1363 =
  fun _u -> ((poly_ballec_35)@(L("TEST_POP_CONTEXT")))[@inline] in
let ballec#1364 =
  fun _u -> ((poly_ballec_35)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let ballec#1365 =
  fun _u -> ((poly_ballec_35)@(L("TEST_DROP_CONTEXT")))[@inline] in
let ballec#1366 =
  fun _fn -> ((poly_ballec_35)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let ballec#1367 =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_ballec_35)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let ballec#1369 =
  fun _c -> (fun _s -> (fun _t -> ((poly_ballec_34)@(L("TEST_ORIGINATE")))))[@inline] in
let ballec#1370 = fun _c -> ((poly_ballec_33)@(L("TEST_SIZE")))[@inline] in
let ballec#1371 =
  fun _n -> ((poly_ballec_32)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let ballec#1372 =
  fun _sk -> (fun _d -> ((poly_ballec_31)@(L("TEST_SIGN"))))[@inline] in
let ballec#1376 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let ballec#1377 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let ballec#1378 = ({ DROP ; NOW })@(L(unit))[@inline] in
let ballec#1379 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let ballec#1380 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let ballec#1381 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let ballec#1382 = SELF_ADDRESS()[@inline] in
let ballec#1383 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let ballec#1384 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let ballec#1385 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let ballec#1386 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let ballec#1387 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let ballec#1388 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let ballec#1389 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let ballec#1390 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let ballec#1391 = fun _u -> (SELF_ADDRESS())[@inline] in
let ballec#1392 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let ballec#1393 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let ballec#1394 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let ballec#1396 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let ballec#1402 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let ballec#1403 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let ballec#1407 = fun o -> (SET_DELEGATE(o))[@inline] in
let ballec#1408 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let ballec#1409 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let ballec#1410 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let ballec#1451 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1452 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1453 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#1456 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1457 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1460 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#1461 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let ballec#1462 = fun b -> (({ SHA256 })@(b))[@inline] in
let ballec#1463 = fun b -> (({ SHA512 })@(b))[@inline] in
let ballec#1464 = fun b -> (({ SHA3 })@(b))[@inline] in
let ballec#1465 = fun b -> (({ KECCAK })@(b))[@inline] in
let ballec#1466 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let ballec#1467 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let ballec#1468 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let ballec#1469 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let ballec#1474 = fun i -> (({ ABS })@(i))[@inline] in
let ballec#1475 = fun i -> (({ ISNAT })@(i))[@inline] in
let ballec#1476 = TRUE()[@inline] in
let ballec#1477 = FALSE()[@inline] in
let ballec#1478 = UNIT()[@inline] in
let ballec#1482 = ADD(ballec#368 , ballec#647) in
let ballec#1483 = (ballec#648)@(PAIR(L(unit) , L(3))) in
let ballec#1487 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let ballec#1488 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let ballec#1489 = ({ DROP ; NOW })@(L(unit))[@inline] in
let ballec#1490 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let ballec#1491 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let ballec#1492 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let ballec#1493 = SELF_ADDRESS()[@inline] in
let ballec#1494 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let ballec#1495 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let ballec#1496 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let ballec#1497 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let ballec#1498 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let ballec#1499 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let ballec#1500 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let ballec#1501 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let ballec#1502 = fun _u -> (SELF_ADDRESS())[@inline] in
let ballec#1503 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let ballec#1504 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let ballec#1505 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let ballec#1506 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let ballec#1507 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let ballec#1509 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let ballec#1515 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let ballec#1516 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let ballec#1520 = fun o -> (SET_DELEGATE(o))[@inline] in
let ballec#1521 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let ballec#1522 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let ballec#1523 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let ballec#1564 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1565 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1566 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#1569 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1570 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1573 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#1574 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let ballec#1575 = fun b -> (({ SHA256 })@(b))[@inline] in
let ballec#1576 = fun b -> (({ SHA512 })@(b))[@inline] in
let ballec#1577 = fun b -> (({ SHA3 })@(b))[@inline] in
let ballec#1578 = fun b -> (({ KECCAK })@(b))[@inline] in
let ballec#1579 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let ballec#1580 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let ballec#1581 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let ballec#1582 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let ballec#1587 = fun i -> (({ ABS })@(i))[@inline] in
let ballec#1588 = fun i -> (({ ISNAT })@(i))[@inline] in
let ballec#1589 = TRUE()[@inline] in
let ballec#1590 = FALSE()[@inline] in
let ballec#1591 = UNIT()[@inline] in
let poly_ballec_30 = { FAILWITH }[@inline] in
let poly_ballec_29 = { FAILWITH }[@inline] in
let poly_ballec_28 = { FAILWITH }[@inline] in
let poly_ballec_27 = { FAILWITH }[@inline] in
let poly_ballec_26 = { FAILWITH }[@inline] in
let poly_ballec_25 = { FAILWITH }[@inline] in
let poly_ballec_24 = { FAILWITH }[@inline] in
let poly_ballec_23 = { FAILWITH }[@inline] in
let poly_ballec_22 = { FAILWITH }[@inline] in
let poly_ballec_21 = { FAILWITH }[@inline] in
let poly_ballec_20 = { FAILWITH }[@inline] in
let poly_ballec_19 = { FAILWITH }[@inline] in
let poly_ballec_18 = { FAILWITH }[@inline] in
let poly_ballec_17 = { FAILWITH }[@inline] in
let poly_ballec_16 = { FAILWITH }[@inline] in
let ballec#1597 =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_ballec_30)@(L("TEST MODE")))))))[@inline] in
let ballec#1599 = fun _a -> ((poly_ballec_20)@(L("TEST MODE")))[@inline] in
let ballec#1600 = fun _a -> ((poly_ballec_20)@(L("TEST MODE")))[@inline] in
let ballec#1601 = fun _bp -> ((poly_ballec_20)@(L("TEST MODE")))[@inline] in
let ballec#1602 =
  fun _a -> (fun _s -> (fun _t -> ((poly_ballec_20)@(L("TEST MODE")))))[@inline] in
let ballec#1603 =
  fun _a -> (fun _s -> (fun _t -> ((poly_ballec_27)@(L("TEST MODE")))))[@inline] in
let ballec#1607 = fun _a -> ((poly_ballec_20)@(L("TEST MODE")))[@inline] in
let ballec#1608 = fun _a -> ((poly_ballec_29)@(L("TEST MODE")))[@inline] in
let ballec#1609 =
  fun _m1 -> (fun _m2 -> ((poly_ballec_28)@(L("TEST MODE"))))[@inline] in
let ballec#1611 =
  fun _n -> (fun _l -> ((poly_ballec_20)@(L("TEST MODE"))))[@inline] in
let ballec#1612 =
  fun _t -> (fun _n -> (fun _l -> ((poly_ballec_20)@(L("TEST MODE")))))[@inline] in
let ballec#1613 = fun _kh -> ((poly_ballec_27)@(L("TEST MODE")))[@inline] in
let ballec#1614 = (poly_ballec_27)@(L("TEST MODE"))[@inline] in
let ballec#1616 = fun _i -> ((poly_ballec_19)@(L("TEST MODE")))[@inline] in
let ballec#1617 = fun _i -> ((poly_ballec_19)@(L("TEST MODE")))[@inline] in
let ballec#1619 = fun _u -> ((poly_ballec_26)@(L("TEST MODE")))[@inline] in
let ballec#1622 =
  fun _s -> (fun _m -> ((poly_ballec_25)@(L("TEST MODE"))))[@inline] in
let ballec#1629 =
  fun _s -> (fun _k -> ((poly_ballec_20)@(L("TEST MODE"))))[@inline] in
let ballec#1630 = fun _u -> ((poly_ballec_24)@(L("TEST MODE")))[@inline] in
let ballec#1631 =
  fun _p -> (fun _o -> ((poly_ballec_20)@(L("TEST MODE"))))[@inline] in
let ballec#1632 = fun _n -> ((poly_ballec_20)@(L("TEST MODE")))[@inline] in
let ballec#1633 = fun _kh -> ((poly_ballec_20)@(L("TEST MODE")))[@inline] in
let ballec#1634 = fun _m -> ((poly_ballec_23)@(L("TEST MODE")))[@inline] in
let ballec#1639 =
  fun _b -> (fun _n -> ((poly_ballec_22)@(L("TEST MODE"))))[@inline] in
let ballec#1640 =
  fun _c -> (fun _n -> ((poly_ballec_21)@(L("TEST MODE"))))[@inline] in
let ballec#1641 = fun _s -> ((poly_ballec_20)@(L("TEST MODE")))[@inline] in
let ballec#1642 =
  fun _u -> ((poly_ballec_20)@(L("TEST_POP_CONTEXT")))[@inline] in
let ballec#1643 =
  fun _u -> ((poly_ballec_20)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let ballec#1644 =
  fun _u -> ((poly_ballec_20)@(L("TEST_DROP_CONTEXT")))[@inline] in
let ballec#1645 =
  fun _fn -> ((poly_ballec_20)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let ballec#1646 =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_ballec_20)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let ballec#1648 =
  fun _c -> (fun _s -> (fun _t -> ((poly_ballec_19)@(L("TEST_ORIGINATE")))))[@inline] in
let ballec#1649 = fun _c -> ((poly_ballec_18)@(L("TEST_SIZE")))[@inline] in
let ballec#1650 =
  fun _n -> ((poly_ballec_17)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let ballec#1651 =
  fun _sk -> (fun _d -> ((poly_ballec_16)@(L("TEST_SIGN"))))[@inline] in
let ballec#1655 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let ballec#1656 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let ballec#1657 = ({ DROP ; NOW })@(L(unit))[@inline] in
let ballec#1658 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let ballec#1659 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let ballec#1660 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let ballec#1661 = SELF_ADDRESS()[@inline] in
let ballec#1662 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let ballec#1663 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let ballec#1664 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let ballec#1665 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let ballec#1666 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let ballec#1667 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let ballec#1668 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let ballec#1669 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let ballec#1670 = fun _u -> (SELF_ADDRESS())[@inline] in
let ballec#1671 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let ballec#1672 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let ballec#1673 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let ballec#1675 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let ballec#1681 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let ballec#1682 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let ballec#1686 = fun o -> (SET_DELEGATE(o))[@inline] in
let ballec#1687 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let ballec#1688 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let ballec#1689 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let ballec#1730 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1731 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1732 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#1735 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1736 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1739 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#1740 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let ballec#1741 = fun b -> (({ SHA256 })@(b))[@inline] in
let ballec#1742 = fun b -> (({ SHA512 })@(b))[@inline] in
let ballec#1743 = fun b -> (({ SHA3 })@(b))[@inline] in
let ballec#1744 = fun b -> (({ KECCAK })@(b))[@inline] in
let ballec#1745 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let ballec#1746 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let ballec#1747 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let ballec#1748 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let ballec#1753 = fun i -> (({ ABS })@(i))[@inline] in
let ballec#1754 = fun i -> (({ ISNAT })@(i))[@inline] in
let ballec#1755 = TRUE()[@inline] in
let ballec#1756 = FALSE()[@inline] in
let ballec#1757 = UNIT()[@inline] in
let ballec#1761 = L(10) in
let ballec#1762 = L("bar") in
let ballec#1766 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let ballec#1767 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let ballec#1768 = ({ DROP ; NOW })@(L(unit))[@inline] in
let ballec#1769 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let ballec#1770 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let ballec#1771 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let ballec#1772 = SELF_ADDRESS()[@inline] in
let ballec#1773 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let ballec#1774 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let ballec#1775 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let ballec#1776 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let ballec#1777 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let ballec#1778 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let ballec#1779 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let ballec#1780 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let ballec#1781 = fun _u -> (SELF_ADDRESS())[@inline] in
let ballec#1782 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let ballec#1783 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let ballec#1784 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let ballec#1785 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let ballec#1786 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let ballec#1788 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let ballec#1794 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let ballec#1795 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let ballec#1799 = fun o -> (SET_DELEGATE(o))[@inline] in
let ballec#1800 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let ballec#1801 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let ballec#1802 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let ballec#1843 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1844 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1845 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#1848 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1849 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1852 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#1853 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let ballec#1854 = fun b -> (({ SHA256 })@(b))[@inline] in
let ballec#1855 = fun b -> (({ SHA512 })@(b))[@inline] in
let ballec#1856 = fun b -> (({ SHA3 })@(b))[@inline] in
let ballec#1857 = fun b -> (({ KECCAK })@(b))[@inline] in
let ballec#1858 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let ballec#1859 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let abs = fun i -> (({ ABS })@(i))[@inline] in
let is_nat = fun i -> (({ ISNAT })@(i))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let poly_ballec_15 = { FAILWITH }[@inline] in
let poly_ballec_14 = { FAILWITH }[@inline] in
let poly_ballec_13 = { FAILWITH }[@inline] in
let poly_ballec_12 = { FAILWITH }[@inline] in
let poly_ballec_11 = { FAILWITH }[@inline] in
let poly_ballec_10 = { FAILWITH }[@inline] in
let poly_ballec_9 = { FAILWITH }[@inline] in
let poly_ballec_8 = { FAILWITH }[@inline] in
let poly_ballec_7 = { FAILWITH }[@inline] in
let poly_ballec_6 = { FAILWITH }[@inline] in
let poly_ballec_5 = { FAILWITH }[@inline] in
let poly_ballec_4 = { FAILWITH }[@inline] in
let poly_ballec_3 = { FAILWITH }[@inline] in
let poly_ballec_2 = { FAILWITH }[@inline] in
let poly_ballec_1 = { FAILWITH }[@inline] in
let ballec#1862 =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_ballec_15)@(L("TEST MODE")))))))[@inline] in
let ballec#1864 = fun _a -> ((poly_ballec_5)@(L("TEST MODE")))[@inline] in
let ballec#1865 = fun _a -> ((poly_ballec_5)@(L("TEST MODE")))[@inline] in
let ballec#1866 = fun _bp -> ((poly_ballec_5)@(L("TEST MODE")))[@inline] in
let ballec#1867 =
  fun _a -> (fun _s -> (fun _t -> ((poly_ballec_5)@(L("TEST MODE")))))[@inline] in
let ballec#1868 =
  fun _a -> (fun _s -> (fun _t -> ((poly_ballec_12)@(L("TEST MODE")))))[@inline] in
let ballec#1872 = fun _a -> ((poly_ballec_5)@(L("TEST MODE")))[@inline] in
let ballec#1873 = fun _a -> ((poly_ballec_14)@(L("TEST MODE")))[@inline] in
let ballec#1874 =
  fun _m1 -> (fun _m2 -> ((poly_ballec_13)@(L("TEST MODE"))))[@inline] in
let ballec#1876 =
  fun _n -> (fun _l -> ((poly_ballec_5)@(L("TEST MODE"))))[@inline] in
let ballec#1877 =
  fun _t -> (fun _n -> (fun _l -> ((poly_ballec_5)@(L("TEST MODE")))))[@inline] in
let ballec#1878 = fun _kh -> ((poly_ballec_12)@(L("TEST MODE")))[@inline] in
let ballec#1879 = (poly_ballec_12)@(L("TEST MODE"))[@inline] in
let ballec#1881 = fun _i -> ((poly_ballec_4)@(L("TEST MODE")))[@inline] in
let ballec#1882 = fun _i -> ((poly_ballec_4)@(L("TEST MODE")))[@inline] in
let ballec#1884 = fun _u -> ((poly_ballec_11)@(L("TEST MODE")))[@inline] in
let ballec#1887 =
  fun _s -> (fun _m -> ((poly_ballec_10)@(L("TEST MODE"))))[@inline] in
let ballec#1894 =
  fun _s -> (fun _k -> ((poly_ballec_5)@(L("TEST MODE"))))[@inline] in
let ballec#1895 = fun _u -> ((poly_ballec_9)@(L("TEST MODE")))[@inline] in
let ballec#1896 =
  fun _p -> (fun _o -> ((poly_ballec_5)@(L("TEST MODE"))))[@inline] in
let ballec#1897 = fun _n -> ((poly_ballec_5)@(L("TEST MODE")))[@inline] in
let ballec#1898 = fun _kh -> ((poly_ballec_5)@(L("TEST MODE")))[@inline] in
let ballec#1899 = fun _m -> ((poly_ballec_8)@(L("TEST MODE")))[@inline] in
let ballec#1904 =
  fun _b -> (fun _n -> ((poly_ballec_7)@(L("TEST MODE"))))[@inline] in
let ballec#1905 =
  fun _c -> (fun _n -> ((poly_ballec_6)@(L("TEST MODE"))))[@inline] in
let ballec#1906 = fun _s -> ((poly_ballec_5)@(L("TEST MODE")))[@inline] in
let ballec#1907 =
  fun _u -> ((poly_ballec_5)@(L("TEST_POP_CONTEXT")))[@inline] in
let ballec#1908 =
  fun _u -> ((poly_ballec_5)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let ballec#1909 =
  fun _u -> ((poly_ballec_5)@(L("TEST_DROP_CONTEXT")))[@inline] in
let ballec#1910 =
  fun _fn -> ((poly_ballec_5)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let ballec#1911 =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_ballec_5)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let ballec#1913 =
  fun _c -> (fun _s -> (fun _t -> ((poly_ballec_4)@(L("TEST_ORIGINATE")))))[@inline] in
let ballec#1914 = fun _c -> ((poly_ballec_3)@(L("TEST_SIZE")))[@inline] in
let ballec#1915 =
  fun _n -> ((poly_ballec_2)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let ballec#1916 =
  fun _sk -> (fun _d -> ((poly_ballec_1)@(L("TEST_SIGN"))))[@inline] in
let ballec#1920 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let ballec#1921 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let ballec#1922 = ({ DROP ; NOW })@(L(unit))[@inline] in
let ballec#1923 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let ballec#1924 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let ballec#1925 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let ballec#1926 = SELF_ADDRESS()[@inline] in
let ballec#1927 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let ballec#1928 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let ballec#1929 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let ballec#1930 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let ballec#1931 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let ballec#1932 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let ballec#1933 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let ballec#1934 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let ballec#1935 = fun _u -> (SELF_ADDRESS())[@inline] in
let ballec#1936 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let ballec#1937 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let ballec#1938 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let ballec#1940 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let ballec#1946 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let ballec#1947 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let ballec#1951 = fun o -> (SET_DELEGATE(o))[@inline] in
let ballec#1952 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let ballec#1953 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let ballec#1954 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let ballec#1995 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#1996 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#1997 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#2000 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let ballec#2001 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let ballec#2004 = fun b -> (({ SIZE })@(b))[@inline] in
let ballec#2005 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let ballec#2006 = fun b -> (({ SHA256 })@(b))[@inline] in
let ballec#2007 = fun b -> (({ SHA512 })@(b))[@inline] in
let ballec#2008 = fun b -> (({ SHA3 })@(b))[@inline] in
let ballec#2009 = fun b -> (({ KECCAK })@(b))[@inline] in
let ballec#2010 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let ballec#2011 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let abs = fun i -> (({ ABS })@(i))[@inline] in
let is_nat = fun i -> (({ ISNAT })@(i))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let toto = ADD(ballec#1761 , ballec#368) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#6971 ->
  (let (gen#6977, gen#6978) = gen#6971 in
   let p = gen#6977 in
   let s = gen#6978 in
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
