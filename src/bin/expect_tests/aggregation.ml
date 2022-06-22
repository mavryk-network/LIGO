open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let ballec#263 : int = 42 in
    let ballec#264 : int = 1 in
    let x : int = ballec#263 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let ballec#263 : int = 40 in
    let ballec#266 : int = let ballec#264 : int = 1 in
    let ballec#265 : int = ballec#264 in
    ADD(ballec#264 ,
    ballec#265) in
    let x : int = ADD(ballec#263 , ballec#266) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let ballec#263 : int = 1 in
    let ballec#264 : int = 42 in
    let ballec#265 : int = ballec#263 in
    let ballec#266 : int = ballec#264 in
    let x : int = ballec#264 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let ballec#263 : int = 20 in
  let ballec#264 : int = 22 in
  let x : int = ADD(ballec#263 , ballec#264) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let ballec#263 : int = 1 in
  let ballec#264 : int = 42 in
  let ballec#265 : int = 3 in
  let x : int = ballec#264 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let ballec#263 : int = 1 in
  let foo : int = let x = 20 in
  let ballec#264 : int = x in
  let ballec#265 : int = ballec#263 in
  let ballec#266 : int = ballec#265 in
  ADD(ADD(ADD(ballec#264 , ballec#265) , x) ,
  ballec#266) in
  let x : int = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let ballec#263 : int = 40 in
  let ballec#264 : int = ADD(ballec#263 , 1) in
  let ballec#265 : int = ADD(ballec#264 , 1) in
  let x : int = ballec#265 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let ballec#263 : int = 41 in
  let x : int = 1 in
  let ballec#264 : int = x in
  let ballec#265 : int = ballec#263 in
  let u : int = ADD(ballec#264 , ballec#265) in
  let x : int = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let ballec#263 : int = 41 in
  let ballec#264 : int = ADD(ballec#263 , 1) in
  let x : int = ballec#264 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let ballec#263 : int = 42 in
  let ballec#264 : int = 2 in
  let ballec#265 : int = ballec#263 in
  let x : int = ballec#265 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let ballec#263 : int = 19 in
  let ballec#264 : int = 22 in
  let x : int = let x = 1 in
  let u = ballec#263 in
  let v = ballec#264 in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let ballec#263 : int = 42 in
  let ballec#264 : int = ballec#263 in
  let x : int = ballec#264 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let ballec#265 : nat -> nat = lambda (i : nat) return ADD(i , +1) in
  let ballec#266 : nat -> unit = lambda (n : nat) return let current_turn = (ballec#265)@(+1) in
  (assert)@(EQ(n ,
  current_turn)) in
  let main : ( unit * unit ) -> ( list (operation) * unit ) = lambda (gen#2 : ( unit * unit )) return  match
                                                                      gen#2 with
                                                                      | ( _p , _s ) ->
                                                                      ( LIST_EMPTY() , (ballec#266)@(+2) ) in
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
  [%expect{|
    { parameter int ;
      storage int ;
      code { CDR ; PUSH string "foo" ; FAILWITH } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "bug_module_record.ligo" ] ;
  [%expect{|
    let ballec#90 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
    let ballec#91 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
    let ballec#92 = ({ DROP ; NOW })@(L(unit))[@inline] in
    let ballec#93 = ({ DROP ; SENDER })@(L(unit))[@inline] in
    let ballec#94 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
    let ballec#95 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
    let ballec#96 = SELF_ADDRESS()[@inline] in
    let ballec#97 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
    let ballec#98 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
    let ballec#99 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
    let ballec#100 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
    let ballec#101 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
    let ballec#102 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
    let ballec#103 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
    let ballec#104 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
    let ballec#105 = fun _u -> (SELF_ADDRESS())[@inline] in
    let ballec#106 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
    let ballec#107 =
      fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
    let ballec#108 = { DROP ; MIN_BLOCK_TIME }[@inline] in
    let ballec#109 = { DROP ; MIN_BLOCK_TIME }[@inline] in
    let ballec#110 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
    let ballec#112 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
    let ballec#118 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
    let ballec#119 =
      fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
    let ballec#123 = fun o -> (SET_DELEGATE(o))[@inline] in
    let ballec#124 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
    let ballec#125 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
    let ballec#126 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
    let ballec#167 =
      fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
    let ballec#168 =
      fun s ->
      (fun l ->
       (fun b ->
        (({ UNPAIR ;
           UNPAIR ;
           SLICE ;
           IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                                  b)))))[@inline] in
    let ballec#169 = fun b -> (({ SIZE })@(b))[@inline] in
    let ballec#172 =
      fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
    let ballec#173 =
      fun s ->
      (fun l ->
       (fun b ->
        (({ UNPAIR ;
           UNPAIR ;
           SLICE ;
           IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                                  b)))))[@inline] in
    let ballec#176 = fun b -> (({ SIZE })@(b))[@inline] in
    let ballec#177 = fun b -> (({ BLAKE2B })@(b))[@inline] in
    let ballec#178 = fun b -> (({ SHA256 })@(b))[@inline] in
    let ballec#179 = fun b -> (({ SHA512 })@(b))[@inline] in
    let ballec#180 = fun b -> (({ SHA3 })@(b))[@inline] in
    let ballec#181 = fun b -> (({ KECCAK })@(b))[@inline] in
    let ballec#182 = fun k -> (({ HASH_KEY })@(k))[@inline] in
    let ballec#183 =
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
    let ballec#186 =
      fun gen#621 ->
      (let (gen#1119, gen#1120) = gen#621 in
       let (gen#1121, gen#1122) = gen#1119 in
       let (gen#1125, gen#1126) = gen#1121 in
       let _fn = gen#1125 in
       let _e = gen#1126 in
       let (gen#1123, gen#1124) = gen#1122 in
       let _v = gen#1123 in
       let _s = gen#1124 in
       let _t = gen#1120 in (poly_ballec_15)@(L("TEST MODE")))[@inline] in
    let ballec#188 = fun _a -> ((poly_ballec_5)@(L("TEST MODE")))[@inline] in
    let ballec#189 = fun _a -> ((poly_ballec_5)@(L("TEST MODE")))[@inline] in
    let ballec#190 = fun _bp -> ((poly_ballec_5)@(L("TEST MODE")))[@inline] in
    let ballec#191 =
      fun gen#639 ->
      (let (gen#1127, gen#1128) = gen#639 in
       let (gen#1129, gen#1130) = gen#1127 in
       let _a = gen#1129 in
       let _s = gen#1130 in let _t = gen#1128 in (poly_ballec_5)@(L("TEST MODE")))[@inline] in
    let ballec#192 =
      fun gen#644 ->
      (let (gen#1131, gen#1132) = gen#644 in
       let (gen#1133, gen#1134) = gen#1131 in
       let _a = gen#1133 in
       let _s = gen#1134 in
       let _t = gen#1132 in (poly_ballec_12)@(L("TEST MODE")))[@inline] in
    let ballec#196 = fun _a -> ((poly_ballec_5)@(L("TEST MODE")))[@inline] in
    let ballec#197 = fun _a -> ((poly_ballec_14)@(L("TEST MODE")))[@inline] in
    let ballec#198 =
      fun gen#665 ->
      (let (gen#1135, gen#1136) = gen#665 in
       let _m1 = gen#1135 in
       let _m2 = gen#1136 in (poly_ballec_13)@(L("TEST MODE")))[@inline] in
    let ballec#200 =
      fun gen#671 ->
      (let (gen#1137, gen#1138) = gen#671 in
       let _n = gen#1137 in let _l = gen#1138 in (poly_ballec_5)@(L("TEST MODE")))[@inline] in
    let ballec#201 =
      fun gen#675 ->
      (let (gen#1139, gen#1140) = gen#675 in
       let (gen#1141, gen#1142) = gen#1139 in
       let _t = gen#1141 in
       let _n = gen#1142 in let _l = gen#1140 in (poly_ballec_5)@(L("TEST MODE")))[@inline] in
    let ballec#202 = fun _kh -> ((poly_ballec_12)@(L("TEST MODE")))[@inline] in
    let ballec#203 = (poly_ballec_12)@(L("TEST MODE"))[@inline] in
    let ballec#205 = fun _i -> ((poly_ballec_4)@(L("TEST MODE")))[@inline] in
    let ballec#206 = fun _i -> ((poly_ballec_4)@(L("TEST MODE")))[@inline] in
    let ballec#208 = fun _u -> ((poly_ballec_11)@(L("TEST MODE")))[@inline] in
    let ballec#211 =
      fun gen#702 ->
      (let (gen#1143, gen#1144) = gen#702 in
       let _s = gen#1143 in
       let _m = gen#1144 in (poly_ballec_10)@(L("TEST MODE")))[@inline] in
    let ballec#218 =
      fun gen#724 ->
      (let (gen#1145, gen#1146) = gen#724 in
       let _s = gen#1145 in let _k = gen#1146 in (poly_ballec_5)@(L("TEST MODE")))[@inline] in
    let ballec#219 = fun _u -> ((poly_ballec_9)@(L("TEST MODE")))[@inline] in
    let ballec#220 =
      fun gen#730 ->
      (let (gen#1147, gen#1148) = gen#730 in
       let _p = gen#1147 in let _o = gen#1148 in (poly_ballec_5)@(L("TEST MODE")))[@inline] in
    let ballec#221 = fun _n -> ((poly_ballec_5)@(L("TEST MODE")))[@inline] in
    let ballec#222 = fun _kh -> ((poly_ballec_5)@(L("TEST MODE")))[@inline] in
    let ballec#223 = fun _m -> ((poly_ballec_8)@(L("TEST MODE")))[@inline] in
    let ballec#228 =
      fun gen#752 ->
      (let (gen#1149, gen#1150) = gen#752 in
       let _b = gen#1149 in let _n = gen#1150 in (poly_ballec_7)@(L("TEST MODE")))[@inline] in
    let ballec#229 =
      fun gen#756 ->
      (let (gen#1151, gen#1152) = gen#756 in
       let _c = gen#1151 in let _n = gen#1152 in (poly_ballec_6)@(L("TEST MODE")))[@inline] in
    let ballec#230 = fun _s -> ((poly_ballec_5)@(L("TEST MODE")))[@inline] in
    let ballec#231 =
      fun _u -> ((poly_ballec_5)@(L("TEST_POP_CONTEXT")))[@inline] in
    let ballec#232 =
      fun _u -> ((poly_ballec_5)@(L("TEST_PUSH_CONTEXT")))[@inline] in
    let ballec#233 =
      fun _u -> ((poly_ballec_5)@(L("TEST_DROP_CONTEXT")))[@inline] in
    let ballec#234 =
      fun _fn -> ((poly_ballec_5)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
    let ballec#235 =
      fun gen#770 ->
      (let (gen#1153, gen#1154) = gen#770 in
       let (gen#1155, gen#1156) = gen#1153 in
       let _fn = gen#1155 in
       let _e = gen#1156 in
       let _v = gen#1154 in
       (poly_ballec_5)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))[@inline] in
    let ballec#237 =
      fun gen#777 ->
      (let (gen#1157, gen#1158) = gen#777 in
       let (gen#1159, gen#1160) = gen#1157 in
       let _c = gen#1159 in
       let _s = gen#1160 in
       let _t = gen#1158 in (poly_ballec_4)@(L("TEST_ORIGINATE")))[@inline] in
    let ballec#238 = fun _c -> ((poly_ballec_3)@(L("TEST_SIZE")))[@inline] in
    let ballec#239 =
      fun _n -> ((poly_ballec_2)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
    let ballec#240 =
      fun gen#786 ->
      (let (gen#1161, gen#1162) = gen#786 in
       let _sk = gen#1161 in
       let _d = gen#1162 in (poly_ballec_1)@(L("TEST_SIGN")))[@inline] in
    let ballec#244 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
    let ballec#245 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
    let ballec#246 = ({ DROP ; NOW })@(L(unit))[@inline] in
    let ballec#247 = ({ DROP ; SENDER })@(L(unit))[@inline] in
    let ballec#248 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
    let ballec#249 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
    let ballec#250 = SELF_ADDRESS()[@inline] in
    let ballec#251 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
    let ballec#252 = ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
    let ballec#253 = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
    let ballec#254 = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
    let ballec#255 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
    let ballec#256 = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
    let ballec#257 = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
    let ballec#258 = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
    let ballec#259 = fun _u -> (SELF_ADDRESS())[@inline] in
    let ballec#260 = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
    let ballec#261 =
      fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
    let ballec#262 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
    let ballec#264 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
    let ballec#270 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
    let ballec#271 =
      fun gen#847 ->
      (let (gen#1163, gen#1164) = gen#847 in
       let (gen#1165, gen#1166) = gen#1163 in
       let ck = gen#1165 in
       let c = gen#1166 in let n = gen#1164 in OPEN_CHEST(ck , c , n))[@inline] in
    let ballec#275 = fun o -> (SET_DELEGATE(o))[@inline] in
    let ballec#276 =
      fun gen#866 ->
      (let (gen#1167, gen#1168) = gen#866 in
       let l = gen#1167 in let r = gen#1168 in XOR(l , r))[@inline] in
    let ballec#277 =
      fun gen#870 ->
      (let (gen#1169, gen#1170) = gen#870 in
       let l = gen#1169 in let r = gen#1170 in LSL(l , r))[@inline] in
    let ballec#278 =
      fun gen#874 ->
      (let (gen#1171, gen#1172) = gen#874 in
       let l = gen#1171 in let r = gen#1172 in LSR(l , r))[@inline] in
    let ballec#319 =
      fun gen#1036 ->
      (let (gen#1173, gen#1174) = gen#1036 in
       let b1 = gen#1173 in
       let b2 = gen#1174 in ({ UNPAIR ; CONCAT })@(PAIR(b1 , b2)))[@inline] in
    let ballec#320 =
      fun gen#1040 ->
      (let (gen#1175, gen#1176) = gen#1040 in
       let (gen#1177, gen#1178) = gen#1175 in
       let s = gen#1177 in
       let l = gen#1178 in
       let b = gen#1176 in
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) , b)))[@inline] in
    let ballec#321 = fun b -> (({ SIZE })@(b))[@inline] in
    let ballec#324 =
      fun gen#1053 ->
      (let (gen#1179, gen#1180) = gen#1053 in
       let b1 = gen#1179 in
       let b2 = gen#1180 in ({ UNPAIR ; CONCAT })@(PAIR(b1 , b2)))[@inline] in
    let ballec#325 =
      fun gen#1057 ->
      (let (gen#1181, gen#1182) = gen#1057 in
       let (gen#1183, gen#1184) = gen#1181 in
       let s = gen#1183 in
       let l = gen#1184 in
       let b = gen#1182 in
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) , b)))[@inline] in
    let ballec#328 = fun b -> (({ SIZE })@(b))[@inline] in
    let ballec#329 = fun b -> (({ BLAKE2B })@(b))[@inline] in
    let ballec#330 = fun b -> (({ SHA256 })@(b))[@inline] in
    let ballec#331 = fun b -> (({ SHA512 })@(b))[@inline] in
    let ballec#332 = fun b -> (({ SHA3 })@(b))[@inline] in
    let ballec#333 = fun b -> (({ KECCAK })@(b))[@inline] in
    let ballec#334 = fun k -> (({ HASH_KEY })@(k))[@inline] in
    let ballec#335 =
      fun gen#1080 ->
      (let (gen#1185, gen#1186) = gen#1080 in
       let (gen#1187, gen#1188) = gen#1185 in
       let k = gen#1187 in
       let s = gen#1188 in
       let b = gen#1186 in
       ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))[@inline] in
    let assert =
      fun b ->
      (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
    let assert_with_error =
      fun gen#1087 ->
      (let (gen#1189, gen#1190) = gen#1087 in
       let b = gen#1189 in
       let s = gen#1190 in
       ({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s)))[@inline] in
    let abs = fun i -> (({ ABS })@(i))[@inline] in
    let is_nat = fun i -> (({ ISNAT })@(i))[@inline] in
    let true = TRUE()[@inline] in
    let false = FALSE()[@inline] in
    let unit = UNIT()[@inline] in
    let v = PAIR(L(1) , L("b")) in
    let ballec#336 = v in let tm = ballec#336 in L(unit) |}]
