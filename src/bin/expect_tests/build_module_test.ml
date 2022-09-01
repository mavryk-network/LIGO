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
    module C =
      Mangled_module_____________________test__contracts__build__C____mligo.
    module E =
      Mangled_module_____________________test__contracts__build__E____mligo.
    const toto : int = ADD(E.toto , C.B.A.toto)
    const fb : record[tata -> int , tete -> int , titi -> int , toto -> int] =
      record[tata -> 2 , tete -> 3 , titi -> 1 , toto -> toto]
    const main : ( int * int ) -> ( list (operation) * int ) =
      lambda (gen#5( int * int ))( list (operation) * int ) return  match
                                                                     gen#5 with
                                                                     | ( p , s ) ->
                                                                     let sint =
                                                                       ADD
                                                                       (ADD
                                                                        (p ,
                                                                        s) ,
                                                                        toto) in
                                                                     ( LIST_EMPTY
                                                                       () ,
                                                                       s ) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "instance/main.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "FA2_TOKEN_UNDEFINED" ;
             PUSH string "AAAA" ;
             DUP 2 ;
             CONCAT ;
             SWAP ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "instance/main.mligo" ] ;
  [%expect {|
    module Errors =
      Mangled_module_____________________test__contracts__build__instance____________common__errors____mligo.
    module Storage =
      Mangled_module_____________________test__contracts__build__instance____________common__storage____mligo.
    const main : ( unit * string ) -> ( list (operation) * string ) =
      lambda (gen#2( unit * string ))( list (operation) * string ) return
       match gen#2 with
        | ( _#4 , _#3 ) ->
        ( LIST_EMPTY() , CONCAT(Errors.undefined_token , Storage.s) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let get_balance#70 =
  fun _u#4330 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#71 =
  fun _u#4332 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#72 = fun _u#4334 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#73 =
  fun _u#4336 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#74 =
  fun _u#4338 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#75 = fun _u#4340 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#76 = fun _u#4342 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#77 =
  fun _u#4344 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#78 =
  fun _u#4346 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#79 =
  fun _u#4348 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#80 =
  fun kh#4350 -> (({ VOTING_POWER })@(kh#4350))[@inline] in
let implicit_account#82 =
  fun kh#4354 -> (IMPLICIT_ACCOUNT(kh#4354))[@inline] in
let pairing_check#86 =
  fun l#4362 -> (({ PAIRING_CHECK })@(l#4362))[@inline] in
let set_delegate#88 = fun o#4366 -> (SET_DELEGATE(o#4366))[@inline] in
let open_chest#94 =
  fun ck#4382 ->
  (fun c#4383 -> (fun n#4384 -> (OPEN_CHEST(ck#4382 , c#4383 , n#4384))))[@inline] in
let xor#97 = fun l#4393 -> (fun r#4394 -> (XOR(l#4393 , r#4394)))[@inline] in
let shift_left#98 =
  fun l#4396 -> (fun r#4397 -> (LSL(l#4396 , r#4397)))[@inline] in
let shift_right#99 =
  fun l#4399 -> (fun r#4400 -> (LSR(l#4399 , r#4400)))[@inline] in
let length#140 = fun b#4532 -> (({ SIZE })@(b#4532))[@inline] in
let concat#141 =
  fun b1#4534 ->
  (fun b2#4535 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4534 , b2#4535))))[@inline] in
let sub#142 =
  fun s#4537 ->
  (fun l#4538 ->
   (fun b#4539 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4537 ,
                                                                   l#4538) ,
                                                              b#4539)))))[@inline] in
let length#148 = fun b#4554 -> (({ SIZE })@(b#4554))[@inline] in
let concat#149 =
  fun b1#4556 ->
  (fun b2#4557 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4556 , b2#4557))))[@inline] in
let sub#150 =
  fun s#4559 ->
  (fun l#4560 ->
   (fun b#4561 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4559 ,
                                                                   l#4560) ,
                                                              b#4561)))))[@inline] in
let blake2b#151 = fun b#4563 -> (({ BLAKE2B })@(b#4563))[@inline] in
let sha256#152 = fun b#4565 -> (({ SHA256 })@(b#4565))[@inline] in
let sha512#153 = fun b#4567 -> (({ SHA512 })@(b#4567))[@inline] in
let sha3#154 = fun b#4569 -> (({ SHA3 })@(b#4569))[@inline] in
let keccak#155 = fun b#4571 -> (({ KECCAK })@(b#4571))[@inline] in
let hash_key#156 = fun k#4573 -> (({ HASH_KEY })@(k#4573))[@inline] in
let check#157 =
  fun k#4575 ->
  (fun s#4576 ->
   (fun b#4577 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4575 , s#4576) ,
                                                   b#4577)))))[@inline] in
let assert#158 =
  fun b#4579 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4579))[@inline] in
let abs#161 = fun i#4585 -> (({ ABS })@(i#4585))[@inline] in
let is_nat#162 = fun i#4587 -> (({ ISNAT })@(i#4587))[@inline] in
let true#163 = TRUE()[@inline] in
let false#164 = FALSE()[@inline] in
let unit#165 = UNIT()[@inline] in
let assert_with_error#168 =
  fun b#4595 ->
  (fun s#4596 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4595 , s#4596))))[@inline] in
let poly_stub_273 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_272 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_271 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_270 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_269 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_268 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_267 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_266 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_265 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_264 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_263 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_262 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_261 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_260 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_259 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_258 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_257 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_256 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_255 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_254 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_253 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_252 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_251 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_250 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_249 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_248 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_247 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_246 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_245 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_244 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_243 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_242 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_241 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_240 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_239 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_238 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_237 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_236 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let poly_stub_235 = fun x#4607 -> (({ FAILWITH })@(x#4607))[@inline] in
let get_total_voting_power#176 =
  fun _u#4616 -> ((poly_stub_273)@(L(unit)))[@inline] in
let set_source#179 = fun _a#4622 -> ((poly_stub_272)@(L(unit)))[@inline] in
let get_storage_of_address#180 =
  fun _a#4624 -> ((poly_stub_271)@(L(unit)))[@inline] in
let get_balance#181 = fun _a#4626 -> ((poly_stub_270)@(L(unit)))[@inline] in
let print#182 = fun _v#4628 -> ((poly_stub_269)@(L(unit)))[@inline] in
let eprint#183 = fun _v#4630 -> ((poly_stub_268)@(L(unit)))[@inline] in
let get_voting_power#184 =
  fun _kh#4632 -> ((poly_stub_267)@(L(unit)))[@inline] in
let nth_bootstrap_contract#185 =
  fun _i#4634 -> ((poly_stub_266)@(L(unit)))[@inline] in
let nth_bootstrap_account#186 =
  fun _i#4636 -> ((poly_stub_265)@(L(unit)))[@inline] in
let get_bootstrap_account#187 =
  fun _n#4638 -> ((poly_stub_264)@(L(unit)))[@inline] in
let last_originations#189 =
  fun _u#4642 -> ((poly_stub_263)@(L(unit)))[@inline] in
let new_account#191 = fun _u#4646 -> ((poly_stub_262)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#193 =
  fun _n#4650 -> ((poly_stub_261)@(L(unit)))[@inline] in
let register_delegate#195 =
  fun _kh#4654 -> ((poly_stub_260)@(L(unit)))[@inline] in
let register_constant#196 =
  fun _m#4656 -> ((poly_stub_259)@(L(unit)))[@inline] in
let constant_to_michelson_program#198 =
  fun _s#4660 -> ((poly_stub_258)@(L(unit)))[@inline] in
let restore_context#199 =
  fun _u#4662 -> ((poly_stub_257)@(L(unit)))[@inline] in
let save_context#200 = fun _u#4664 -> ((poly_stub_256)@(L(unit)))[@inline] in
let drop_context#201 = fun _u#4666 -> ((poly_stub_255)@(L(unit)))[@inline] in
let set_baker_policy#204 =
  fun _bp#4672 -> ((poly_stub_254)@(L(unit)))[@inline] in
let set_baker#205 = fun _a#4674 -> ((poly_stub_253)@(L(unit)))[@inline] in
let size#206 = fun _c#4676 -> ((poly_stub_252)@(L(unit)))[@inline] in
let read_contract_from_file#208 =
  fun _fn#4680 -> ((poly_stub_251)@(L(unit)))[@inline] in
let chr#209 = fun _n#4682 -> ((poly_stub_250)@(L(unit)))[@inline] in
let nl#210 = L("NEWLINE")[@inline] in
let println#211 = fun _v#4685 -> ((poly_stub_249)@(L(unit)))[@inline] in
let transfer#212 =
  fun _a#4687 ->
  (fun _s#4688 -> (fun _t#4689 -> ((poly_stub_248)@(L(unit)))))[@inline] in
let transfer_exn#213 =
  fun _a#4691 ->
  (fun _s#4692 -> (fun _t#4693 -> ((poly_stub_247)@(L(unit)))))[@inline] in
let reset_state#215 =
  fun _n#4697 -> (fun _l#4698 -> ((poly_stub_246)@(L(unit))))[@inline] in
let reset_state_at#216 =
  fun _t#4700 ->
  (fun _n#4701 -> (fun _l#4702 -> ((poly_stub_245)@(L(unit)))))[@inline] in
let save_mutation#219 =
  fun _s#4711 -> (fun _m#4712 -> ((poly_stub_244)@(L(unit))))[@inline] in
let sign#222 =
  fun _sk#4720 -> (fun _d#4721 -> ((poly_stub_243)@(L(unit))))[@inline] in
let add_account#223 =
  fun _s#4723 -> (fun _k#4724 -> ((poly_stub_242)@(L(unit))))[@inline] in
let baker_account#224 =
  fun _p#4726 -> (fun _o#4727 -> ((poly_stub_241)@(L(unit))))[@inline] in
let create_chest#226 =
  fun _b#4732 -> (fun _n#4733 -> ((poly_stub_240)@(L(unit))))[@inline] in
let create_chest_key#227 =
  fun _c#4735 -> (fun _n#4736 -> ((poly_stub_239)@(L(unit))))[@inline] in
let michelson_equal#230 =
  fun _m1#4746 -> (fun _m2#4747 -> ((poly_stub_238)@(L(unit))))[@inline] in
let originate_contract#232 =
  fun _c#4752 ->
  (fun _s#4753 -> (fun _t#4754 -> ((poly_stub_237)@(L(unit)))))[@inline] in
let compile_contract_from_file#234 =
  fun _fn#4760 ->
  (fun _e#4761 -> (fun _v#4762 -> ((poly_stub_236)@(L(unit)))))[@inline] in
let originate_from_file#235 =
  fun _fn#4764 ->
  (fun _e#4765 ->
   (fun _v#4766 ->
    (fun _s#4767 -> (fun _t#4768 -> ((poly_stub_235)@(L(unit)))))))[@inline] in
let toto#236 = L(1) in
let get_balance#237 =
  fun _u#4771 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#238 =
  fun _u#4773 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#239 = fun _u#4775 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#240 =
  fun _u#4777 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#241 =
  fun _u#4779 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#242 = fun _u#4781 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#243 = fun _u#4783 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#244 =
  fun _u#4785 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#245 =
  fun _u#4787 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#246 =
  fun _u#4789 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#247 =
  fun kh#4791 -> (({ VOTING_POWER })@(kh#4791))[@inline] in
let implicit_account#249 =
  fun kh#4795 -> (IMPLICIT_ACCOUNT(kh#4795))[@inline] in
let pairing_check#253 =
  fun l#4803 -> (({ PAIRING_CHECK })@(l#4803))[@inline] in
let set_delegate#255 = fun o#4807 -> (SET_DELEGATE(o#4807))[@inline] in
let open_chest#261 =
  fun ck#4823 ->
  (fun c#4824 -> (fun n#4825 -> (OPEN_CHEST(ck#4823 , c#4824 , n#4825))))[@inline] in
let xor#264 =
  fun l#4834 -> (fun r#4835 -> (XOR(l#4834 , r#4835)))[@inline] in
let shift_left#265 =
  fun l#4837 -> (fun r#4838 -> (LSL(l#4837 , r#4838)))[@inline] in
let shift_right#266 =
  fun l#4840 -> (fun r#4841 -> (LSR(l#4840 , r#4841)))[@inline] in
let length#307 = fun b#4973 -> (({ SIZE })@(b#4973))[@inline] in
let concat#308 =
  fun b1#4975 ->
  (fun b2#4976 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4975 , b2#4976))))[@inline] in
let sub#309 =
  fun s#4978 ->
  (fun l#4979 ->
   (fun b#4980 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4978 ,
                                                                   l#4979) ,
                                                              b#4980)))))[@inline] in
let length#315 = fun b#4995 -> (({ SIZE })@(b#4995))[@inline] in
let concat#316 =
  fun b1#4997 ->
  (fun b2#4998 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4997 , b2#4998))))[@inline] in
let sub#317 =
  fun s#5000 ->
  (fun l#5001 ->
   (fun b#5002 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5000 ,
                                                                   l#5001) ,
                                                              b#5002)))))[@inline] in
let blake2b#318 = fun b#5004 -> (({ BLAKE2B })@(b#5004))[@inline] in
let sha256#319 = fun b#5006 -> (({ SHA256 })@(b#5006))[@inline] in
let sha512#320 = fun b#5008 -> (({ SHA512 })@(b#5008))[@inline] in
let sha3#321 = fun b#5010 -> (({ SHA3 })@(b#5010))[@inline] in
let keccak#322 = fun b#5012 -> (({ KECCAK })@(b#5012))[@inline] in
let hash_key#323 = fun k#5014 -> (({ HASH_KEY })@(k#5014))[@inline] in
let check#324 =
  fun k#5016 ->
  (fun s#5017 ->
   (fun b#5018 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5016 , s#5017) ,
                                                   b#5018)))))[@inline] in
let assert#325 =
  fun b#5020 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5020))[@inline] in
let abs#328 = fun i#5026 -> (({ ABS })@(i#5026))[@inline] in
let is_nat#329 = fun i#5028 -> (({ ISNAT })@(i#5028))[@inline] in
let true#330 = TRUE()[@inline] in
let false#331 = FALSE()[@inline] in
let unit#332 = UNIT()[@inline] in
let assert_with_error#335 =
  fun b#5036 ->
  (fun s#5037 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5036 , s#5037))))[@inline] in
let poly_stub_234 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_233 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_232 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_231 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_230 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_229 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_228 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_227 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_226 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_225 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_224 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_223 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_222 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_221 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_220 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_219 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_218 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_217 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_216 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_215 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_214 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_213 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_212 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_211 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_210 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_209 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_208 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_207 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_206 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_205 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_204 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_203 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_202 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_201 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_200 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_199 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_198 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_197 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let poly_stub_196 = fun x#5048 -> (({ FAILWITH })@(x#5048))[@inline] in
let get_total_voting_power#343 =
  fun _u#5057 -> ((poly_stub_234)@(L(unit)))[@inline] in
let set_source#346 = fun _a#5063 -> ((poly_stub_233)@(L(unit)))[@inline] in
let get_storage_of_address#347 =
  fun _a#5065 -> ((poly_stub_232)@(L(unit)))[@inline] in
let get_balance#348 = fun _a#5067 -> ((poly_stub_231)@(L(unit)))[@inline] in
let print#349 = fun _v#5069 -> ((poly_stub_230)@(L(unit)))[@inline] in
let eprint#350 = fun _v#5071 -> ((poly_stub_229)@(L(unit)))[@inline] in
let get_voting_power#351 =
  fun _kh#5073 -> ((poly_stub_228)@(L(unit)))[@inline] in
let nth_bootstrap_contract#352 =
  fun _i#5075 -> ((poly_stub_227)@(L(unit)))[@inline] in
let nth_bootstrap_account#353 =
  fun _i#5077 -> ((poly_stub_226)@(L(unit)))[@inline] in
let get_bootstrap_account#354 =
  fun _n#5079 -> ((poly_stub_225)@(L(unit)))[@inline] in
let last_originations#356 =
  fun _u#5083 -> ((poly_stub_224)@(L(unit)))[@inline] in
let new_account#358 = fun _u#5087 -> ((poly_stub_223)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#360 =
  fun _n#5091 -> ((poly_stub_222)@(L(unit)))[@inline] in
let register_delegate#362 =
  fun _kh#5095 -> ((poly_stub_221)@(L(unit)))[@inline] in
let register_constant#363 =
  fun _m#5097 -> ((poly_stub_220)@(L(unit)))[@inline] in
let constant_to_michelson_program#365 =
  fun _s#5101 -> ((poly_stub_219)@(L(unit)))[@inline] in
let restore_context#366 =
  fun _u#5103 -> ((poly_stub_218)@(L(unit)))[@inline] in
let save_context#367 = fun _u#5105 -> ((poly_stub_217)@(L(unit)))[@inline] in
let drop_context#368 = fun _u#5107 -> ((poly_stub_216)@(L(unit)))[@inline] in
let set_baker_policy#371 =
  fun _bp#5113 -> ((poly_stub_215)@(L(unit)))[@inline] in
let set_baker#372 = fun _a#5115 -> ((poly_stub_214)@(L(unit)))[@inline] in
let size#373 = fun _c#5117 -> ((poly_stub_213)@(L(unit)))[@inline] in
let read_contract_from_file#375 =
  fun _fn#5121 -> ((poly_stub_212)@(L(unit)))[@inline] in
let chr#376 = fun _n#5123 -> ((poly_stub_211)@(L(unit)))[@inline] in
let nl#377 = L("NEWLINE")[@inline] in
let println#378 = fun _v#5126 -> ((poly_stub_210)@(L(unit)))[@inline] in
let transfer#379 =
  fun _a#5128 ->
  (fun _s#5129 -> (fun _t#5130 -> ((poly_stub_209)@(L(unit)))))[@inline] in
let transfer_exn#380 =
  fun _a#5132 ->
  (fun _s#5133 -> (fun _t#5134 -> ((poly_stub_208)@(L(unit)))))[@inline] in
let reset_state#382 =
  fun _n#5138 -> (fun _l#5139 -> ((poly_stub_207)@(L(unit))))[@inline] in
let reset_state_at#383 =
  fun _t#5141 ->
  (fun _n#5142 -> (fun _l#5143 -> ((poly_stub_206)@(L(unit)))))[@inline] in
let save_mutation#386 =
  fun _s#5152 -> (fun _m#5153 -> ((poly_stub_205)@(L(unit))))[@inline] in
let sign#389 =
  fun _sk#5161 -> (fun _d#5162 -> ((poly_stub_204)@(L(unit))))[@inline] in
let add_account#390 =
  fun _s#5164 -> (fun _k#5165 -> ((poly_stub_203)@(L(unit))))[@inline] in
let baker_account#391 =
  fun _p#5167 -> (fun _o#5168 -> ((poly_stub_202)@(L(unit))))[@inline] in
let create_chest#393 =
  fun _b#5173 -> (fun _n#5174 -> ((poly_stub_201)@(L(unit))))[@inline] in
let create_chest_key#394 =
  fun _c#5176 -> (fun _n#5177 -> ((poly_stub_200)@(L(unit))))[@inline] in
let michelson_equal#397 =
  fun _m1#5187 -> (fun _m2#5188 -> ((poly_stub_199)@(L(unit))))[@inline] in
let originate_contract#399 =
  fun _c#5193 ->
  (fun _s#5194 -> (fun _t#5195 -> ((poly_stub_198)@(L(unit)))))[@inline] in
let compile_contract_from_file#401 =
  fun _fn#5201 ->
  (fun _e#5202 -> (fun _v#5203 -> ((poly_stub_197)@(L(unit)))))[@inline] in
let originate_from_file#402 =
  fun _fn#5205 ->
  (fun _e#5206 ->
   (fun _v#5207 ->
    (fun _s#5208 -> (fun _t#5209 -> ((poly_stub_196)@(L(unit)))))))[@inline] in
let toto#403 = L(32) in
let titi#404 = ADD(toto#236 , L(42)) in
let f#405 =
  fun gen#5213 ->
  (let (gen#7430, gen#7431) = gen#5213 in
   let gen#5214 = gen#7430 in
   let x#5215 = gen#7431 in
   let x#5216 = ADD(ADD(x#5215 , toto#236) , titi#404) in
   PAIR(LIST_EMPTY() , x#5216)) in
let get_balance#406 =
  fun _u#5218 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#407 =
  fun _u#5220 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#408 = fun _u#5222 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#409 =
  fun _u#5224 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#410 =
  fun _u#5226 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#411 = fun _u#5228 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#412 = fun _u#5230 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#413 =
  fun _u#5232 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#414 =
  fun _u#5234 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#415 =
  fun _u#5236 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#416 =
  fun kh#5238 -> (({ VOTING_POWER })@(kh#5238))[@inline] in
let implicit_account#418 =
  fun kh#5242 -> (IMPLICIT_ACCOUNT(kh#5242))[@inline] in
let pairing_check#422 =
  fun l#5250 -> (({ PAIRING_CHECK })@(l#5250))[@inline] in
let set_delegate#424 = fun o#5254 -> (SET_DELEGATE(o#5254))[@inline] in
let open_chest#430 =
  fun ck#5270 ->
  (fun c#5271 -> (fun n#5272 -> (OPEN_CHEST(ck#5270 , c#5271 , n#5272))))[@inline] in
let xor#433 =
  fun l#5281 -> (fun r#5282 -> (XOR(l#5281 , r#5282)))[@inline] in
let shift_left#434 =
  fun l#5284 -> (fun r#5285 -> (LSL(l#5284 , r#5285)))[@inline] in
let shift_right#435 =
  fun l#5287 -> (fun r#5288 -> (LSR(l#5287 , r#5288)))[@inline] in
let length#476 = fun b#5420 -> (({ SIZE })@(b#5420))[@inline] in
let concat#477 =
  fun b1#5422 ->
  (fun b2#5423 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5422 , b2#5423))))[@inline] in
let sub#478 =
  fun s#5425 ->
  (fun l#5426 ->
   (fun b#5427 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5425 ,
                                                                   l#5426) ,
                                                              b#5427)))))[@inline] in
let length#484 = fun b#5442 -> (({ SIZE })@(b#5442))[@inline] in
let concat#485 =
  fun b1#5444 ->
  (fun b2#5445 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5444 , b2#5445))))[@inline] in
let sub#486 =
  fun s#5447 ->
  (fun l#5448 ->
   (fun b#5449 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5447 ,
                                                                   l#5448) ,
                                                              b#5449)))))[@inline] in
let blake2b#487 = fun b#5451 -> (({ BLAKE2B })@(b#5451))[@inline] in
let sha256#488 = fun b#5453 -> (({ SHA256 })@(b#5453))[@inline] in
let sha512#489 = fun b#5455 -> (({ SHA512 })@(b#5455))[@inline] in
let sha3#490 = fun b#5457 -> (({ SHA3 })@(b#5457))[@inline] in
let keccak#491 = fun b#5459 -> (({ KECCAK })@(b#5459))[@inline] in
let hash_key#492 = fun k#5461 -> (({ HASH_KEY })@(k#5461))[@inline] in
let check#493 =
  fun k#5463 ->
  (fun s#5464 ->
   (fun b#5465 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5463 , s#5464) ,
                                                   b#5465)))))[@inline] in
let assert#494 =
  fun b#5467 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5467))[@inline] in
let abs#497 = fun i#5473 -> (({ ABS })@(i#5473))[@inline] in
let is_nat#498 = fun i#5475 -> (({ ISNAT })@(i#5475))[@inline] in
let true#499 = TRUE()[@inline] in
let false#500 = FALSE()[@inline] in
let unit#501 = UNIT()[@inline] in
let assert_with_error#504 =
  fun b#5483 ->
  (fun s#5484 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5483 , s#5484))))[@inline] in
let poly_stub_195 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_194 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_193 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_192 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_191 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_190 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_189 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_188 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_187 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_186 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_185 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_184 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_183 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_182 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_181 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_180 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_179 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_178 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_177 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_176 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_175 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_174 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_173 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_172 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_171 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_170 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_169 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_168 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_167 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_166 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_165 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_164 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_163 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_162 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_161 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_160 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_159 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_158 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let poly_stub_157 = fun x#5495 -> (({ FAILWITH })@(x#5495))[@inline] in
let get_total_voting_power#512 =
  fun _u#5504 -> ((poly_stub_195)@(L(unit)))[@inline] in
let set_source#515 = fun _a#5510 -> ((poly_stub_194)@(L(unit)))[@inline] in
let get_storage_of_address#516 =
  fun _a#5512 -> ((poly_stub_193)@(L(unit)))[@inline] in
let get_balance#517 = fun _a#5514 -> ((poly_stub_192)@(L(unit)))[@inline] in
let print#518 = fun _v#5516 -> ((poly_stub_191)@(L(unit)))[@inline] in
let eprint#519 = fun _v#5518 -> ((poly_stub_190)@(L(unit)))[@inline] in
let get_voting_power#520 =
  fun _kh#5520 -> ((poly_stub_189)@(L(unit)))[@inline] in
let nth_bootstrap_contract#521 =
  fun _i#5522 -> ((poly_stub_188)@(L(unit)))[@inline] in
let nth_bootstrap_account#522 =
  fun _i#5524 -> ((poly_stub_187)@(L(unit)))[@inline] in
let get_bootstrap_account#523 =
  fun _n#5526 -> ((poly_stub_186)@(L(unit)))[@inline] in
let last_originations#525 =
  fun _u#5530 -> ((poly_stub_185)@(L(unit)))[@inline] in
let new_account#527 = fun _u#5534 -> ((poly_stub_184)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#529 =
  fun _n#5538 -> ((poly_stub_183)@(L(unit)))[@inline] in
let register_delegate#531 =
  fun _kh#5542 -> ((poly_stub_182)@(L(unit)))[@inline] in
let register_constant#532 =
  fun _m#5544 -> ((poly_stub_181)@(L(unit)))[@inline] in
let constant_to_michelson_program#534 =
  fun _s#5548 -> ((poly_stub_180)@(L(unit)))[@inline] in
let restore_context#535 =
  fun _u#5550 -> ((poly_stub_179)@(L(unit)))[@inline] in
let save_context#536 = fun _u#5552 -> ((poly_stub_178)@(L(unit)))[@inline] in
let drop_context#537 = fun _u#5554 -> ((poly_stub_177)@(L(unit)))[@inline] in
let set_baker_policy#540 =
  fun _bp#5560 -> ((poly_stub_176)@(L(unit)))[@inline] in
let set_baker#541 = fun _a#5562 -> ((poly_stub_175)@(L(unit)))[@inline] in
let size#542 = fun _c#5564 -> ((poly_stub_174)@(L(unit)))[@inline] in
let read_contract_from_file#544 =
  fun _fn#5568 -> ((poly_stub_173)@(L(unit)))[@inline] in
let chr#545 = fun _n#5570 -> ((poly_stub_172)@(L(unit)))[@inline] in
let nl#546 = L("NEWLINE")[@inline] in
let println#547 = fun _v#5573 -> ((poly_stub_171)@(L(unit)))[@inline] in
let transfer#548 =
  fun _a#5575 ->
  (fun _s#5576 -> (fun _t#5577 -> ((poly_stub_170)@(L(unit)))))[@inline] in
let transfer_exn#549 =
  fun _a#5579 ->
  (fun _s#5580 -> (fun _t#5581 -> ((poly_stub_169)@(L(unit)))))[@inline] in
let reset_state#551 =
  fun _n#5585 -> (fun _l#5586 -> ((poly_stub_168)@(L(unit))))[@inline] in
let reset_state_at#552 =
  fun _t#5588 ->
  (fun _n#5589 -> (fun _l#5590 -> ((poly_stub_167)@(L(unit)))))[@inline] in
let save_mutation#555 =
  fun _s#5599 -> (fun _m#5600 -> ((poly_stub_166)@(L(unit))))[@inline] in
let sign#558 =
  fun _sk#5608 -> (fun _d#5609 -> ((poly_stub_165)@(L(unit))))[@inline] in
let add_account#559 =
  fun _s#5611 -> (fun _k#5612 -> ((poly_stub_164)@(L(unit))))[@inline] in
let baker_account#560 =
  fun _p#5614 -> (fun _o#5615 -> ((poly_stub_163)@(L(unit))))[@inline] in
let create_chest#562 =
  fun _b#5620 -> (fun _n#5621 -> ((poly_stub_162)@(L(unit))))[@inline] in
let create_chest_key#563 =
  fun _c#5623 -> (fun _n#5624 -> ((poly_stub_161)@(L(unit))))[@inline] in
let michelson_equal#566 =
  fun _m1#5634 -> (fun _m2#5635 -> ((poly_stub_160)@(L(unit))))[@inline] in
let originate_contract#568 =
  fun _c#5640 ->
  (fun _s#5641 -> (fun _t#5642 -> ((poly_stub_159)@(L(unit)))))[@inline] in
let compile_contract_from_file#570 =
  fun _fn#5648 ->
  (fun _e#5649 -> (fun _v#5650 -> ((poly_stub_158)@(L(unit)))))[@inline] in
let originate_from_file#571 =
  fun _fn#5652 ->
  (fun _e#5653 ->
   (fun _v#5654 ->
    (fun _s#5655 -> (fun _t#5656 -> ((poly_stub_157)@(L(unit)))))))[@inline] in
let toto#572 = L(44) in
let get_balance#573 =
  fun _u#5659 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#574 =
  fun _u#5661 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#575 = fun _u#5663 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#576 =
  fun _u#5665 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#577 =
  fun _u#5667 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#578 = fun _u#5669 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#579 = fun _u#5671 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#580 =
  fun _u#5673 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#581 =
  fun _u#5675 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#582 =
  fun _u#5677 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#583 =
  fun kh#5679 -> (({ VOTING_POWER })@(kh#5679))[@inline] in
let implicit_account#585 =
  fun kh#5683 -> (IMPLICIT_ACCOUNT(kh#5683))[@inline] in
let pairing_check#589 =
  fun l#5691 -> (({ PAIRING_CHECK })@(l#5691))[@inline] in
let set_delegate#591 = fun o#5695 -> (SET_DELEGATE(o#5695))[@inline] in
let open_chest#597 =
  fun ck#5711 ->
  (fun c#5712 -> (fun n#5713 -> (OPEN_CHEST(ck#5711 , c#5712 , n#5713))))[@inline] in
let xor#600 =
  fun l#5722 -> (fun r#5723 -> (XOR(l#5722 , r#5723)))[@inline] in
let shift_left#601 =
  fun l#5725 -> (fun r#5726 -> (LSL(l#5725 , r#5726)))[@inline] in
let shift_right#602 =
  fun l#5728 -> (fun r#5729 -> (LSR(l#5728 , r#5729)))[@inline] in
let length#643 = fun b#5861 -> (({ SIZE })@(b#5861))[@inline] in
let concat#644 =
  fun b1#5863 ->
  (fun b2#5864 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5863 , b2#5864))))[@inline] in
let sub#645 =
  fun s#5866 ->
  (fun l#5867 ->
   (fun b#5868 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5866 ,
                                                                   l#5867) ,
                                                              b#5868)))))[@inline] in
let length#651 = fun b#5883 -> (({ SIZE })@(b#5883))[@inline] in
let concat#652 =
  fun b1#5885 ->
  (fun b2#5886 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5885 , b2#5886))))[@inline] in
let sub#653 =
  fun s#5888 ->
  (fun l#5889 ->
   (fun b#5890 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5888 ,
                                                                   l#5889) ,
                                                              b#5890)))))[@inline] in
let blake2b#654 = fun b#5892 -> (({ BLAKE2B })@(b#5892))[@inline] in
let sha256#655 = fun b#5894 -> (({ SHA256 })@(b#5894))[@inline] in
let sha512#656 = fun b#5896 -> (({ SHA512 })@(b#5896))[@inline] in
let sha3#657 = fun b#5898 -> (({ SHA3 })@(b#5898))[@inline] in
let keccak#658 = fun b#5900 -> (({ KECCAK })@(b#5900))[@inline] in
let hash_key#659 = fun k#5902 -> (({ HASH_KEY })@(k#5902))[@inline] in
let check#660 =
  fun k#5904 ->
  (fun s#5905 ->
   (fun b#5906 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5904 , s#5905) ,
                                                   b#5906)))))[@inline] in
let assert#661 =
  fun b#5908 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5908))[@inline] in
let abs#664 = fun i#5914 -> (({ ABS })@(i#5914))[@inline] in
let is_nat#665 = fun i#5916 -> (({ ISNAT })@(i#5916))[@inline] in
let true#666 = TRUE()[@inline] in
let false#667 = FALSE()[@inline] in
let unit#668 = UNIT()[@inline] in
let assert_with_error#671 =
  fun b#5924 ->
  (fun s#5925 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5924 , s#5925))))[@inline] in
let poly_stub_156 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_155 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_154 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_153 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_152 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_151 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_150 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_149 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_148 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_147 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_146 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_145 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_144 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_143 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_142 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_141 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_140 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_139 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_138 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_137 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_136 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_135 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_134 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_133 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_132 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_131 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_130 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_129 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_128 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_127 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_126 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_125 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_124 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_123 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_122 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_121 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_120 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_119 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let poly_stub_118 = fun x#5936 -> (({ FAILWITH })@(x#5936))[@inline] in
let get_total_voting_power#679 =
  fun _u#5945 -> ((poly_stub_156)@(L(unit)))[@inline] in
let set_source#682 = fun _a#5951 -> ((poly_stub_155)@(L(unit)))[@inline] in
let get_storage_of_address#683 =
  fun _a#5953 -> ((poly_stub_154)@(L(unit)))[@inline] in
let get_balance#684 = fun _a#5955 -> ((poly_stub_153)@(L(unit)))[@inline] in
let print#685 = fun _v#5957 -> ((poly_stub_152)@(L(unit)))[@inline] in
let eprint#686 = fun _v#5959 -> ((poly_stub_151)@(L(unit)))[@inline] in
let get_voting_power#687 =
  fun _kh#5961 -> ((poly_stub_150)@(L(unit)))[@inline] in
let nth_bootstrap_contract#688 =
  fun _i#5963 -> ((poly_stub_149)@(L(unit)))[@inline] in
let nth_bootstrap_account#689 =
  fun _i#5965 -> ((poly_stub_148)@(L(unit)))[@inline] in
let get_bootstrap_account#690 =
  fun _n#5967 -> ((poly_stub_147)@(L(unit)))[@inline] in
let last_originations#692 =
  fun _u#5971 -> ((poly_stub_146)@(L(unit)))[@inline] in
let new_account#694 = fun _u#5975 -> ((poly_stub_145)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#696 =
  fun _n#5979 -> ((poly_stub_144)@(L(unit)))[@inline] in
let register_delegate#698 =
  fun _kh#5983 -> ((poly_stub_143)@(L(unit)))[@inline] in
let register_constant#699 =
  fun _m#5985 -> ((poly_stub_142)@(L(unit)))[@inline] in
let constant_to_michelson_program#701 =
  fun _s#5989 -> ((poly_stub_141)@(L(unit)))[@inline] in
let restore_context#702 =
  fun _u#5991 -> ((poly_stub_140)@(L(unit)))[@inline] in
let save_context#703 = fun _u#5993 -> ((poly_stub_139)@(L(unit)))[@inline] in
let drop_context#704 = fun _u#5995 -> ((poly_stub_138)@(L(unit)))[@inline] in
let set_baker_policy#707 =
  fun _bp#6001 -> ((poly_stub_137)@(L(unit)))[@inline] in
let set_baker#708 = fun _a#6003 -> ((poly_stub_136)@(L(unit)))[@inline] in
let size#709 = fun _c#6005 -> ((poly_stub_135)@(L(unit)))[@inline] in
let read_contract_from_file#711 =
  fun _fn#6009 -> ((poly_stub_134)@(L(unit)))[@inline] in
let chr#712 = fun _n#6011 -> ((poly_stub_133)@(L(unit)))[@inline] in
let nl#713 = L("NEWLINE")[@inline] in
let println#714 = fun _v#6014 -> ((poly_stub_132)@(L(unit)))[@inline] in
let transfer#715 =
  fun _a#6016 ->
  (fun _s#6017 -> (fun _t#6018 -> ((poly_stub_131)@(L(unit)))))[@inline] in
let transfer_exn#716 =
  fun _a#6020 ->
  (fun _s#6021 -> (fun _t#6022 -> ((poly_stub_130)@(L(unit)))))[@inline] in
let reset_state#718 =
  fun _n#6026 -> (fun _l#6027 -> ((poly_stub_129)@(L(unit))))[@inline] in
let reset_state_at#719 =
  fun _t#6029 ->
  (fun _n#6030 -> (fun _l#6031 -> ((poly_stub_128)@(L(unit)))))[@inline] in
let save_mutation#722 =
  fun _s#6040 -> (fun _m#6041 -> ((poly_stub_127)@(L(unit))))[@inline] in
let sign#725 =
  fun _sk#6049 -> (fun _d#6050 -> ((poly_stub_126)@(L(unit))))[@inline] in
let add_account#726 =
  fun _s#6052 -> (fun _k#6053 -> ((poly_stub_125)@(L(unit))))[@inline] in
let baker_account#727 =
  fun _p#6055 -> (fun _o#6056 -> ((poly_stub_124)@(L(unit))))[@inline] in
let create_chest#729 =
  fun _b#6061 -> (fun _n#6062 -> ((poly_stub_123)@(L(unit))))[@inline] in
let create_chest_key#730 =
  fun _c#6064 -> (fun _n#6065 -> ((poly_stub_122)@(L(unit))))[@inline] in
let michelson_equal#733 =
  fun _m1#6075 -> (fun _m2#6076 -> ((poly_stub_121)@(L(unit))))[@inline] in
let originate_contract#735 =
  fun _c#6081 ->
  (fun _s#6082 -> (fun _t#6083 -> ((poly_stub_120)@(L(unit)))))[@inline] in
let compile_contract_from_file#737 =
  fun _fn#6089 ->
  (fun _e#6090 -> (fun _v#6091 -> ((poly_stub_119)@(L(unit)))))[@inline] in
let originate_from_file#738 =
  fun _fn#6093 ->
  (fun _e#6094 ->
   (fun _v#6095 ->
    (fun _s#6096 -> (fun _t#6097 -> ((poly_stub_118)@(L(unit)))))))[@inline] in
let toto#739 = L(43) in
let get_balance#740 =
  fun _u#6100 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#741 =
  fun _u#6102 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#742 = fun _u#6104 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#743 =
  fun _u#6106 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#744 =
  fun _u#6108 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#745 = fun _u#6110 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#746 = fun _u#6112 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#747 =
  fun _u#6114 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#748 =
  fun _u#6116 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#749 =
  fun _u#6118 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#750 =
  fun kh#6120 -> (({ VOTING_POWER })@(kh#6120))[@inline] in
let implicit_account#752 =
  fun kh#6124 -> (IMPLICIT_ACCOUNT(kh#6124))[@inline] in
let pairing_check#756 =
  fun l#6132 -> (({ PAIRING_CHECK })@(l#6132))[@inline] in
let set_delegate#758 = fun o#6136 -> (SET_DELEGATE(o#6136))[@inline] in
let open_chest#764 =
  fun ck#6152 ->
  (fun c#6153 -> (fun n#6154 -> (OPEN_CHEST(ck#6152 , c#6153 , n#6154))))[@inline] in
let xor#767 =
  fun l#6163 -> (fun r#6164 -> (XOR(l#6163 , r#6164)))[@inline] in
let shift_left#768 =
  fun l#6166 -> (fun r#6167 -> (LSL(l#6166 , r#6167)))[@inline] in
let shift_right#769 =
  fun l#6169 -> (fun r#6170 -> (LSR(l#6169 , r#6170)))[@inline] in
let length#810 = fun b#6302 -> (({ SIZE })@(b#6302))[@inline] in
let concat#811 =
  fun b1#6304 ->
  (fun b2#6305 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6304 , b2#6305))))[@inline] in
let sub#812 =
  fun s#6307 ->
  (fun l#6308 ->
   (fun b#6309 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6307 ,
                                                                   l#6308) ,
                                                              b#6309)))))[@inline] in
let length#818 = fun b#6324 -> (({ SIZE })@(b#6324))[@inline] in
let concat#819 =
  fun b1#6326 ->
  (fun b2#6327 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6326 , b2#6327))))[@inline] in
let sub#820 =
  fun s#6329 ->
  (fun l#6330 ->
   (fun b#6331 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6329 ,
                                                                   l#6330) ,
                                                              b#6331)))))[@inline] in
let blake2b#821 = fun b#6333 -> (({ BLAKE2B })@(b#6333))[@inline] in
let sha256#822 = fun b#6335 -> (({ SHA256 })@(b#6335))[@inline] in
let sha512#823 = fun b#6337 -> (({ SHA512 })@(b#6337))[@inline] in
let sha3#824 = fun b#6339 -> (({ SHA3 })@(b#6339))[@inline] in
let keccak#825 = fun b#6341 -> (({ KECCAK })@(b#6341))[@inline] in
let hash_key#826 = fun k#6343 -> (({ HASH_KEY })@(k#6343))[@inline] in
let check#827 =
  fun k#6345 ->
  (fun s#6346 ->
   (fun b#6347 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6345 , s#6346) ,
                                                   b#6347)))))[@inline] in
let assert#828 =
  fun b#6349 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6349))[@inline] in
let abs#831 = fun i#6355 -> (({ ABS })@(i#6355))[@inline] in
let is_nat#832 = fun i#6357 -> (({ ISNAT })@(i#6357))[@inline] in
let true#833 = TRUE()[@inline] in
let false#834 = FALSE()[@inline] in
let unit#835 = UNIT()[@inline] in
let assert_with_error#838 =
  fun b#6365 ->
  (fun s#6366 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6365 , s#6366))))[@inline] in
let poly_stub_117 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_116 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_115 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_114 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_113 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_112 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_111 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_110 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_109 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_108 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_107 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_106 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_105 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_104 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_103 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_102 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_101 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_100 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_99 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_98 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_97 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_96 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_95 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_94 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_93 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_92 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_91 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_90 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_89 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_88 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_87 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_86 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_85 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_84 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_83 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_82 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_81 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_80 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let poly_stub_79 = fun x#6377 -> (({ FAILWITH })@(x#6377))[@inline] in
let get_total_voting_power#846 =
  fun _u#6386 -> ((poly_stub_117)@(L(unit)))[@inline] in
let set_source#849 = fun _a#6392 -> ((poly_stub_116)@(L(unit)))[@inline] in
let get_storage_of_address#850 =
  fun _a#6394 -> ((poly_stub_115)@(L(unit)))[@inline] in
let get_balance#851 = fun _a#6396 -> ((poly_stub_114)@(L(unit)))[@inline] in
let print#852 = fun _v#6398 -> ((poly_stub_113)@(L(unit)))[@inline] in
let eprint#853 = fun _v#6400 -> ((poly_stub_112)@(L(unit)))[@inline] in
let get_voting_power#854 =
  fun _kh#6402 -> ((poly_stub_111)@(L(unit)))[@inline] in
let nth_bootstrap_contract#855 =
  fun _i#6404 -> ((poly_stub_110)@(L(unit)))[@inline] in
let nth_bootstrap_account#856 =
  fun _i#6406 -> ((poly_stub_109)@(L(unit)))[@inline] in
let get_bootstrap_account#857 =
  fun _n#6408 -> ((poly_stub_108)@(L(unit)))[@inline] in
let last_originations#859 =
  fun _u#6412 -> ((poly_stub_107)@(L(unit)))[@inline] in
let new_account#861 = fun _u#6416 -> ((poly_stub_106)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#863 =
  fun _n#6420 -> ((poly_stub_105)@(L(unit)))[@inline] in
let register_delegate#865 =
  fun _kh#6424 -> ((poly_stub_104)@(L(unit)))[@inline] in
let register_constant#866 =
  fun _m#6426 -> ((poly_stub_103)@(L(unit)))[@inline] in
let constant_to_michelson_program#868 =
  fun _s#6430 -> ((poly_stub_102)@(L(unit)))[@inline] in
let restore_context#869 =
  fun _u#6432 -> ((poly_stub_101)@(L(unit)))[@inline] in
let save_context#870 = fun _u#6434 -> ((poly_stub_100)@(L(unit)))[@inline] in
let drop_context#871 = fun _u#6436 -> ((poly_stub_99)@(L(unit)))[@inline] in
let set_baker_policy#874 =
  fun _bp#6442 -> ((poly_stub_98)@(L(unit)))[@inline] in
let set_baker#875 = fun _a#6444 -> ((poly_stub_97)@(L(unit)))[@inline] in
let size#876 = fun _c#6446 -> ((poly_stub_96)@(L(unit)))[@inline] in
let read_contract_from_file#878 =
  fun _fn#6450 -> ((poly_stub_95)@(L(unit)))[@inline] in
let chr#879 = fun _n#6452 -> ((poly_stub_94)@(L(unit)))[@inline] in
let nl#880 = L("NEWLINE")[@inline] in
let println#881 = fun _v#6455 -> ((poly_stub_93)@(L(unit)))[@inline] in
let transfer#882 =
  fun _a#6457 -> (fun _s#6458 -> (fun _t#6459 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let transfer_exn#883 =
  fun _a#6461 -> (fun _s#6462 -> (fun _t#6463 -> ((poly_stub_91)@(L(unit)))))[@inline] in
let reset_state#885 =
  fun _n#6467 -> (fun _l#6468 -> ((poly_stub_90)@(L(unit))))[@inline] in
let reset_state_at#886 =
  fun _t#6470 -> (fun _n#6471 -> (fun _l#6472 -> ((poly_stub_89)@(L(unit)))))[@inline] in
let save_mutation#889 =
  fun _s#6481 -> (fun _m#6482 -> ((poly_stub_88)@(L(unit))))[@inline] in
let sign#892 =
  fun _sk#6490 -> (fun _d#6491 -> ((poly_stub_87)@(L(unit))))[@inline] in
let add_account#893 =
  fun _s#6493 -> (fun _k#6494 -> ((poly_stub_86)@(L(unit))))[@inline] in
let baker_account#894 =
  fun _p#6496 -> (fun _o#6497 -> ((poly_stub_85)@(L(unit))))[@inline] in
let create_chest#896 =
  fun _b#6502 -> (fun _n#6503 -> ((poly_stub_84)@(L(unit))))[@inline] in
let create_chest_key#897 =
  fun _c#6505 -> (fun _n#6506 -> ((poly_stub_83)@(L(unit))))[@inline] in
let michelson_equal#900 =
  fun _m1#6516 -> (fun _m2#6517 -> ((poly_stub_82)@(L(unit))))[@inline] in
let originate_contract#902 =
  fun _c#6522 -> (fun _s#6523 -> (fun _t#6524 -> ((poly_stub_81)@(L(unit)))))[@inline] in
let compile_contract_from_file#904 =
  fun _fn#6530 ->
  (fun _e#6531 -> (fun _v#6532 -> ((poly_stub_80)@(L(unit)))))[@inline] in
let originate_from_file#905 =
  fun _fn#6534 ->
  (fun _e#6535 ->
   (fun _v#6536 ->
    (fun _s#6537 -> (fun _t#6538 -> ((poly_stub_79)@(L(unit)))))))[@inline] in
let tata#906 = ADD(toto#236 , titi#404) in
let foo#907 = (f#405)@(PAIR(L(unit) , L(3))) in
let get_balance#908 =
  fun _u#6542 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#909 =
  fun _u#6544 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#910 = fun _u#6546 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#911 =
  fun _u#6548 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#912 =
  fun _u#6550 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#913 = fun _u#6552 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#914 = fun _u#6554 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#915 =
  fun _u#6556 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#916 =
  fun _u#6558 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#917 =
  fun _u#6560 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#918 =
  fun kh#6562 -> (({ VOTING_POWER })@(kh#6562))[@inline] in
let implicit_account#920 =
  fun kh#6566 -> (IMPLICIT_ACCOUNT(kh#6566))[@inline] in
let pairing_check#924 =
  fun l#6574 -> (({ PAIRING_CHECK })@(l#6574))[@inline] in
let set_delegate#926 = fun o#6578 -> (SET_DELEGATE(o#6578))[@inline] in
let open_chest#932 =
  fun ck#6594 ->
  (fun c#6595 -> (fun n#6596 -> (OPEN_CHEST(ck#6594 , c#6595 , n#6596))))[@inline] in
let xor#935 =
  fun l#6605 -> (fun r#6606 -> (XOR(l#6605 , r#6606)))[@inline] in
let shift_left#936 =
  fun l#6608 -> (fun r#6609 -> (LSL(l#6608 , r#6609)))[@inline] in
let shift_right#937 =
  fun l#6611 -> (fun r#6612 -> (LSR(l#6611 , r#6612)))[@inline] in
let length#978 = fun b#6744 -> (({ SIZE })@(b#6744))[@inline] in
let concat#979 =
  fun b1#6746 ->
  (fun b2#6747 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6746 , b2#6747))))[@inline] in
let sub#980 =
  fun s#6749 ->
  (fun l#6750 ->
   (fun b#6751 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6749 ,
                                                                   l#6750) ,
                                                              b#6751)))))[@inline] in
let length#986 = fun b#6766 -> (({ SIZE })@(b#6766))[@inline] in
let concat#987 =
  fun b1#6768 ->
  (fun b2#6769 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6768 , b2#6769))))[@inline] in
let sub#988 =
  fun s#6771 ->
  (fun l#6772 ->
   (fun b#6773 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6771 ,
                                                                   l#6772) ,
                                                              b#6773)))))[@inline] in
let blake2b#989 = fun b#6775 -> (({ BLAKE2B })@(b#6775))[@inline] in
let sha256#990 = fun b#6777 -> (({ SHA256 })@(b#6777))[@inline] in
let sha512#991 = fun b#6779 -> (({ SHA512 })@(b#6779))[@inline] in
let sha3#992 = fun b#6781 -> (({ SHA3 })@(b#6781))[@inline] in
let keccak#993 = fun b#6783 -> (({ KECCAK })@(b#6783))[@inline] in
let hash_key#994 = fun k#6785 -> (({ HASH_KEY })@(k#6785))[@inline] in
let check#995 =
  fun k#6787 ->
  (fun s#6788 ->
   (fun b#6789 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6787 , s#6788) ,
                                                   b#6789)))))[@inline] in
let assert#996 =
  fun b#6791 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6791))[@inline] in
let abs#999 = fun i#6797 -> (({ ABS })@(i#6797))[@inline] in
let is_nat#1000 = fun i#6799 -> (({ ISNAT })@(i#6799))[@inline] in
let true#1001 = TRUE()[@inline] in
let false#1002 = FALSE()[@inline] in
let unit#1003 = UNIT()[@inline] in
let assert_with_error#1006 =
  fun b#6807 ->
  (fun s#6808 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6807 , s#6808))))[@inline] in
let poly_stub_78 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_77 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_76 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_75 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_74 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_73 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_72 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_71 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_70 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_69 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_68 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_67 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_66 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_65 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_64 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_63 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_62 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_61 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_60 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_59 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_58 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_57 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_56 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_55 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_54 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_53 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_52 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_51 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_50 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_49 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_48 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_47 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_46 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_45 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_44 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_43 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_42 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_41 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let poly_stub_40 = fun x#6819 -> (({ FAILWITH })@(x#6819))[@inline] in
let get_total_voting_power#1014 =
  fun _u#6828 -> ((poly_stub_78)@(L(unit)))[@inline] in
let set_source#1017 = fun _a#6834 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#1018 =
  fun _a#6836 -> ((poly_stub_76)@(L(unit)))[@inline] in
let get_balance#1019 = fun _a#6838 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#1020 = fun _v#6840 -> ((poly_stub_74)@(L(unit)))[@inline] in
let eprint#1021 = fun _v#6842 -> ((poly_stub_73)@(L(unit)))[@inline] in
let get_voting_power#1022 =
  fun _kh#6844 -> ((poly_stub_72)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1023 =
  fun _i#6846 -> ((poly_stub_71)@(L(unit)))[@inline] in
let nth_bootstrap_account#1024 =
  fun _i#6848 -> ((poly_stub_70)@(L(unit)))[@inline] in
let get_bootstrap_account#1025 =
  fun _n#6850 -> ((poly_stub_69)@(L(unit)))[@inline] in
let last_originations#1027 =
  fun _u#6854 -> ((poly_stub_68)@(L(unit)))[@inline] in
let new_account#1029 = fun _u#6858 -> ((poly_stub_67)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1031 =
  fun _n#6862 -> ((poly_stub_66)@(L(unit)))[@inline] in
let register_delegate#1033 =
  fun _kh#6866 -> ((poly_stub_65)@(L(unit)))[@inline] in
let register_constant#1034 =
  fun _m#6868 -> ((poly_stub_64)@(L(unit)))[@inline] in
let constant_to_michelson_program#1036 =
  fun _s#6872 -> ((poly_stub_63)@(L(unit)))[@inline] in
let restore_context#1037 =
  fun _u#6874 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#1038 = fun _u#6876 -> ((poly_stub_61)@(L(unit)))[@inline] in
let drop_context#1039 = fun _u#6878 -> ((poly_stub_60)@(L(unit)))[@inline] in
let set_baker_policy#1042 =
  fun _bp#6884 -> ((poly_stub_59)@(L(unit)))[@inline] in
let set_baker#1043 = fun _a#6886 -> ((poly_stub_58)@(L(unit)))[@inline] in
let size#1044 = fun _c#6888 -> ((poly_stub_57)@(L(unit)))[@inline] in
let read_contract_from_file#1046 =
  fun _fn#6892 -> ((poly_stub_56)@(L(unit)))[@inline] in
let chr#1047 = fun _n#6894 -> ((poly_stub_55)@(L(unit)))[@inline] in
let nl#1048 = L("NEWLINE")[@inline] in
let println#1049 = fun _v#6897 -> ((poly_stub_54)@(L(unit)))[@inline] in
let transfer#1050 =
  fun _a#6899 -> (fun _s#6900 -> (fun _t#6901 -> ((poly_stub_53)@(L(unit)))))[@inline] in
let transfer_exn#1051 =
  fun _a#6903 -> (fun _s#6904 -> (fun _t#6905 -> ((poly_stub_52)@(L(unit)))))[@inline] in
let reset_state#1053 =
  fun _n#6909 -> (fun _l#6910 -> ((poly_stub_51)@(L(unit))))[@inline] in
let reset_state_at#1054 =
  fun _t#6912 -> (fun _n#6913 -> (fun _l#6914 -> ((poly_stub_50)@(L(unit)))))[@inline] in
let save_mutation#1057 =
  fun _s#6923 -> (fun _m#6924 -> ((poly_stub_49)@(L(unit))))[@inline] in
let sign#1060 =
  fun _sk#6932 -> (fun _d#6933 -> ((poly_stub_48)@(L(unit))))[@inline] in
let add_account#1061 =
  fun _s#6935 -> (fun _k#6936 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#1062 =
  fun _p#6938 -> (fun _o#6939 -> ((poly_stub_46)@(L(unit))))[@inline] in
let create_chest#1064 =
  fun _b#6944 -> (fun _n#6945 -> ((poly_stub_45)@(L(unit))))[@inline] in
let create_chest_key#1065 =
  fun _c#6947 -> (fun _n#6948 -> ((poly_stub_44)@(L(unit))))[@inline] in
let michelson_equal#1068 =
  fun _m1#6958 -> (fun _m2#6959 -> ((poly_stub_43)@(L(unit))))[@inline] in
let originate_contract#1070 =
  fun _c#6964 -> (fun _s#6965 -> (fun _t#6966 -> ((poly_stub_42)@(L(unit)))))[@inline] in
let compile_contract_from_file#1072 =
  fun _fn#6972 ->
  (fun _e#6973 -> (fun _v#6974 -> ((poly_stub_41)@(L(unit)))))[@inline] in
let originate_from_file#1073 =
  fun _fn#6976 ->
  (fun _e#6977 ->
   (fun _v#6978 ->
    (fun _s#6979 -> (fun _t#6980 -> ((poly_stub_40)@(L(unit)))))))[@inline] in
let toto#1074 = L(10) in
let foo#1075 = L("bar") in
let get_balance#1076 =
  fun _u#6984 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1077 =
  fun _u#6986 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1078 = fun _u#6988 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1079 =
  fun _u#6990 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#1080 =
  fun _u#6992 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1081 =
  fun _u#6994 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1082 = fun _u#6996 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1083 =
  fun _u#6998 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1084 =
  fun _u#7000 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#1085 =
  fun _u#7002 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#1086 =
  fun kh#7004 -> (({ VOTING_POWER })@(kh#7004))[@inline] in
let implicit_account#1088 =
  fun kh#7008 -> (IMPLICIT_ACCOUNT(kh#7008))[@inline] in
let pairing_check#1092 =
  fun l#7016 -> (({ PAIRING_CHECK })@(l#7016))[@inline] in
let set_delegate#1094 = fun o#7020 -> (SET_DELEGATE(o#7020))[@inline] in
let open_chest#1100 =
  fun ck#7036 ->
  (fun c#7037 -> (fun n#7038 -> (OPEN_CHEST(ck#7036 , c#7037 , n#7038))))[@inline] in
let xor#1103 =
  fun l#7047 -> (fun r#7048 -> (XOR(l#7047 , r#7048)))[@inline] in
let shift_left#1104 =
  fun l#7050 -> (fun r#7051 -> (LSL(l#7050 , r#7051)))[@inline] in
let shift_right#1105 =
  fun l#7053 -> (fun r#7054 -> (LSR(l#7053 , r#7054)))[@inline] in
let length#1146 = fun b#7186 -> (({ SIZE })@(b#7186))[@inline] in
let concat#1147 =
  fun b1#7188 ->
  (fun b2#7189 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7188 , b2#7189))))[@inline] in
let sub#1148 =
  fun s#7191 ->
  (fun l#7192 ->
   (fun b#7193 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7191 ,
                                                                   l#7192) ,
                                                              b#7193)))))[@inline] in
let length#1154 = fun b#7208 -> (({ SIZE })@(b#7208))[@inline] in
let concat#1155 =
  fun b1#7210 ->
  (fun b2#7211 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7210 , b2#7211))))[@inline] in
let sub#1156 =
  fun s#7213 ->
  (fun l#7214 ->
   (fun b#7215 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7213 ,
                                                                   l#7214) ,
                                                              b#7215)))))[@inline] in
let blake2b#1157 = fun b#7217 -> (({ BLAKE2B })@(b#7217))[@inline] in
let sha256#1158 = fun b#7219 -> (({ SHA256 })@(b#7219))[@inline] in
let sha512#1159 = fun b#7221 -> (({ SHA512 })@(b#7221))[@inline] in
let sha3#1160 = fun b#7223 -> (({ SHA3 })@(b#7223))[@inline] in
let keccak#1161 = fun b#7225 -> (({ KECCAK })@(b#7225))[@inline] in
let hash_key#1162 = fun k#7227 -> (({ HASH_KEY })@(k#7227))[@inline] in
let check#1163 =
  fun k#7229 ->
  (fun s#7230 ->
   (fun b#7231 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7229 , s#7230) ,
                                                   b#7231)))))[@inline] in
let assert =
  fun b#7233 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7233))[@inline] in
let abs = fun i#7239 -> (({ ABS })@(i#7239))[@inline] in
let is_nat = fun i#7241 -> (({ ISNAT })@(i#7241))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b#7249 ->
  (fun s#7250 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7249 , s#7250))))[@inline] in
let poly_stub_39 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_38 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_37 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_36 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_35 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_34 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_33 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_32 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_31 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_30 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_29 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_28 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_27 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_26 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_25 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_24 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_23 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_22 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_21 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_20 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_19 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_18 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_17 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_16 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_15 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_14 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_13 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_12 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_11 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_10 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_9 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_8 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_7 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_6 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_5 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_4 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_3 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_2 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let poly_stub_1 = fun x#7261 -> (({ FAILWITH })@(x#7261))[@inline] in
let get_total_voting_power#1168 =
  fun _u#7270 -> ((poly_stub_39)@(L(unit)))[@inline] in
let set_source#1171 = fun _a#7276 -> ((poly_stub_38)@(L(unit)))[@inline] in
let get_storage_of_address#1172 =
  fun _a#7278 -> ((poly_stub_37)@(L(unit)))[@inline] in
let get_balance#1173 = fun _a#7280 -> ((poly_stub_36)@(L(unit)))[@inline] in
let print#1174 = fun _v#7282 -> ((poly_stub_35)@(L(unit)))[@inline] in
let eprint#1175 = fun _v#7284 -> ((poly_stub_34)@(L(unit)))[@inline] in
let get_voting_power#1176 =
  fun _kh#7286 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1177 =
  fun _i#7288 -> ((poly_stub_32)@(L(unit)))[@inline] in
let nth_bootstrap_account#1178 =
  fun _i#7290 -> ((poly_stub_31)@(L(unit)))[@inline] in
let get_bootstrap_account#1179 =
  fun _n#7292 -> ((poly_stub_30)@(L(unit)))[@inline] in
let last_originations#1181 =
  fun _u#7296 -> ((poly_stub_29)@(L(unit)))[@inline] in
let new_account#1183 = fun _u#7300 -> ((poly_stub_28)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1185 =
  fun _n#7304 -> ((poly_stub_27)@(L(unit)))[@inline] in
let register_delegate#1187 =
  fun _kh#7308 -> ((poly_stub_26)@(L(unit)))[@inline] in
let register_constant#1188 =
  fun _m#7310 -> ((poly_stub_25)@(L(unit)))[@inline] in
let constant_to_michelson_program#1190 =
  fun _s#7314 -> ((poly_stub_24)@(L(unit)))[@inline] in
let restore_context#1191 =
  fun _u#7316 -> ((poly_stub_23)@(L(unit)))[@inline] in
let save_context#1192 = fun _u#7318 -> ((poly_stub_22)@(L(unit)))[@inline] in
let drop_context#1193 = fun _u#7320 -> ((poly_stub_21)@(L(unit)))[@inline] in
let set_baker_policy#1196 =
  fun _bp#7326 -> ((poly_stub_20)@(L(unit)))[@inline] in
let set_baker#1197 = fun _a#7328 -> ((poly_stub_19)@(L(unit)))[@inline] in
let size#1198 = fun _c#7330 -> ((poly_stub_18)@(L(unit)))[@inline] in
let read_contract_from_file#1200 =
  fun _fn#7334 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#1201 = fun _n#7336 -> ((poly_stub_16)@(L(unit)))[@inline] in
let nl#1202 = L("NEWLINE")[@inline] in
let println#1203 = fun _v#7339 -> ((poly_stub_15)@(L(unit)))[@inline] in
let transfer#1204 =
  fun _a#7341 -> (fun _s#7342 -> (fun _t#7343 -> ((poly_stub_14)@(L(unit)))))[@inline] in
let transfer_exn#1205 =
  fun _a#7345 -> (fun _s#7346 -> (fun _t#7347 -> ((poly_stub_13)@(L(unit)))))[@inline] in
let reset_state#1207 =
  fun _n#7351 -> (fun _l#7352 -> ((poly_stub_12)@(L(unit))))[@inline] in
let reset_state_at#1208 =
  fun _t#7354 -> (fun _n#7355 -> (fun _l#7356 -> ((poly_stub_11)@(L(unit)))))[@inline] in
let save_mutation#1211 =
  fun _s#7365 -> (fun _m#7366 -> ((poly_stub_10)@(L(unit))))[@inline] in
let sign#1214 =
  fun _sk#7374 -> (fun _d#7375 -> ((poly_stub_9)@(L(unit))))[@inline] in
let add_account#1215 =
  fun _s#7377 -> (fun _k#7378 -> ((poly_stub_8)@(L(unit))))[@inline] in
let baker_account#1216 =
  fun _p#7380 -> (fun _o#7381 -> ((poly_stub_7)@(L(unit))))[@inline] in
let create_chest#1218 =
  fun _b#7386 -> (fun _n#7387 -> ((poly_stub_6)@(L(unit))))[@inline] in
let create_chest_key#1219 =
  fun _c#7389 -> (fun _n#7390 -> ((poly_stub_5)@(L(unit))))[@inline] in
let michelson_equal#1222 =
  fun _m1#7400 -> (fun _m2#7401 -> ((poly_stub_4)@(L(unit))))[@inline] in
let originate_contract#1224 =
  fun _c#7406 -> (fun _s#7407 -> (fun _t#7408 -> ((poly_stub_3)@(L(unit)))))[@inline] in
let compile_contract_from_file#1226 =
  fun _fn#7414 -> (fun _e#7415 -> (fun _v#7416 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let originate_from_file#1227 =
  fun _fn#7418 ->
  (fun _e#7419 ->
   (fun _v#7420 ->
    (fun _s#7421 -> (fun _t#7422 -> ((poly_stub_1)@(L(unit)))))))[@inline] in
let toto = ADD(toto#1074 , toto#236) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#7426 ->
  (let (gen#7432, gen#7433) = gen#7426 in
   let p#7427 = gen#7432 in
   let s#7428 = gen#7433 in
   let s#7429 = ADD(ADD(p#7427 , s#7428) , toto) in
   PAIR(LIST_EMPTY() , s#7429)) in
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
    File "../../test/contracts/build/module_scoping_bug.mligo", line 24, characters 8-13:
     23 |
     24 | let x = B.A.a

    Module "A" not found. |}]
