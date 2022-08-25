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
    const toto = ADD(E.toto , C.B.A.toto)
    const fb = record[tata -> 2 , tete -> 3 , titi -> 1 , toto -> toto]
    const main =
      lambda (gen#5 : ( int * int )) return  match gen#5 with
                                              | ( p , s ) ->
                                              let s : int = ADD(ADD(p , s) ,
                                              toto) in ( LIST_EMPTY() , s ) |}]

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
    const main =
      lambda (gen#2 : ( unit * string )) return  match gen#2 with
                                                  | ( _#4 , _#3 ) ->
                                                  ( LIST_EMPTY() , CONCAT(Errors.undefined_token , Storage.s) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let get_balance#63 =
  fun _u#4288 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#64 =
  fun _u#4290 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#65 = fun _u#4292 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#66 =
  fun _u#4294 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#67 =
  fun _u#4296 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#68 = fun _u#4298 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#69 = fun _u#4300 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#70 =
  fun _u#4302 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#71 =
  fun _u#4304 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#72 =
  fun _u#4306 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#73 =
  fun kh#4308 -> (({ VOTING_POWER })@(kh#4308))[@inline] in
let implicit_account#75 =
  fun kh#4312 -> (IMPLICIT_ACCOUNT(kh#4312))[@inline] in
let pairing_check#79 =
  fun l#4320 -> (({ PAIRING_CHECK })@(l#4320))[@inline] in
let set_delegate#81 = fun o#4324 -> (SET_DELEGATE(o#4324))[@inline] in
let open_chest#87 =
  fun ck#4340 ->
  (fun c#4341 -> (fun n#4342 -> (OPEN_CHEST(ck#4340 , c#4341 , n#4342))))[@inline] in
let xor#90 = fun l#4351 -> (fun r#4352 -> (XOR(l#4351 , r#4352)))[@inline] in
let shift_left#91 =
  fun l#4354 -> (fun r#4355 -> (LSL(l#4354 , r#4355)))[@inline] in
let shift_right#92 =
  fun l#4357 -> (fun r#4358 -> (LSR(l#4357 , r#4358)))[@inline] in
let length#133 = fun b#4490 -> (({ SIZE })@(b#4490))[@inline] in
let concat#134 =
  fun b1#4492 ->
  (fun b2#4493 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4492 , b2#4493))))[@inline] in
let sub#135 =
  fun s#4495 ->
  (fun l#4496 ->
   (fun b#4497 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4495 ,
                                                                   l#4496) ,
                                                              b#4497)))))[@inline] in
let length#140 = fun b#4508 -> (({ SIZE })@(b#4508))[@inline] in
let concat#141 =
  fun b1#4510 ->
  (fun b2#4511 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4510 , b2#4511))))[@inline] in
let sub#142 =
  fun s#4513 ->
  (fun l#4514 ->
   (fun b#4515 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4513 ,
                                                                   l#4514) ,
                                                              b#4515)))))[@inline] in
let blake2b#143 = fun b#4517 -> (({ BLAKE2B })@(b#4517))[@inline] in
let sha256#144 = fun b#4519 -> (({ SHA256 })@(b#4519))[@inline] in
let sha512#145 = fun b#4521 -> (({ SHA512 })@(b#4521))[@inline] in
let sha3#146 = fun b#4523 -> (({ SHA3 })@(b#4523))[@inline] in
let keccak#147 = fun b#4525 -> (({ KECCAK })@(b#4525))[@inline] in
let hash_key#148 = fun k#4527 -> (({ HASH_KEY })@(k#4527))[@inline] in
let check#149 =
  fun k#4529 ->
  (fun s#4530 ->
   (fun b#4531 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4529 , s#4530) ,
                                                   b#4531)))))[@inline] in
let assert#150 =
  fun b#4533 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4533))[@inline] in
let abs#153 = fun i#4539 -> (({ ABS })@(i#4539))[@inline] in
let is_nat#154 = fun i#4541 -> (({ ISNAT })@(i#4541))[@inline] in
let true#155 = TRUE()[@inline] in
let false#156 = FALSE()[@inline] in
let unit#157 = UNIT()[@inline] in
let assert_with_error#160 =
  fun b#4549 ->
  (fun s#4550 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4549 , s#4550))))[@inline] in
let poly_stub_273 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_272 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_271 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_270 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_269 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_268 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_267 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_266 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_265 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_264 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_263 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_262 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_261 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_260 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_259 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_258 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_257 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_256 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_255 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_254 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_253 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_252 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_251 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_250 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_249 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_248 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_247 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_246 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_245 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_244 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_243 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_242 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_241 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_240 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_239 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_238 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_237 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_236 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let poly_stub_235 = fun x#4561 -> (({ FAILWITH })@(x#4561))[@inline] in
let get_total_voting_power#168 =
  fun _u#4570 -> ((poly_stub_273)@(L(unit)))[@inline] in
let set_source#171 = fun _a#4576 -> ((poly_stub_272)@(L(unit)))[@inline] in
let get_storage_of_address#172 =
  fun _a#4578 -> ((poly_stub_271)@(L(unit)))[@inline] in
let get_balance#173 = fun _a#4580 -> ((poly_stub_270)@(L(unit)))[@inline] in
let print#174 = fun _v#4582 -> ((poly_stub_269)@(L(unit)))[@inline] in
let eprint#175 = fun _v#4584 -> ((poly_stub_268)@(L(unit)))[@inline] in
let get_voting_power#176 =
  fun _kh#4586 -> ((poly_stub_267)@(L(unit)))[@inline] in
let nth_bootstrap_contract#177 =
  fun _i#4588 -> ((poly_stub_266)@(L(unit)))[@inline] in
let nth_bootstrap_account#178 =
  fun _i#4590 -> ((poly_stub_265)@(L(unit)))[@inline] in
let get_bootstrap_account#179 =
  fun _n#4592 -> ((poly_stub_264)@(L(unit)))[@inline] in
let last_originations#181 =
  fun _u#4596 -> ((poly_stub_263)@(L(unit)))[@inline] in
let new_account#183 = fun _u#4600 -> ((poly_stub_262)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#185 =
  fun _n#4604 -> ((poly_stub_261)@(L(unit)))[@inline] in
let register_delegate#187 =
  fun _kh#4608 -> ((poly_stub_260)@(L(unit)))[@inline] in
let register_constant#188 =
  fun _m#4610 -> ((poly_stub_259)@(L(unit)))[@inline] in
let constant_to_michelson_program#190 =
  fun _s#4614 -> ((poly_stub_258)@(L(unit)))[@inline] in
let restore_context#191 =
  fun _u#4616 -> ((poly_stub_257)@(L(unit)))[@inline] in
let save_context#192 = fun _u#4618 -> ((poly_stub_256)@(L(unit)))[@inline] in
let drop_context#193 = fun _u#4620 -> ((poly_stub_255)@(L(unit)))[@inline] in
let set_baker_policy#196 =
  fun _bp#4626 -> ((poly_stub_254)@(L(unit)))[@inline] in
let set_baker#197 = fun _a#4628 -> ((poly_stub_253)@(L(unit)))[@inline] in
let size#198 = fun _c#4630 -> ((poly_stub_252)@(L(unit)))[@inline] in
let read_contract_from_file#200 =
  fun _fn#4634 -> ((poly_stub_251)@(L(unit)))[@inline] in
let chr#201 = fun _n#4636 -> ((poly_stub_250)@(L(unit)))[@inline] in
let nl#202 = L("NEWLINE")[@inline] in
let println#203 = fun _v#4639 -> ((poly_stub_249)@(L(unit)))[@inline] in
let transfer#204 =
  fun _a#4641 ->
  (fun _s#4642 -> (fun _t#4643 -> ((poly_stub_248)@(L(unit)))))[@inline] in
let transfer_exn#205 =
  fun _a#4645 ->
  (fun _s#4646 -> (fun _t#4647 -> ((poly_stub_247)@(L(unit)))))[@inline] in
let reset_state#207 =
  fun _n#4651 -> (fun _l#4652 -> ((poly_stub_246)@(L(unit))))[@inline] in
let reset_state_at#208 =
  fun _t#4654 ->
  (fun _n#4655 -> (fun _l#4656 -> ((poly_stub_245)@(L(unit)))))[@inline] in
let save_mutation#211 =
  fun _s#4665 -> (fun _m#4666 -> ((poly_stub_244)@(L(unit))))[@inline] in
let sign#214 =
  fun _sk#4674 -> (fun _d#4675 -> ((poly_stub_243)@(L(unit))))[@inline] in
let add_account#215 =
  fun _s#4677 -> (fun _k#4678 -> ((poly_stub_242)@(L(unit))))[@inline] in
let baker_account#216 =
  fun _p#4680 -> (fun _o#4681 -> ((poly_stub_241)@(L(unit))))[@inline] in
let create_chest#218 =
  fun _b#4686 -> (fun _n#4687 -> ((poly_stub_240)@(L(unit))))[@inline] in
let create_chest_key#219 =
  fun _c#4689 -> (fun _n#4690 -> ((poly_stub_239)@(L(unit))))[@inline] in
let michelson_equal#222 =
  fun _m1#4700 -> (fun _m2#4701 -> ((poly_stub_238)@(L(unit))))[@inline] in
let originate_contract#224 =
  fun _c#4706 ->
  (fun _s#4707 -> (fun _t#4708 -> ((poly_stub_237)@(L(unit)))))[@inline] in
let compile_contract_from_file#226 =
  fun _fn#4714 ->
  (fun _e#4715 -> (fun _v#4716 -> ((poly_stub_236)@(L(unit)))))[@inline] in
let originate_from_file#227 =
  fun _fn#4718 ->
  (fun _e#4719 ->
   (fun _v#4720 ->
    (fun _s#4721 -> (fun _t#4722 -> ((poly_stub_235)@(L(unit)))))))[@inline] in
let toto#228 = L(1) in
let get_balance#229 =
  fun _u#4725 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#230 =
  fun _u#4727 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#231 = fun _u#4729 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#232 =
  fun _u#4731 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#233 =
  fun _u#4733 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#234 = fun _u#4735 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#235 = fun _u#4737 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#236 =
  fun _u#4739 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#237 =
  fun _u#4741 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#238 =
  fun _u#4743 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#239 =
  fun kh#4745 -> (({ VOTING_POWER })@(kh#4745))[@inline] in
let implicit_account#241 =
  fun kh#4749 -> (IMPLICIT_ACCOUNT(kh#4749))[@inline] in
let pairing_check#245 =
  fun l#4757 -> (({ PAIRING_CHECK })@(l#4757))[@inline] in
let set_delegate#247 = fun o#4761 -> (SET_DELEGATE(o#4761))[@inline] in
let open_chest#253 =
  fun ck#4777 ->
  (fun c#4778 -> (fun n#4779 -> (OPEN_CHEST(ck#4777 , c#4778 , n#4779))))[@inline] in
let xor#256 =
  fun l#4788 -> (fun r#4789 -> (XOR(l#4788 , r#4789)))[@inline] in
let shift_left#257 =
  fun l#4791 -> (fun r#4792 -> (LSL(l#4791 , r#4792)))[@inline] in
let shift_right#258 =
  fun l#4794 -> (fun r#4795 -> (LSR(l#4794 , r#4795)))[@inline] in
let length#299 = fun b#4927 -> (({ SIZE })@(b#4927))[@inline] in
let concat#300 =
  fun b1#4929 ->
  (fun b2#4930 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4929 , b2#4930))))[@inline] in
let sub#301 =
  fun s#4932 ->
  (fun l#4933 ->
   (fun b#4934 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4932 ,
                                                                   l#4933) ,
                                                              b#4934)))))[@inline] in
let length#306 = fun b#4945 -> (({ SIZE })@(b#4945))[@inline] in
let concat#307 =
  fun b1#4947 ->
  (fun b2#4948 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4947 , b2#4948))))[@inline] in
let sub#308 =
  fun s#4950 ->
  (fun l#4951 ->
   (fun b#4952 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4950 ,
                                                                   l#4951) ,
                                                              b#4952)))))[@inline] in
let blake2b#309 = fun b#4954 -> (({ BLAKE2B })@(b#4954))[@inline] in
let sha256#310 = fun b#4956 -> (({ SHA256 })@(b#4956))[@inline] in
let sha512#311 = fun b#4958 -> (({ SHA512 })@(b#4958))[@inline] in
let sha3#312 = fun b#4960 -> (({ SHA3 })@(b#4960))[@inline] in
let keccak#313 = fun b#4962 -> (({ KECCAK })@(b#4962))[@inline] in
let hash_key#314 = fun k#4964 -> (({ HASH_KEY })@(k#4964))[@inline] in
let check#315 =
  fun k#4966 ->
  (fun s#4967 ->
   (fun b#4968 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4966 , s#4967) ,
                                                   b#4968)))))[@inline] in
let assert#316 =
  fun b#4970 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4970))[@inline] in
let abs#319 = fun i#4976 -> (({ ABS })@(i#4976))[@inline] in
let is_nat#320 = fun i#4978 -> (({ ISNAT })@(i#4978))[@inline] in
let true#321 = TRUE()[@inline] in
let false#322 = FALSE()[@inline] in
let unit#323 = UNIT()[@inline] in
let assert_with_error#326 =
  fun b#4986 ->
  (fun s#4987 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4986 , s#4987))))[@inline] in
let poly_stub_234 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_233 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_232 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_231 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_230 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_229 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_228 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_227 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_226 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_225 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_224 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_223 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_222 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_221 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_220 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_219 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_218 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_217 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_216 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_215 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_214 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_213 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_212 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_211 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_210 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_209 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_208 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_207 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_206 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_205 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_204 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_203 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_202 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_201 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_200 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_199 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_198 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_197 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let poly_stub_196 = fun x#4998 -> (({ FAILWITH })@(x#4998))[@inline] in
let get_total_voting_power#334 =
  fun _u#5007 -> ((poly_stub_234)@(L(unit)))[@inline] in
let set_source#337 = fun _a#5013 -> ((poly_stub_233)@(L(unit)))[@inline] in
let get_storage_of_address#338 =
  fun _a#5015 -> ((poly_stub_232)@(L(unit)))[@inline] in
let get_balance#339 = fun _a#5017 -> ((poly_stub_231)@(L(unit)))[@inline] in
let print#340 = fun _v#5019 -> ((poly_stub_230)@(L(unit)))[@inline] in
let eprint#341 = fun _v#5021 -> ((poly_stub_229)@(L(unit)))[@inline] in
let get_voting_power#342 =
  fun _kh#5023 -> ((poly_stub_228)@(L(unit)))[@inline] in
let nth_bootstrap_contract#343 =
  fun _i#5025 -> ((poly_stub_227)@(L(unit)))[@inline] in
let nth_bootstrap_account#344 =
  fun _i#5027 -> ((poly_stub_226)@(L(unit)))[@inline] in
let get_bootstrap_account#345 =
  fun _n#5029 -> ((poly_stub_225)@(L(unit)))[@inline] in
let last_originations#347 =
  fun _u#5033 -> ((poly_stub_224)@(L(unit)))[@inline] in
let new_account#349 = fun _u#5037 -> ((poly_stub_223)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#351 =
  fun _n#5041 -> ((poly_stub_222)@(L(unit)))[@inline] in
let register_delegate#353 =
  fun _kh#5045 -> ((poly_stub_221)@(L(unit)))[@inline] in
let register_constant#354 =
  fun _m#5047 -> ((poly_stub_220)@(L(unit)))[@inline] in
let constant_to_michelson_program#356 =
  fun _s#5051 -> ((poly_stub_219)@(L(unit)))[@inline] in
let restore_context#357 =
  fun _u#5053 -> ((poly_stub_218)@(L(unit)))[@inline] in
let save_context#358 = fun _u#5055 -> ((poly_stub_217)@(L(unit)))[@inline] in
let drop_context#359 = fun _u#5057 -> ((poly_stub_216)@(L(unit)))[@inline] in
let set_baker_policy#362 =
  fun _bp#5063 -> ((poly_stub_215)@(L(unit)))[@inline] in
let set_baker#363 = fun _a#5065 -> ((poly_stub_214)@(L(unit)))[@inline] in
let size#364 = fun _c#5067 -> ((poly_stub_213)@(L(unit)))[@inline] in
let read_contract_from_file#366 =
  fun _fn#5071 -> ((poly_stub_212)@(L(unit)))[@inline] in
let chr#367 = fun _n#5073 -> ((poly_stub_211)@(L(unit)))[@inline] in
let nl#368 = L("NEWLINE")[@inline] in
let println#369 = fun _v#5076 -> ((poly_stub_210)@(L(unit)))[@inline] in
let transfer#370 =
  fun _a#5078 ->
  (fun _s#5079 -> (fun _t#5080 -> ((poly_stub_209)@(L(unit)))))[@inline] in
let transfer_exn#371 =
  fun _a#5082 ->
  (fun _s#5083 -> (fun _t#5084 -> ((poly_stub_208)@(L(unit)))))[@inline] in
let reset_state#373 =
  fun _n#5088 -> (fun _l#5089 -> ((poly_stub_207)@(L(unit))))[@inline] in
let reset_state_at#374 =
  fun _t#5091 ->
  (fun _n#5092 -> (fun _l#5093 -> ((poly_stub_206)@(L(unit)))))[@inline] in
let save_mutation#377 =
  fun _s#5102 -> (fun _m#5103 -> ((poly_stub_205)@(L(unit))))[@inline] in
let sign#380 =
  fun _sk#5111 -> (fun _d#5112 -> ((poly_stub_204)@(L(unit))))[@inline] in
let add_account#381 =
  fun _s#5114 -> (fun _k#5115 -> ((poly_stub_203)@(L(unit))))[@inline] in
let baker_account#382 =
  fun _p#5117 -> (fun _o#5118 -> ((poly_stub_202)@(L(unit))))[@inline] in
let create_chest#384 =
  fun _b#5123 -> (fun _n#5124 -> ((poly_stub_201)@(L(unit))))[@inline] in
let create_chest_key#385 =
  fun _c#5126 -> (fun _n#5127 -> ((poly_stub_200)@(L(unit))))[@inline] in
let michelson_equal#388 =
  fun _m1#5137 -> (fun _m2#5138 -> ((poly_stub_199)@(L(unit))))[@inline] in
let originate_contract#390 =
  fun _c#5143 ->
  (fun _s#5144 -> (fun _t#5145 -> ((poly_stub_198)@(L(unit)))))[@inline] in
let compile_contract_from_file#392 =
  fun _fn#5151 ->
  (fun _e#5152 -> (fun _v#5153 -> ((poly_stub_197)@(L(unit)))))[@inline] in
let originate_from_file#393 =
  fun _fn#5155 ->
  (fun _e#5156 ->
   (fun _v#5157 ->
    (fun _s#5158 -> (fun _t#5159 -> ((poly_stub_196)@(L(unit)))))))[@inline] in
let toto#394 = L(32) in
let titi#395 = ADD(toto#228 , L(42)) in
let f#396 =
  fun gen#5163 ->
  (let (gen#7360, gen#7361) = gen#5163 in
   let gen#5164 = gen#7360 in
   let x#5165 = gen#7361 in
   let x#5166 = ADD(ADD(x#5165 , toto#228) , titi#395) in
   PAIR(LIST_EMPTY() , x#5166)) in
let get_balance#397 =
  fun _u#5168 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#398 =
  fun _u#5170 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#399 = fun _u#5172 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#400 =
  fun _u#5174 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#401 =
  fun _u#5176 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#402 = fun _u#5178 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#403 = fun _u#5180 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#404 =
  fun _u#5182 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#405 =
  fun _u#5184 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#406 =
  fun _u#5186 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#407 =
  fun kh#5188 -> (({ VOTING_POWER })@(kh#5188))[@inline] in
let implicit_account#409 =
  fun kh#5192 -> (IMPLICIT_ACCOUNT(kh#5192))[@inline] in
let pairing_check#413 =
  fun l#5200 -> (({ PAIRING_CHECK })@(l#5200))[@inline] in
let set_delegate#415 = fun o#5204 -> (SET_DELEGATE(o#5204))[@inline] in
let open_chest#421 =
  fun ck#5220 ->
  (fun c#5221 -> (fun n#5222 -> (OPEN_CHEST(ck#5220 , c#5221 , n#5222))))[@inline] in
let xor#424 =
  fun l#5231 -> (fun r#5232 -> (XOR(l#5231 , r#5232)))[@inline] in
let shift_left#425 =
  fun l#5234 -> (fun r#5235 -> (LSL(l#5234 , r#5235)))[@inline] in
let shift_right#426 =
  fun l#5237 -> (fun r#5238 -> (LSR(l#5237 , r#5238)))[@inline] in
let length#467 = fun b#5370 -> (({ SIZE })@(b#5370))[@inline] in
let concat#468 =
  fun b1#5372 ->
  (fun b2#5373 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5372 , b2#5373))))[@inline] in
let sub#469 =
  fun s#5375 ->
  (fun l#5376 ->
   (fun b#5377 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5375 ,
                                                                   l#5376) ,
                                                              b#5377)))))[@inline] in
let length#474 = fun b#5388 -> (({ SIZE })@(b#5388))[@inline] in
let concat#475 =
  fun b1#5390 ->
  (fun b2#5391 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5390 , b2#5391))))[@inline] in
let sub#476 =
  fun s#5393 ->
  (fun l#5394 ->
   (fun b#5395 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5393 ,
                                                                   l#5394) ,
                                                              b#5395)))))[@inline] in
let blake2b#477 = fun b#5397 -> (({ BLAKE2B })@(b#5397))[@inline] in
let sha256#478 = fun b#5399 -> (({ SHA256 })@(b#5399))[@inline] in
let sha512#479 = fun b#5401 -> (({ SHA512 })@(b#5401))[@inline] in
let sha3#480 = fun b#5403 -> (({ SHA3 })@(b#5403))[@inline] in
let keccak#481 = fun b#5405 -> (({ KECCAK })@(b#5405))[@inline] in
let hash_key#482 = fun k#5407 -> (({ HASH_KEY })@(k#5407))[@inline] in
let check#483 =
  fun k#5409 ->
  (fun s#5410 ->
   (fun b#5411 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5409 , s#5410) ,
                                                   b#5411)))))[@inline] in
let assert#484 =
  fun b#5413 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5413))[@inline] in
let abs#487 = fun i#5419 -> (({ ABS })@(i#5419))[@inline] in
let is_nat#488 = fun i#5421 -> (({ ISNAT })@(i#5421))[@inline] in
let true#489 = TRUE()[@inline] in
let false#490 = FALSE()[@inline] in
let unit#491 = UNIT()[@inline] in
let assert_with_error#494 =
  fun b#5429 ->
  (fun s#5430 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5429 , s#5430))))[@inline] in
let poly_stub_195 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_194 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_193 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_192 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_191 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_190 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_189 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_188 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_187 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_186 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_185 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_184 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_183 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_182 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_181 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_180 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_179 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_178 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_177 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_176 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_175 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_174 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_173 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_172 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_171 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_170 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_169 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_168 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_167 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_166 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_165 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_164 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_163 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_162 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_161 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_160 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_159 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_158 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let poly_stub_157 = fun x#5441 -> (({ FAILWITH })@(x#5441))[@inline] in
let get_total_voting_power#502 =
  fun _u#5450 -> ((poly_stub_195)@(L(unit)))[@inline] in
let set_source#505 = fun _a#5456 -> ((poly_stub_194)@(L(unit)))[@inline] in
let get_storage_of_address#506 =
  fun _a#5458 -> ((poly_stub_193)@(L(unit)))[@inline] in
let get_balance#507 = fun _a#5460 -> ((poly_stub_192)@(L(unit)))[@inline] in
let print#508 = fun _v#5462 -> ((poly_stub_191)@(L(unit)))[@inline] in
let eprint#509 = fun _v#5464 -> ((poly_stub_190)@(L(unit)))[@inline] in
let get_voting_power#510 =
  fun _kh#5466 -> ((poly_stub_189)@(L(unit)))[@inline] in
let nth_bootstrap_contract#511 =
  fun _i#5468 -> ((poly_stub_188)@(L(unit)))[@inline] in
let nth_bootstrap_account#512 =
  fun _i#5470 -> ((poly_stub_187)@(L(unit)))[@inline] in
let get_bootstrap_account#513 =
  fun _n#5472 -> ((poly_stub_186)@(L(unit)))[@inline] in
let last_originations#515 =
  fun _u#5476 -> ((poly_stub_185)@(L(unit)))[@inline] in
let new_account#517 = fun _u#5480 -> ((poly_stub_184)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#519 =
  fun _n#5484 -> ((poly_stub_183)@(L(unit)))[@inline] in
let register_delegate#521 =
  fun _kh#5488 -> ((poly_stub_182)@(L(unit)))[@inline] in
let register_constant#522 =
  fun _m#5490 -> ((poly_stub_181)@(L(unit)))[@inline] in
let constant_to_michelson_program#524 =
  fun _s#5494 -> ((poly_stub_180)@(L(unit)))[@inline] in
let restore_context#525 =
  fun _u#5496 -> ((poly_stub_179)@(L(unit)))[@inline] in
let save_context#526 = fun _u#5498 -> ((poly_stub_178)@(L(unit)))[@inline] in
let drop_context#527 = fun _u#5500 -> ((poly_stub_177)@(L(unit)))[@inline] in
let set_baker_policy#530 =
  fun _bp#5506 -> ((poly_stub_176)@(L(unit)))[@inline] in
let set_baker#531 = fun _a#5508 -> ((poly_stub_175)@(L(unit)))[@inline] in
let size#532 = fun _c#5510 -> ((poly_stub_174)@(L(unit)))[@inline] in
let read_contract_from_file#534 =
  fun _fn#5514 -> ((poly_stub_173)@(L(unit)))[@inline] in
let chr#535 = fun _n#5516 -> ((poly_stub_172)@(L(unit)))[@inline] in
let nl#536 = L("NEWLINE")[@inline] in
let println#537 = fun _v#5519 -> ((poly_stub_171)@(L(unit)))[@inline] in
let transfer#538 =
  fun _a#5521 ->
  (fun _s#5522 -> (fun _t#5523 -> ((poly_stub_170)@(L(unit)))))[@inline] in
let transfer_exn#539 =
  fun _a#5525 ->
  (fun _s#5526 -> (fun _t#5527 -> ((poly_stub_169)@(L(unit)))))[@inline] in
let reset_state#541 =
  fun _n#5531 -> (fun _l#5532 -> ((poly_stub_168)@(L(unit))))[@inline] in
let reset_state_at#542 =
  fun _t#5534 ->
  (fun _n#5535 -> (fun _l#5536 -> ((poly_stub_167)@(L(unit)))))[@inline] in
let save_mutation#545 =
  fun _s#5545 -> (fun _m#5546 -> ((poly_stub_166)@(L(unit))))[@inline] in
let sign#548 =
  fun _sk#5554 -> (fun _d#5555 -> ((poly_stub_165)@(L(unit))))[@inline] in
let add_account#549 =
  fun _s#5557 -> (fun _k#5558 -> ((poly_stub_164)@(L(unit))))[@inline] in
let baker_account#550 =
  fun _p#5560 -> (fun _o#5561 -> ((poly_stub_163)@(L(unit))))[@inline] in
let create_chest#552 =
  fun _b#5566 -> (fun _n#5567 -> ((poly_stub_162)@(L(unit))))[@inline] in
let create_chest_key#553 =
  fun _c#5569 -> (fun _n#5570 -> ((poly_stub_161)@(L(unit))))[@inline] in
let michelson_equal#556 =
  fun _m1#5580 -> (fun _m2#5581 -> ((poly_stub_160)@(L(unit))))[@inline] in
let originate_contract#558 =
  fun _c#5586 ->
  (fun _s#5587 -> (fun _t#5588 -> ((poly_stub_159)@(L(unit)))))[@inline] in
let compile_contract_from_file#560 =
  fun _fn#5594 ->
  (fun _e#5595 -> (fun _v#5596 -> ((poly_stub_158)@(L(unit)))))[@inline] in
let originate_from_file#561 =
  fun _fn#5598 ->
  (fun _e#5599 ->
   (fun _v#5600 ->
    (fun _s#5601 -> (fun _t#5602 -> ((poly_stub_157)@(L(unit)))))))[@inline] in
let toto#562 = L(44) in
let get_balance#563 =
  fun _u#5605 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#564 =
  fun _u#5607 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#565 = fun _u#5609 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#566 =
  fun _u#5611 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#567 =
  fun _u#5613 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#568 = fun _u#5615 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#569 = fun _u#5617 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#570 =
  fun _u#5619 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#571 =
  fun _u#5621 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#572 =
  fun _u#5623 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#573 =
  fun kh#5625 -> (({ VOTING_POWER })@(kh#5625))[@inline] in
let implicit_account#575 =
  fun kh#5629 -> (IMPLICIT_ACCOUNT(kh#5629))[@inline] in
let pairing_check#579 =
  fun l#5637 -> (({ PAIRING_CHECK })@(l#5637))[@inline] in
let set_delegate#581 = fun o#5641 -> (SET_DELEGATE(o#5641))[@inline] in
let open_chest#587 =
  fun ck#5657 ->
  (fun c#5658 -> (fun n#5659 -> (OPEN_CHEST(ck#5657 , c#5658 , n#5659))))[@inline] in
let xor#590 =
  fun l#5668 -> (fun r#5669 -> (XOR(l#5668 , r#5669)))[@inline] in
let shift_left#591 =
  fun l#5671 -> (fun r#5672 -> (LSL(l#5671 , r#5672)))[@inline] in
let shift_right#592 =
  fun l#5674 -> (fun r#5675 -> (LSR(l#5674 , r#5675)))[@inline] in
let length#633 = fun b#5807 -> (({ SIZE })@(b#5807))[@inline] in
let concat#634 =
  fun b1#5809 ->
  (fun b2#5810 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5809 , b2#5810))))[@inline] in
let sub#635 =
  fun s#5812 ->
  (fun l#5813 ->
   (fun b#5814 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5812 ,
                                                                   l#5813) ,
                                                              b#5814)))))[@inline] in
let length#640 = fun b#5825 -> (({ SIZE })@(b#5825))[@inline] in
let concat#641 =
  fun b1#5827 ->
  (fun b2#5828 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5827 , b2#5828))))[@inline] in
let sub#642 =
  fun s#5830 ->
  (fun l#5831 ->
   (fun b#5832 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5830 ,
                                                                   l#5831) ,
                                                              b#5832)))))[@inline] in
let blake2b#643 = fun b#5834 -> (({ BLAKE2B })@(b#5834))[@inline] in
let sha256#644 = fun b#5836 -> (({ SHA256 })@(b#5836))[@inline] in
let sha512#645 = fun b#5838 -> (({ SHA512 })@(b#5838))[@inline] in
let sha3#646 = fun b#5840 -> (({ SHA3 })@(b#5840))[@inline] in
let keccak#647 = fun b#5842 -> (({ KECCAK })@(b#5842))[@inline] in
let hash_key#648 = fun k#5844 -> (({ HASH_KEY })@(k#5844))[@inline] in
let check#649 =
  fun k#5846 ->
  (fun s#5847 ->
   (fun b#5848 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5846 , s#5847) ,
                                                   b#5848)))))[@inline] in
let assert#650 =
  fun b#5850 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5850))[@inline] in
let abs#653 = fun i#5856 -> (({ ABS })@(i#5856))[@inline] in
let is_nat#654 = fun i#5858 -> (({ ISNAT })@(i#5858))[@inline] in
let true#655 = TRUE()[@inline] in
let false#656 = FALSE()[@inline] in
let unit#657 = UNIT()[@inline] in
let assert_with_error#660 =
  fun b#5866 ->
  (fun s#5867 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5866 , s#5867))))[@inline] in
let poly_stub_156 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_155 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_154 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_153 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_152 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_151 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_150 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_149 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_148 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_147 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_146 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_145 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_144 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_143 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_142 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_141 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_140 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_139 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_138 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_137 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_136 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_135 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_134 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_133 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_132 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_131 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_130 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_129 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_128 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_127 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_126 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_125 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_124 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_123 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_122 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_121 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_120 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_119 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let poly_stub_118 = fun x#5878 -> (({ FAILWITH })@(x#5878))[@inline] in
let get_total_voting_power#668 =
  fun _u#5887 -> ((poly_stub_156)@(L(unit)))[@inline] in
let set_source#671 = fun _a#5893 -> ((poly_stub_155)@(L(unit)))[@inline] in
let get_storage_of_address#672 =
  fun _a#5895 -> ((poly_stub_154)@(L(unit)))[@inline] in
let get_balance#673 = fun _a#5897 -> ((poly_stub_153)@(L(unit)))[@inline] in
let print#674 = fun _v#5899 -> ((poly_stub_152)@(L(unit)))[@inline] in
let eprint#675 = fun _v#5901 -> ((poly_stub_151)@(L(unit)))[@inline] in
let get_voting_power#676 =
  fun _kh#5903 -> ((poly_stub_150)@(L(unit)))[@inline] in
let nth_bootstrap_contract#677 =
  fun _i#5905 -> ((poly_stub_149)@(L(unit)))[@inline] in
let nth_bootstrap_account#678 =
  fun _i#5907 -> ((poly_stub_148)@(L(unit)))[@inline] in
let get_bootstrap_account#679 =
  fun _n#5909 -> ((poly_stub_147)@(L(unit)))[@inline] in
let last_originations#681 =
  fun _u#5913 -> ((poly_stub_146)@(L(unit)))[@inline] in
let new_account#683 = fun _u#5917 -> ((poly_stub_145)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#685 =
  fun _n#5921 -> ((poly_stub_144)@(L(unit)))[@inline] in
let register_delegate#687 =
  fun _kh#5925 -> ((poly_stub_143)@(L(unit)))[@inline] in
let register_constant#688 =
  fun _m#5927 -> ((poly_stub_142)@(L(unit)))[@inline] in
let constant_to_michelson_program#690 =
  fun _s#5931 -> ((poly_stub_141)@(L(unit)))[@inline] in
let restore_context#691 =
  fun _u#5933 -> ((poly_stub_140)@(L(unit)))[@inline] in
let save_context#692 = fun _u#5935 -> ((poly_stub_139)@(L(unit)))[@inline] in
let drop_context#693 = fun _u#5937 -> ((poly_stub_138)@(L(unit)))[@inline] in
let set_baker_policy#696 =
  fun _bp#5943 -> ((poly_stub_137)@(L(unit)))[@inline] in
let set_baker#697 = fun _a#5945 -> ((poly_stub_136)@(L(unit)))[@inline] in
let size#698 = fun _c#5947 -> ((poly_stub_135)@(L(unit)))[@inline] in
let read_contract_from_file#700 =
  fun _fn#5951 -> ((poly_stub_134)@(L(unit)))[@inline] in
let chr#701 = fun _n#5953 -> ((poly_stub_133)@(L(unit)))[@inline] in
let nl#702 = L("NEWLINE")[@inline] in
let println#703 = fun _v#5956 -> ((poly_stub_132)@(L(unit)))[@inline] in
let transfer#704 =
  fun _a#5958 ->
  (fun _s#5959 -> (fun _t#5960 -> ((poly_stub_131)@(L(unit)))))[@inline] in
let transfer_exn#705 =
  fun _a#5962 ->
  (fun _s#5963 -> (fun _t#5964 -> ((poly_stub_130)@(L(unit)))))[@inline] in
let reset_state#707 =
  fun _n#5968 -> (fun _l#5969 -> ((poly_stub_129)@(L(unit))))[@inline] in
let reset_state_at#708 =
  fun _t#5971 ->
  (fun _n#5972 -> (fun _l#5973 -> ((poly_stub_128)@(L(unit)))))[@inline] in
let save_mutation#711 =
  fun _s#5982 -> (fun _m#5983 -> ((poly_stub_127)@(L(unit))))[@inline] in
let sign#714 =
  fun _sk#5991 -> (fun _d#5992 -> ((poly_stub_126)@(L(unit))))[@inline] in
let add_account#715 =
  fun _s#5994 -> (fun _k#5995 -> ((poly_stub_125)@(L(unit))))[@inline] in
let baker_account#716 =
  fun _p#5997 -> (fun _o#5998 -> ((poly_stub_124)@(L(unit))))[@inline] in
let create_chest#718 =
  fun _b#6003 -> (fun _n#6004 -> ((poly_stub_123)@(L(unit))))[@inline] in
let create_chest_key#719 =
  fun _c#6006 -> (fun _n#6007 -> ((poly_stub_122)@(L(unit))))[@inline] in
let michelson_equal#722 =
  fun _m1#6017 -> (fun _m2#6018 -> ((poly_stub_121)@(L(unit))))[@inline] in
let originate_contract#724 =
  fun _c#6023 ->
  (fun _s#6024 -> (fun _t#6025 -> ((poly_stub_120)@(L(unit)))))[@inline] in
let compile_contract_from_file#726 =
  fun _fn#6031 ->
  (fun _e#6032 -> (fun _v#6033 -> ((poly_stub_119)@(L(unit)))))[@inline] in
let originate_from_file#727 =
  fun _fn#6035 ->
  (fun _e#6036 ->
   (fun _v#6037 ->
    (fun _s#6038 -> (fun _t#6039 -> ((poly_stub_118)@(L(unit)))))))[@inline] in
let toto#728 = L(43) in
let get_balance#729 =
  fun _u#6042 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#730 =
  fun _u#6044 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#731 = fun _u#6046 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#732 =
  fun _u#6048 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#733 =
  fun _u#6050 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#734 = fun _u#6052 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#735 = fun _u#6054 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#736 =
  fun _u#6056 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#737 =
  fun _u#6058 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#738 =
  fun _u#6060 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#739 =
  fun kh#6062 -> (({ VOTING_POWER })@(kh#6062))[@inline] in
let implicit_account#741 =
  fun kh#6066 -> (IMPLICIT_ACCOUNT(kh#6066))[@inline] in
let pairing_check#745 =
  fun l#6074 -> (({ PAIRING_CHECK })@(l#6074))[@inline] in
let set_delegate#747 = fun o#6078 -> (SET_DELEGATE(o#6078))[@inline] in
let open_chest#753 =
  fun ck#6094 ->
  (fun c#6095 -> (fun n#6096 -> (OPEN_CHEST(ck#6094 , c#6095 , n#6096))))[@inline] in
let xor#756 =
  fun l#6105 -> (fun r#6106 -> (XOR(l#6105 , r#6106)))[@inline] in
let shift_left#757 =
  fun l#6108 -> (fun r#6109 -> (LSL(l#6108 , r#6109)))[@inline] in
let shift_right#758 =
  fun l#6111 -> (fun r#6112 -> (LSR(l#6111 , r#6112)))[@inline] in
let length#799 = fun b#6244 -> (({ SIZE })@(b#6244))[@inline] in
let concat#800 =
  fun b1#6246 ->
  (fun b2#6247 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6246 , b2#6247))))[@inline] in
let sub#801 =
  fun s#6249 ->
  (fun l#6250 ->
   (fun b#6251 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6249 ,
                                                                   l#6250) ,
                                                              b#6251)))))[@inline] in
let length#806 = fun b#6262 -> (({ SIZE })@(b#6262))[@inline] in
let concat#807 =
  fun b1#6264 ->
  (fun b2#6265 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6264 , b2#6265))))[@inline] in
let sub#808 =
  fun s#6267 ->
  (fun l#6268 ->
   (fun b#6269 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6267 ,
                                                                   l#6268) ,
                                                              b#6269)))))[@inline] in
let blake2b#809 = fun b#6271 -> (({ BLAKE2B })@(b#6271))[@inline] in
let sha256#810 = fun b#6273 -> (({ SHA256 })@(b#6273))[@inline] in
let sha512#811 = fun b#6275 -> (({ SHA512 })@(b#6275))[@inline] in
let sha3#812 = fun b#6277 -> (({ SHA3 })@(b#6277))[@inline] in
let keccak#813 = fun b#6279 -> (({ KECCAK })@(b#6279))[@inline] in
let hash_key#814 = fun k#6281 -> (({ HASH_KEY })@(k#6281))[@inline] in
let check#815 =
  fun k#6283 ->
  (fun s#6284 ->
   (fun b#6285 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6283 , s#6284) ,
                                                   b#6285)))))[@inline] in
let assert#816 =
  fun b#6287 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6287))[@inline] in
let abs#819 = fun i#6293 -> (({ ABS })@(i#6293))[@inline] in
let is_nat#820 = fun i#6295 -> (({ ISNAT })@(i#6295))[@inline] in
let true#821 = TRUE()[@inline] in
let false#822 = FALSE()[@inline] in
let unit#823 = UNIT()[@inline] in
let assert_with_error#826 =
  fun b#6303 ->
  (fun s#6304 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6303 , s#6304))))[@inline] in
let poly_stub_117 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_116 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_115 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_114 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_113 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_112 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_111 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_110 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_109 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_108 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_107 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_106 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_105 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_104 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_103 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_102 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_101 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_100 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_99 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_98 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_97 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_96 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_95 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_94 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_93 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_92 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_91 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_90 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_89 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_88 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_87 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_86 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_85 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_84 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_83 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_82 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_81 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_80 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let poly_stub_79 = fun x#6315 -> (({ FAILWITH })@(x#6315))[@inline] in
let get_total_voting_power#834 =
  fun _u#6324 -> ((poly_stub_117)@(L(unit)))[@inline] in
let set_source#837 = fun _a#6330 -> ((poly_stub_116)@(L(unit)))[@inline] in
let get_storage_of_address#838 =
  fun _a#6332 -> ((poly_stub_115)@(L(unit)))[@inline] in
let get_balance#839 = fun _a#6334 -> ((poly_stub_114)@(L(unit)))[@inline] in
let print#840 = fun _v#6336 -> ((poly_stub_113)@(L(unit)))[@inline] in
let eprint#841 = fun _v#6338 -> ((poly_stub_112)@(L(unit)))[@inline] in
let get_voting_power#842 =
  fun _kh#6340 -> ((poly_stub_111)@(L(unit)))[@inline] in
let nth_bootstrap_contract#843 =
  fun _i#6342 -> ((poly_stub_110)@(L(unit)))[@inline] in
let nth_bootstrap_account#844 =
  fun _i#6344 -> ((poly_stub_109)@(L(unit)))[@inline] in
let get_bootstrap_account#845 =
  fun _n#6346 -> ((poly_stub_108)@(L(unit)))[@inline] in
let last_originations#847 =
  fun _u#6350 -> ((poly_stub_107)@(L(unit)))[@inline] in
let new_account#849 = fun _u#6354 -> ((poly_stub_106)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#851 =
  fun _n#6358 -> ((poly_stub_105)@(L(unit)))[@inline] in
let register_delegate#853 =
  fun _kh#6362 -> ((poly_stub_104)@(L(unit)))[@inline] in
let register_constant#854 =
  fun _m#6364 -> ((poly_stub_103)@(L(unit)))[@inline] in
let constant_to_michelson_program#856 =
  fun _s#6368 -> ((poly_stub_102)@(L(unit)))[@inline] in
let restore_context#857 =
  fun _u#6370 -> ((poly_stub_101)@(L(unit)))[@inline] in
let save_context#858 = fun _u#6372 -> ((poly_stub_100)@(L(unit)))[@inline] in
let drop_context#859 = fun _u#6374 -> ((poly_stub_99)@(L(unit)))[@inline] in
let set_baker_policy#862 =
  fun _bp#6380 -> ((poly_stub_98)@(L(unit)))[@inline] in
let set_baker#863 = fun _a#6382 -> ((poly_stub_97)@(L(unit)))[@inline] in
let size#864 = fun _c#6384 -> ((poly_stub_96)@(L(unit)))[@inline] in
let read_contract_from_file#866 =
  fun _fn#6388 -> ((poly_stub_95)@(L(unit)))[@inline] in
let chr#867 = fun _n#6390 -> ((poly_stub_94)@(L(unit)))[@inline] in
let nl#868 = L("NEWLINE")[@inline] in
let println#869 = fun _v#6393 -> ((poly_stub_93)@(L(unit)))[@inline] in
let transfer#870 =
  fun _a#6395 -> (fun _s#6396 -> (fun _t#6397 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let transfer_exn#871 =
  fun _a#6399 -> (fun _s#6400 -> (fun _t#6401 -> ((poly_stub_91)@(L(unit)))))[@inline] in
let reset_state#873 =
  fun _n#6405 -> (fun _l#6406 -> ((poly_stub_90)@(L(unit))))[@inline] in
let reset_state_at#874 =
  fun _t#6408 -> (fun _n#6409 -> (fun _l#6410 -> ((poly_stub_89)@(L(unit)))))[@inline] in
let save_mutation#877 =
  fun _s#6419 -> (fun _m#6420 -> ((poly_stub_88)@(L(unit))))[@inline] in
let sign#880 =
  fun _sk#6428 -> (fun _d#6429 -> ((poly_stub_87)@(L(unit))))[@inline] in
let add_account#881 =
  fun _s#6431 -> (fun _k#6432 -> ((poly_stub_86)@(L(unit))))[@inline] in
let baker_account#882 =
  fun _p#6434 -> (fun _o#6435 -> ((poly_stub_85)@(L(unit))))[@inline] in
let create_chest#884 =
  fun _b#6440 -> (fun _n#6441 -> ((poly_stub_84)@(L(unit))))[@inline] in
let create_chest_key#885 =
  fun _c#6443 -> (fun _n#6444 -> ((poly_stub_83)@(L(unit))))[@inline] in
let michelson_equal#888 =
  fun _m1#6454 -> (fun _m2#6455 -> ((poly_stub_82)@(L(unit))))[@inline] in
let originate_contract#890 =
  fun _c#6460 -> (fun _s#6461 -> (fun _t#6462 -> ((poly_stub_81)@(L(unit)))))[@inline] in
let compile_contract_from_file#892 =
  fun _fn#6468 ->
  (fun _e#6469 -> (fun _v#6470 -> ((poly_stub_80)@(L(unit)))))[@inline] in
let originate_from_file#893 =
  fun _fn#6472 ->
  (fun _e#6473 ->
   (fun _v#6474 ->
    (fun _s#6475 -> (fun _t#6476 -> ((poly_stub_79)@(L(unit)))))))[@inline] in
let tata#894 = ADD(toto#228 , titi#395) in
let foo#895 = (f#396)@(PAIR(L(unit) , L(3))) in
let get_balance#896 =
  fun _u#6480 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#897 =
  fun _u#6482 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#898 = fun _u#6484 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#899 =
  fun _u#6486 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#900 =
  fun _u#6488 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#901 = fun _u#6490 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#902 = fun _u#6492 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#903 =
  fun _u#6494 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#904 =
  fun _u#6496 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#905 =
  fun _u#6498 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#906 =
  fun kh#6500 -> (({ VOTING_POWER })@(kh#6500))[@inline] in
let implicit_account#908 =
  fun kh#6504 -> (IMPLICIT_ACCOUNT(kh#6504))[@inline] in
let pairing_check#912 =
  fun l#6512 -> (({ PAIRING_CHECK })@(l#6512))[@inline] in
let set_delegate#914 = fun o#6516 -> (SET_DELEGATE(o#6516))[@inline] in
let open_chest#920 =
  fun ck#6532 ->
  (fun c#6533 -> (fun n#6534 -> (OPEN_CHEST(ck#6532 , c#6533 , n#6534))))[@inline] in
let xor#923 =
  fun l#6543 -> (fun r#6544 -> (XOR(l#6543 , r#6544)))[@inline] in
let shift_left#924 =
  fun l#6546 -> (fun r#6547 -> (LSL(l#6546 , r#6547)))[@inline] in
let shift_right#925 =
  fun l#6549 -> (fun r#6550 -> (LSR(l#6549 , r#6550)))[@inline] in
let length#966 = fun b#6682 -> (({ SIZE })@(b#6682))[@inline] in
let concat#967 =
  fun b1#6684 ->
  (fun b2#6685 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6684 , b2#6685))))[@inline] in
let sub#968 =
  fun s#6687 ->
  (fun l#6688 ->
   (fun b#6689 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6687 ,
                                                                   l#6688) ,
                                                              b#6689)))))[@inline] in
let length#973 = fun b#6700 -> (({ SIZE })@(b#6700))[@inline] in
let concat#974 =
  fun b1#6702 ->
  (fun b2#6703 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6702 , b2#6703))))[@inline] in
let sub#975 =
  fun s#6705 ->
  (fun l#6706 ->
   (fun b#6707 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6705 ,
                                                                   l#6706) ,
                                                              b#6707)))))[@inline] in
let blake2b#976 = fun b#6709 -> (({ BLAKE2B })@(b#6709))[@inline] in
let sha256#977 = fun b#6711 -> (({ SHA256 })@(b#6711))[@inline] in
let sha512#978 = fun b#6713 -> (({ SHA512 })@(b#6713))[@inline] in
let sha3#979 = fun b#6715 -> (({ SHA3 })@(b#6715))[@inline] in
let keccak#980 = fun b#6717 -> (({ KECCAK })@(b#6717))[@inline] in
let hash_key#981 = fun k#6719 -> (({ HASH_KEY })@(k#6719))[@inline] in
let check#982 =
  fun k#6721 ->
  (fun s#6722 ->
   (fun b#6723 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6721 , s#6722) ,
                                                   b#6723)))))[@inline] in
let assert#983 =
  fun b#6725 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6725))[@inline] in
let abs#986 = fun i#6731 -> (({ ABS })@(i#6731))[@inline] in
let is_nat#987 = fun i#6733 -> (({ ISNAT })@(i#6733))[@inline] in
let true#988 = TRUE()[@inline] in
let false#989 = FALSE()[@inline] in
let unit#990 = UNIT()[@inline] in
let assert_with_error#993 =
  fun b#6741 ->
  (fun s#6742 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6741 , s#6742))))[@inline] in
let poly_stub_78 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_77 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_76 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_75 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_74 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_73 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_72 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_71 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_70 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_69 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_68 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_67 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_66 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_65 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_64 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_63 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_62 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_61 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_60 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_59 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_58 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_57 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_56 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_55 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_54 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_53 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_52 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_51 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_50 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_49 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_48 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_47 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_46 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_45 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_44 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_43 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_42 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_41 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let poly_stub_40 = fun x#6753 -> (({ FAILWITH })@(x#6753))[@inline] in
let get_total_voting_power#1001 =
  fun _u#6762 -> ((poly_stub_78)@(L(unit)))[@inline] in
let set_source#1004 = fun _a#6768 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#1005 =
  fun _a#6770 -> ((poly_stub_76)@(L(unit)))[@inline] in
let get_balance#1006 = fun _a#6772 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#1007 = fun _v#6774 -> ((poly_stub_74)@(L(unit)))[@inline] in
let eprint#1008 = fun _v#6776 -> ((poly_stub_73)@(L(unit)))[@inline] in
let get_voting_power#1009 =
  fun _kh#6778 -> ((poly_stub_72)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1010 =
  fun _i#6780 -> ((poly_stub_71)@(L(unit)))[@inline] in
let nth_bootstrap_account#1011 =
  fun _i#6782 -> ((poly_stub_70)@(L(unit)))[@inline] in
let get_bootstrap_account#1012 =
  fun _n#6784 -> ((poly_stub_69)@(L(unit)))[@inline] in
let last_originations#1014 =
  fun _u#6788 -> ((poly_stub_68)@(L(unit)))[@inline] in
let new_account#1016 = fun _u#6792 -> ((poly_stub_67)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1018 =
  fun _n#6796 -> ((poly_stub_66)@(L(unit)))[@inline] in
let register_delegate#1020 =
  fun _kh#6800 -> ((poly_stub_65)@(L(unit)))[@inline] in
let register_constant#1021 =
  fun _m#6802 -> ((poly_stub_64)@(L(unit)))[@inline] in
let constant_to_michelson_program#1023 =
  fun _s#6806 -> ((poly_stub_63)@(L(unit)))[@inline] in
let restore_context#1024 =
  fun _u#6808 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#1025 = fun _u#6810 -> ((poly_stub_61)@(L(unit)))[@inline] in
let drop_context#1026 = fun _u#6812 -> ((poly_stub_60)@(L(unit)))[@inline] in
let set_baker_policy#1029 =
  fun _bp#6818 -> ((poly_stub_59)@(L(unit)))[@inline] in
let set_baker#1030 = fun _a#6820 -> ((poly_stub_58)@(L(unit)))[@inline] in
let size#1031 = fun _c#6822 -> ((poly_stub_57)@(L(unit)))[@inline] in
let read_contract_from_file#1033 =
  fun _fn#6826 -> ((poly_stub_56)@(L(unit)))[@inline] in
let chr#1034 = fun _n#6828 -> ((poly_stub_55)@(L(unit)))[@inline] in
let nl#1035 = L("NEWLINE")[@inline] in
let println#1036 = fun _v#6831 -> ((poly_stub_54)@(L(unit)))[@inline] in
let transfer#1037 =
  fun _a#6833 -> (fun _s#6834 -> (fun _t#6835 -> ((poly_stub_53)@(L(unit)))))[@inline] in
let transfer_exn#1038 =
  fun _a#6837 -> (fun _s#6838 -> (fun _t#6839 -> ((poly_stub_52)@(L(unit)))))[@inline] in
let reset_state#1040 =
  fun _n#6843 -> (fun _l#6844 -> ((poly_stub_51)@(L(unit))))[@inline] in
let reset_state_at#1041 =
  fun _t#6846 -> (fun _n#6847 -> (fun _l#6848 -> ((poly_stub_50)@(L(unit)))))[@inline] in
let save_mutation#1044 =
  fun _s#6857 -> (fun _m#6858 -> ((poly_stub_49)@(L(unit))))[@inline] in
let sign#1047 =
  fun _sk#6866 -> (fun _d#6867 -> ((poly_stub_48)@(L(unit))))[@inline] in
let add_account#1048 =
  fun _s#6869 -> (fun _k#6870 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#1049 =
  fun _p#6872 -> (fun _o#6873 -> ((poly_stub_46)@(L(unit))))[@inline] in
let create_chest#1051 =
  fun _b#6878 -> (fun _n#6879 -> ((poly_stub_45)@(L(unit))))[@inline] in
let create_chest_key#1052 =
  fun _c#6881 -> (fun _n#6882 -> ((poly_stub_44)@(L(unit))))[@inline] in
let michelson_equal#1055 =
  fun _m1#6892 -> (fun _m2#6893 -> ((poly_stub_43)@(L(unit))))[@inline] in
let originate_contract#1057 =
  fun _c#6898 -> (fun _s#6899 -> (fun _t#6900 -> ((poly_stub_42)@(L(unit)))))[@inline] in
let compile_contract_from_file#1059 =
  fun _fn#6906 ->
  (fun _e#6907 -> (fun _v#6908 -> ((poly_stub_41)@(L(unit)))))[@inline] in
let originate_from_file#1060 =
  fun _fn#6910 ->
  (fun _e#6911 ->
   (fun _v#6912 ->
    (fun _s#6913 -> (fun _t#6914 -> ((poly_stub_40)@(L(unit)))))))[@inline] in
let toto#1061 = L(10) in
let foo#1062 = L("bar") in
let get_balance#1063 =
  fun _u#6918 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1064 =
  fun _u#6920 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1065 = fun _u#6922 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1066 =
  fun _u#6924 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#1067 =
  fun _u#6926 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1068 =
  fun _u#6928 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1069 = fun _u#6930 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1070 =
  fun _u#6932 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1071 =
  fun _u#6934 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#1072 =
  fun _u#6936 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#1073 =
  fun kh#6938 -> (({ VOTING_POWER })@(kh#6938))[@inline] in
let implicit_account#1075 =
  fun kh#6942 -> (IMPLICIT_ACCOUNT(kh#6942))[@inline] in
let pairing_check#1079 =
  fun l#6950 -> (({ PAIRING_CHECK })@(l#6950))[@inline] in
let set_delegate#1081 = fun o#6954 -> (SET_DELEGATE(o#6954))[@inline] in
let open_chest#1087 =
  fun ck#6970 ->
  (fun c#6971 -> (fun n#6972 -> (OPEN_CHEST(ck#6970 , c#6971 , n#6972))))[@inline] in
let xor#1090 =
  fun l#6981 -> (fun r#6982 -> (XOR(l#6981 , r#6982)))[@inline] in
let shift_left#1091 =
  fun l#6984 -> (fun r#6985 -> (LSL(l#6984 , r#6985)))[@inline] in
let shift_right#1092 =
  fun l#6987 -> (fun r#6988 -> (LSR(l#6987 , r#6988)))[@inline] in
let length#1133 = fun b#7120 -> (({ SIZE })@(b#7120))[@inline] in
let concat#1134 =
  fun b1#7122 ->
  (fun b2#7123 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7122 , b2#7123))))[@inline] in
let sub#1135 =
  fun s#7125 ->
  (fun l#7126 ->
   (fun b#7127 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7125 ,
                                                                   l#7126) ,
                                                              b#7127)))))[@inline] in
let length#1140 = fun b#7138 -> (({ SIZE })@(b#7138))[@inline] in
let concat#1141 =
  fun b1#7140 ->
  (fun b2#7141 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7140 , b2#7141))))[@inline] in
let sub#1142 =
  fun s#7143 ->
  (fun l#7144 ->
   (fun b#7145 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7143 ,
                                                                   l#7144) ,
                                                              b#7145)))))[@inline] in
let blake2b#1143 = fun b#7147 -> (({ BLAKE2B })@(b#7147))[@inline] in
let sha256#1144 = fun b#7149 -> (({ SHA256 })@(b#7149))[@inline] in
let sha512#1145 = fun b#7151 -> (({ SHA512 })@(b#7151))[@inline] in
let sha3#1146 = fun b#7153 -> (({ SHA3 })@(b#7153))[@inline] in
let keccak#1147 = fun b#7155 -> (({ KECCAK })@(b#7155))[@inline] in
let hash_key#1148 = fun k#7157 -> (({ HASH_KEY })@(k#7157))[@inline] in
let check#1149 =
  fun k#7159 ->
  (fun s#7160 ->
   (fun b#7161 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7159 , s#7160) ,
                                                   b#7161)))))[@inline] in
let assert =
  fun b#7163 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7163))[@inline] in
let abs = fun i#7169 -> (({ ABS })@(i#7169))[@inline] in
let is_nat = fun i#7171 -> (({ ISNAT })@(i#7171))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b#7179 ->
  (fun s#7180 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7179 , s#7180))))[@inline] in
let poly_stub_39 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_38 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_37 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_36 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_35 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_34 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_33 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_32 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_31 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_30 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_29 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_28 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_27 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_26 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_25 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_24 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_23 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_22 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_21 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_20 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_19 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_18 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_17 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_16 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_15 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_14 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_13 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_12 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_11 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_10 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_9 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_8 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_7 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_6 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_5 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_4 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_3 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_2 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let poly_stub_1 = fun x#7191 -> (({ FAILWITH })@(x#7191))[@inline] in
let get_total_voting_power#1154 =
  fun _u#7200 -> ((poly_stub_39)@(L(unit)))[@inline] in
let set_source#1157 = fun _a#7206 -> ((poly_stub_38)@(L(unit)))[@inline] in
let get_storage_of_address#1158 =
  fun _a#7208 -> ((poly_stub_37)@(L(unit)))[@inline] in
let get_balance#1159 = fun _a#7210 -> ((poly_stub_36)@(L(unit)))[@inline] in
let print#1160 = fun _v#7212 -> ((poly_stub_35)@(L(unit)))[@inline] in
let eprint#1161 = fun _v#7214 -> ((poly_stub_34)@(L(unit)))[@inline] in
let get_voting_power#1162 =
  fun _kh#7216 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1163 =
  fun _i#7218 -> ((poly_stub_32)@(L(unit)))[@inline] in
let nth_bootstrap_account#1164 =
  fun _i#7220 -> ((poly_stub_31)@(L(unit)))[@inline] in
let get_bootstrap_account#1165 =
  fun _n#7222 -> ((poly_stub_30)@(L(unit)))[@inline] in
let last_originations#1167 =
  fun _u#7226 -> ((poly_stub_29)@(L(unit)))[@inline] in
let new_account#1169 = fun _u#7230 -> ((poly_stub_28)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1171 =
  fun _n#7234 -> ((poly_stub_27)@(L(unit)))[@inline] in
let register_delegate#1173 =
  fun _kh#7238 -> ((poly_stub_26)@(L(unit)))[@inline] in
let register_constant#1174 =
  fun _m#7240 -> ((poly_stub_25)@(L(unit)))[@inline] in
let constant_to_michelson_program#1176 =
  fun _s#7244 -> ((poly_stub_24)@(L(unit)))[@inline] in
let restore_context#1177 =
  fun _u#7246 -> ((poly_stub_23)@(L(unit)))[@inline] in
let save_context#1178 = fun _u#7248 -> ((poly_stub_22)@(L(unit)))[@inline] in
let drop_context#1179 = fun _u#7250 -> ((poly_stub_21)@(L(unit)))[@inline] in
let set_baker_policy#1182 =
  fun _bp#7256 -> ((poly_stub_20)@(L(unit)))[@inline] in
let set_baker#1183 = fun _a#7258 -> ((poly_stub_19)@(L(unit)))[@inline] in
let size#1184 = fun _c#7260 -> ((poly_stub_18)@(L(unit)))[@inline] in
let read_contract_from_file#1186 =
  fun _fn#7264 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#1187 = fun _n#7266 -> ((poly_stub_16)@(L(unit)))[@inline] in
let nl#1188 = L("NEWLINE")[@inline] in
let println#1189 = fun _v#7269 -> ((poly_stub_15)@(L(unit)))[@inline] in
let transfer#1190 =
  fun _a#7271 -> (fun _s#7272 -> (fun _t#7273 -> ((poly_stub_14)@(L(unit)))))[@inline] in
let transfer_exn#1191 =
  fun _a#7275 -> (fun _s#7276 -> (fun _t#7277 -> ((poly_stub_13)@(L(unit)))))[@inline] in
let reset_state#1193 =
  fun _n#7281 -> (fun _l#7282 -> ((poly_stub_12)@(L(unit))))[@inline] in
let reset_state_at#1194 =
  fun _t#7284 -> (fun _n#7285 -> (fun _l#7286 -> ((poly_stub_11)@(L(unit)))))[@inline] in
let save_mutation#1197 =
  fun _s#7295 -> (fun _m#7296 -> ((poly_stub_10)@(L(unit))))[@inline] in
let sign#1200 =
  fun _sk#7304 -> (fun _d#7305 -> ((poly_stub_9)@(L(unit))))[@inline] in
let add_account#1201 =
  fun _s#7307 -> (fun _k#7308 -> ((poly_stub_8)@(L(unit))))[@inline] in
let baker_account#1202 =
  fun _p#7310 -> (fun _o#7311 -> ((poly_stub_7)@(L(unit))))[@inline] in
let create_chest#1204 =
  fun _b#7316 -> (fun _n#7317 -> ((poly_stub_6)@(L(unit))))[@inline] in
let create_chest_key#1205 =
  fun _c#7319 -> (fun _n#7320 -> ((poly_stub_5)@(L(unit))))[@inline] in
let michelson_equal#1208 =
  fun _m1#7330 -> (fun _m2#7331 -> ((poly_stub_4)@(L(unit))))[@inline] in
let originate_contract#1210 =
  fun _c#7336 -> (fun _s#7337 -> (fun _t#7338 -> ((poly_stub_3)@(L(unit)))))[@inline] in
let compile_contract_from_file#1212 =
  fun _fn#7344 -> (fun _e#7345 -> (fun _v#7346 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let originate_from_file#1213 =
  fun _fn#7348 ->
  (fun _e#7349 ->
   (fun _v#7350 ->
    (fun _s#7351 -> (fun _t#7352 -> ((poly_stub_1)@(L(unit)))))))[@inline] in
let toto = ADD(toto#1061 , toto#228) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#7356 ->
  (let (gen#7362, gen#7363) = gen#7356 in
   let p#7357 = gen#7362 in
   let s#7358 = gen#7363 in
   let s#7359 = ADD(ADD(p#7357 , s#7358) , toto) in
   PAIR(LIST_EMPTY() , s#7359)) in
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
