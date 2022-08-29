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
                                              let s = ADD(ADD(p , s) ,
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
let get_balance#49 =
  fun _u#4400 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#50 =
  fun _u#4402 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#51 = fun _u#4404 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#52 =
  fun _u#4406 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#53 =
  fun _u#4408 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#54 = fun _u#4410 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#55 = fun _u#4412 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#56 =
  fun _u#4414 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#57 =
  fun _u#4416 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#58 =
  fun _u#4418 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#69 =
  fun kh#4430 -> (({ VOTING_POWER })@(kh#4430))[@inline] in
let implicit_account#71 =
  fun kh#4434 -> (IMPLICIT_ACCOUNT(kh#4434))[@inline] in
let pairing_check#75 =
  fun l#4442 -> (({ PAIRING_CHECK })@(l#4442))[@inline] in
let set_delegate#77 = fun o#4446 -> (SET_DELEGATE(o#4446))[@inline] in
let open_chest#83 =
  fun ck#4462 ->
  (fun c#4463 -> (fun n#4464 -> (OPEN_CHEST(ck#4462 , c#4463 , n#4464))))[@inline] in
let xor#86 = fun l#4473 -> (fun r#4474 -> (XOR(l#4473 , r#4474)))[@inline] in
let shift_left#87 =
  fun l#4476 -> (fun r#4477 -> (LSL(l#4476 , r#4477)))[@inline] in
let shift_right#88 =
  fun l#4479 -> (fun r#4480 -> (LSR(l#4479 , r#4480)))[@inline] in
let length#129 = fun b#4610 -> (({ SIZE })@(b#4610))[@inline] in
let concat#130 =
  fun b1#4612 ->
  (fun b2#4613 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4612 , b2#4613))))[@inline] in
let sub#131 =
  fun s#4615 ->
  (fun l#4616 ->
   (fun b#4617 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4615 ,
                                                                   l#4616) ,
                                                              b#4617)))))[@inline] in
let length#136 = fun b#4628 -> (({ SIZE })@(b#4628))[@inline] in
let concat#137 =
  fun b1#4630 ->
  (fun b2#4631 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4630 , b2#4631))))[@inline] in
let sub#138 =
  fun s#4633 ->
  (fun l#4634 ->
   (fun b#4635 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4633 ,
                                                                   l#4634) ,
                                                              b#4635)))))[@inline] in
let blake2b#139 = fun b#4637 -> (({ BLAKE2B })@(b#4637))[@inline] in
let sha256#140 = fun b#4639 -> (({ SHA256 })@(b#4639))[@inline] in
let sha512#141 = fun b#4641 -> (({ SHA512 })@(b#4641))[@inline] in
let sha3#142 = fun b#4643 -> (({ SHA3 })@(b#4643))[@inline] in
let keccak#143 = fun b#4645 -> (({ KECCAK })@(b#4645))[@inline] in
let hash_key#144 = fun k#4647 -> (({ HASH_KEY })@(k#4647))[@inline] in
let check#145 =
  fun k#4649 ->
  (fun s#4650 ->
   (fun b#4651 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4649 , s#4650) ,
                                                   b#4651)))))[@inline] in
let assert#146 =
  fun b#4653 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4653))[@inline] in
let abs#149 = fun i#4659 -> (({ ABS })@(i#4659))[@inline] in
let is_nat#150 = fun i#4661 -> (({ ISNAT })@(i#4661))[@inline] in
let true#151 = TRUE()[@inline] in
let false#152 = FALSE()[@inline] in
let unit#153 = UNIT()[@inline] in
let assert_with_error#156 =
  fun b#4669 ->
  (fun s#4670 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4669 , s#4670))))[@inline] in
let poly_stub_273 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_272 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_271 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_270 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_269 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_268 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_267 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_266 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_265 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_264 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_263 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_262 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_261 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_260 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_259 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_258 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_257 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_256 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_255 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_254 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_253 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_252 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_251 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_250 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_249 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_248 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_247 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_246 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_245 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_244 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_243 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_242 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_241 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_240 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_239 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_238 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_237 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_236 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let poly_stub_235 = fun x#4681 -> (({ FAILWITH })@(x#4681))[@inline] in
let get_total_voting_power#164 =
  fun _u#4690 -> ((poly_stub_273)@(L(unit)))[@inline] in
let set_source#167 = fun _a#4696 -> ((poly_stub_272)@(L(unit)))[@inline] in
let get_storage_of_address#168 =
  fun _a#4698 -> ((poly_stub_271)@(L(unit)))[@inline] in
let get_balance#169 = fun _a#4700 -> ((poly_stub_270)@(L(unit)))[@inline] in
let print#170 = fun _v#4702 -> ((poly_stub_269)@(L(unit)))[@inline] in
let eprint#171 = fun _v#4704 -> ((poly_stub_268)@(L(unit)))[@inline] in
let get_voting_power#172 =
  fun _kh#4706 -> ((poly_stub_267)@(L(unit)))[@inline] in
let nth_bootstrap_contract#173 =
  fun _i#4708 -> ((poly_stub_266)@(L(unit)))[@inline] in
let nth_bootstrap_account#174 =
  fun _i#4710 -> ((poly_stub_265)@(L(unit)))[@inline] in
let get_bootstrap_account#175 =
  fun _n#4712 -> ((poly_stub_264)@(L(unit)))[@inline] in
let last_originations#177 =
  fun _u#4716 -> ((poly_stub_263)@(L(unit)))[@inline] in
let new_account#179 = fun _u#4720 -> ((poly_stub_262)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#181 =
  fun _n#4724 -> ((poly_stub_261)@(L(unit)))[@inline] in
let register_delegate#183 =
  fun _kh#4728 -> ((poly_stub_260)@(L(unit)))[@inline] in
let register_constant#184 =
  fun _m#4730 -> ((poly_stub_259)@(L(unit)))[@inline] in
let constant_to_michelson_program#186 =
  fun _s#4734 -> ((poly_stub_258)@(L(unit)))[@inline] in
let restore_context#187 =
  fun _u#4736 -> ((poly_stub_257)@(L(unit)))[@inline] in
let save_context#188 = fun _u#4738 -> ((poly_stub_256)@(L(unit)))[@inline] in
let drop_context#189 = fun _u#4740 -> ((poly_stub_255)@(L(unit)))[@inline] in
let set_baker_policy#192 =
  fun _bp#4746 -> ((poly_stub_254)@(L(unit)))[@inline] in
let set_baker#193 = fun _a#4748 -> ((poly_stub_253)@(L(unit)))[@inline] in
let size#194 = fun _c#4750 -> ((poly_stub_252)@(L(unit)))[@inline] in
let read_contract_from_file#196 =
  fun _fn#4754 -> ((poly_stub_251)@(L(unit)))[@inline] in
let chr#197 = fun _n#4756 -> ((poly_stub_250)@(L(unit)))[@inline] in
let nl#198 = L("NEWLINE")[@inline] in
let println#199 = fun _v#4759 -> ((poly_stub_249)@(L(unit)))[@inline] in
let transfer#200 =
  fun _a#4761 ->
  (fun _s#4762 -> (fun _t#4763 -> ((poly_stub_248)@(L(unit)))))[@inline] in
let transfer_exn#201 =
  fun _a#4765 ->
  (fun _s#4766 -> (fun _t#4767 -> ((poly_stub_247)@(L(unit)))))[@inline] in
let reset_state#203 =
  fun _n#4771 -> (fun _l#4772 -> ((poly_stub_246)@(L(unit))))[@inline] in
let reset_state_at#204 =
  fun _t#4774 ->
  (fun _n#4775 -> (fun _l#4776 -> ((poly_stub_245)@(L(unit)))))[@inline] in
let save_mutation#207 =
  fun _s#4785 -> (fun _m#4786 -> ((poly_stub_244)@(L(unit))))[@inline] in
let sign#210 =
  fun _sk#4794 -> (fun _d#4795 -> ((poly_stub_243)@(L(unit))))[@inline] in
let add_account#211 =
  fun _s#4797 -> (fun _k#4798 -> ((poly_stub_242)@(L(unit))))[@inline] in
let baker_account#212 =
  fun _p#4800 -> (fun _o#4801 -> ((poly_stub_241)@(L(unit))))[@inline] in
let create_chest#214 =
  fun _b#4806 -> (fun _n#4807 -> ((poly_stub_240)@(L(unit))))[@inline] in
let create_chest_key#215 =
  fun _c#4809 -> (fun _n#4810 -> ((poly_stub_239)@(L(unit))))[@inline] in
let michelson_equal#218 =
  fun _m1#4820 -> (fun _m2#4821 -> ((poly_stub_238)@(L(unit))))[@inline] in
let originate_contract#220 =
  fun _c#4826 ->
  (fun _s#4827 -> (fun _t#4828 -> ((poly_stub_237)@(L(unit)))))[@inline] in
let compile_contract_from_file#222 =
  fun _fn#4834 ->
  (fun _e#4835 -> (fun _v#4836 -> ((poly_stub_236)@(L(unit)))))[@inline] in
let originate_from_file#223 =
  fun _fn#4838 ->
  (fun _e#4839 ->
   (fun _v#4840 ->
    (fun _s#4841 -> (fun _t#4842 -> ((poly_stub_235)@(L(unit)))))))[@inline] in
let toto#224 = L(1) in
let get_balance#225 =
  fun _u#4845 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#226 =
  fun _u#4847 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#227 = fun _u#4849 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#228 =
  fun _u#4851 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#229 =
  fun _u#4853 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#230 = fun _u#4855 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#231 = fun _u#4857 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#232 =
  fun _u#4859 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#233 =
  fun _u#4861 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#234 =
  fun _u#4863 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#245 =
  fun kh#4875 -> (({ VOTING_POWER })@(kh#4875))[@inline] in
let implicit_account#247 =
  fun kh#4879 -> (IMPLICIT_ACCOUNT(kh#4879))[@inline] in
let pairing_check#251 =
  fun l#4887 -> (({ PAIRING_CHECK })@(l#4887))[@inline] in
let set_delegate#253 = fun o#4891 -> (SET_DELEGATE(o#4891))[@inline] in
let open_chest#259 =
  fun ck#4907 ->
  (fun c#4908 -> (fun n#4909 -> (OPEN_CHEST(ck#4907 , c#4908 , n#4909))))[@inline] in
let xor#262 =
  fun l#4918 -> (fun r#4919 -> (XOR(l#4918 , r#4919)))[@inline] in
let shift_left#263 =
  fun l#4921 -> (fun r#4922 -> (LSL(l#4921 , r#4922)))[@inline] in
let shift_right#264 =
  fun l#4924 -> (fun r#4925 -> (LSR(l#4924 , r#4925)))[@inline] in
let length#305 = fun b#5055 -> (({ SIZE })@(b#5055))[@inline] in
let concat#306 =
  fun b1#5057 ->
  (fun b2#5058 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5057 , b2#5058))))[@inline] in
let sub#307 =
  fun s#5060 ->
  (fun l#5061 ->
   (fun b#5062 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5060 ,
                                                                   l#5061) ,
                                                              b#5062)))))[@inline] in
let length#312 = fun b#5073 -> (({ SIZE })@(b#5073))[@inline] in
let concat#313 =
  fun b1#5075 ->
  (fun b2#5076 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5075 , b2#5076))))[@inline] in
let sub#314 =
  fun s#5078 ->
  (fun l#5079 ->
   (fun b#5080 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5078 ,
                                                                   l#5079) ,
                                                              b#5080)))))[@inline] in
let blake2b#315 = fun b#5082 -> (({ BLAKE2B })@(b#5082))[@inline] in
let sha256#316 = fun b#5084 -> (({ SHA256 })@(b#5084))[@inline] in
let sha512#317 = fun b#5086 -> (({ SHA512 })@(b#5086))[@inline] in
let sha3#318 = fun b#5088 -> (({ SHA3 })@(b#5088))[@inline] in
let keccak#319 = fun b#5090 -> (({ KECCAK })@(b#5090))[@inline] in
let hash_key#320 = fun k#5092 -> (({ HASH_KEY })@(k#5092))[@inline] in
let check#321 =
  fun k#5094 ->
  (fun s#5095 ->
   (fun b#5096 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5094 , s#5095) ,
                                                   b#5096)))))[@inline] in
let assert#322 =
  fun b#5098 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5098))[@inline] in
let abs#325 = fun i#5104 -> (({ ABS })@(i#5104))[@inline] in
let is_nat#326 = fun i#5106 -> (({ ISNAT })@(i#5106))[@inline] in
let true#327 = TRUE()[@inline] in
let false#328 = FALSE()[@inline] in
let unit#329 = UNIT()[@inline] in
let assert_with_error#332 =
  fun b#5114 ->
  (fun s#5115 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5114 , s#5115))))[@inline] in
let poly_stub_234 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_233 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_232 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_231 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_230 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_229 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_228 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_227 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_226 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_225 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_224 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_223 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_222 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_221 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_220 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_219 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_218 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_217 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_216 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_215 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_214 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_213 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_212 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_211 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_210 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_209 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_208 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_207 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_206 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_205 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_204 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_203 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_202 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_201 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_200 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_199 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_198 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_197 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let poly_stub_196 = fun x#5126 -> (({ FAILWITH })@(x#5126))[@inline] in
let get_total_voting_power#340 =
  fun _u#5135 -> ((poly_stub_234)@(L(unit)))[@inline] in
let set_source#343 = fun _a#5141 -> ((poly_stub_233)@(L(unit)))[@inline] in
let get_storage_of_address#344 =
  fun _a#5143 -> ((poly_stub_232)@(L(unit)))[@inline] in
let get_balance#345 = fun _a#5145 -> ((poly_stub_231)@(L(unit)))[@inline] in
let print#346 = fun _v#5147 -> ((poly_stub_230)@(L(unit)))[@inline] in
let eprint#347 = fun _v#5149 -> ((poly_stub_229)@(L(unit)))[@inline] in
let get_voting_power#348 =
  fun _kh#5151 -> ((poly_stub_228)@(L(unit)))[@inline] in
let nth_bootstrap_contract#349 =
  fun _i#5153 -> ((poly_stub_227)@(L(unit)))[@inline] in
let nth_bootstrap_account#350 =
  fun _i#5155 -> ((poly_stub_226)@(L(unit)))[@inline] in
let get_bootstrap_account#351 =
  fun _n#5157 -> ((poly_stub_225)@(L(unit)))[@inline] in
let last_originations#353 =
  fun _u#5161 -> ((poly_stub_224)@(L(unit)))[@inline] in
let new_account#355 = fun _u#5165 -> ((poly_stub_223)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#357 =
  fun _n#5169 -> ((poly_stub_222)@(L(unit)))[@inline] in
let register_delegate#359 =
  fun _kh#5173 -> ((poly_stub_221)@(L(unit)))[@inline] in
let register_constant#360 =
  fun _m#5175 -> ((poly_stub_220)@(L(unit)))[@inline] in
let constant_to_michelson_program#362 =
  fun _s#5179 -> ((poly_stub_219)@(L(unit)))[@inline] in
let restore_context#363 =
  fun _u#5181 -> ((poly_stub_218)@(L(unit)))[@inline] in
let save_context#364 = fun _u#5183 -> ((poly_stub_217)@(L(unit)))[@inline] in
let drop_context#365 = fun _u#5185 -> ((poly_stub_216)@(L(unit)))[@inline] in
let set_baker_policy#368 =
  fun _bp#5191 -> ((poly_stub_215)@(L(unit)))[@inline] in
let set_baker#369 = fun _a#5193 -> ((poly_stub_214)@(L(unit)))[@inline] in
let size#370 = fun _c#5195 -> ((poly_stub_213)@(L(unit)))[@inline] in
let read_contract_from_file#372 =
  fun _fn#5199 -> ((poly_stub_212)@(L(unit)))[@inline] in
let chr#373 = fun _n#5201 -> ((poly_stub_211)@(L(unit)))[@inline] in
let nl#374 = L("NEWLINE")[@inline] in
let println#375 = fun _v#5204 -> ((poly_stub_210)@(L(unit)))[@inline] in
let transfer#376 =
  fun _a#5206 ->
  (fun _s#5207 -> (fun _t#5208 -> ((poly_stub_209)@(L(unit)))))[@inline] in
let transfer_exn#377 =
  fun _a#5210 ->
  (fun _s#5211 -> (fun _t#5212 -> ((poly_stub_208)@(L(unit)))))[@inline] in
let reset_state#379 =
  fun _n#5216 -> (fun _l#5217 -> ((poly_stub_207)@(L(unit))))[@inline] in
let reset_state_at#380 =
  fun _t#5219 ->
  (fun _n#5220 -> (fun _l#5221 -> ((poly_stub_206)@(L(unit)))))[@inline] in
let save_mutation#383 =
  fun _s#5230 -> (fun _m#5231 -> ((poly_stub_205)@(L(unit))))[@inline] in
let sign#386 =
  fun _sk#5239 -> (fun _d#5240 -> ((poly_stub_204)@(L(unit))))[@inline] in
let add_account#387 =
  fun _s#5242 -> (fun _k#5243 -> ((poly_stub_203)@(L(unit))))[@inline] in
let baker_account#388 =
  fun _p#5245 -> (fun _o#5246 -> ((poly_stub_202)@(L(unit))))[@inline] in
let create_chest#390 =
  fun _b#5251 -> (fun _n#5252 -> ((poly_stub_201)@(L(unit))))[@inline] in
let create_chest_key#391 =
  fun _c#5254 -> (fun _n#5255 -> ((poly_stub_200)@(L(unit))))[@inline] in
let michelson_equal#394 =
  fun _m1#5265 -> (fun _m2#5266 -> ((poly_stub_199)@(L(unit))))[@inline] in
let originate_contract#396 =
  fun _c#5271 ->
  (fun _s#5272 -> (fun _t#5273 -> ((poly_stub_198)@(L(unit)))))[@inline] in
let compile_contract_from_file#398 =
  fun _fn#5279 ->
  (fun _e#5280 -> (fun _v#5281 -> ((poly_stub_197)@(L(unit)))))[@inline] in
let originate_from_file#399 =
  fun _fn#5283 ->
  (fun _e#5284 ->
   (fun _v#5285 ->
    (fun _s#5286 -> (fun _t#5287 -> ((poly_stub_196)@(L(unit)))))))[@inline] in
let toto#400 = L(32) in
let titi#401 = ADD(toto#224 , L(42)) in
let f#402 =
  fun gen#5291 ->
  (let (gen#7528, gen#7529) = gen#5291 in
   let gen#5292 = gen#7528 in
   let x#5293 = gen#7529 in
   let x#5294 = ADD(ADD(x#5293 , toto#224) , titi#401) in
   PAIR(LIST_EMPTY() , x#5294)) in
let get_balance#403 =
  fun _u#5296 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#404 =
  fun _u#5298 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#405 = fun _u#5300 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#406 =
  fun _u#5302 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#407 =
  fun _u#5304 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#408 = fun _u#5306 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#409 = fun _u#5308 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#410 =
  fun _u#5310 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#411 =
  fun _u#5312 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#412 =
  fun _u#5314 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#423 =
  fun kh#5326 -> (({ VOTING_POWER })@(kh#5326))[@inline] in
let implicit_account#425 =
  fun kh#5330 -> (IMPLICIT_ACCOUNT(kh#5330))[@inline] in
let pairing_check#429 =
  fun l#5338 -> (({ PAIRING_CHECK })@(l#5338))[@inline] in
let set_delegate#431 = fun o#5342 -> (SET_DELEGATE(o#5342))[@inline] in
let open_chest#437 =
  fun ck#5358 ->
  (fun c#5359 -> (fun n#5360 -> (OPEN_CHEST(ck#5358 , c#5359 , n#5360))))[@inline] in
let xor#440 =
  fun l#5369 -> (fun r#5370 -> (XOR(l#5369 , r#5370)))[@inline] in
let shift_left#441 =
  fun l#5372 -> (fun r#5373 -> (LSL(l#5372 , r#5373)))[@inline] in
let shift_right#442 =
  fun l#5375 -> (fun r#5376 -> (LSR(l#5375 , r#5376)))[@inline] in
let length#483 = fun b#5506 -> (({ SIZE })@(b#5506))[@inline] in
let concat#484 =
  fun b1#5508 ->
  (fun b2#5509 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5508 , b2#5509))))[@inline] in
let sub#485 =
  fun s#5511 ->
  (fun l#5512 ->
   (fun b#5513 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5511 ,
                                                                   l#5512) ,
                                                              b#5513)))))[@inline] in
let length#490 = fun b#5524 -> (({ SIZE })@(b#5524))[@inline] in
let concat#491 =
  fun b1#5526 ->
  (fun b2#5527 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5526 , b2#5527))))[@inline] in
let sub#492 =
  fun s#5529 ->
  (fun l#5530 ->
   (fun b#5531 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5529 ,
                                                                   l#5530) ,
                                                              b#5531)))))[@inline] in
let blake2b#493 = fun b#5533 -> (({ BLAKE2B })@(b#5533))[@inline] in
let sha256#494 = fun b#5535 -> (({ SHA256 })@(b#5535))[@inline] in
let sha512#495 = fun b#5537 -> (({ SHA512 })@(b#5537))[@inline] in
let sha3#496 = fun b#5539 -> (({ SHA3 })@(b#5539))[@inline] in
let keccak#497 = fun b#5541 -> (({ KECCAK })@(b#5541))[@inline] in
let hash_key#498 = fun k#5543 -> (({ HASH_KEY })@(k#5543))[@inline] in
let check#499 =
  fun k#5545 ->
  (fun s#5546 ->
   (fun b#5547 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5545 , s#5546) ,
                                                   b#5547)))))[@inline] in
let assert#500 =
  fun b#5549 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5549))[@inline] in
let abs#503 = fun i#5555 -> (({ ABS })@(i#5555))[@inline] in
let is_nat#504 = fun i#5557 -> (({ ISNAT })@(i#5557))[@inline] in
let true#505 = TRUE()[@inline] in
let false#506 = FALSE()[@inline] in
let unit#507 = UNIT()[@inline] in
let assert_with_error#510 =
  fun b#5565 ->
  (fun s#5566 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5565 , s#5566))))[@inline] in
let poly_stub_195 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_194 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_193 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_192 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_191 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_190 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_189 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_188 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_187 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_186 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_185 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_184 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_183 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_182 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_181 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_180 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_179 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_178 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_177 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_176 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_175 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_174 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_173 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_172 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_171 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_170 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_169 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_168 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_167 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_166 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_165 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_164 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_163 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_162 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_161 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_160 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_159 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_158 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let poly_stub_157 = fun x#5577 -> (({ FAILWITH })@(x#5577))[@inline] in
let get_total_voting_power#518 =
  fun _u#5586 -> ((poly_stub_195)@(L(unit)))[@inline] in
let set_source#521 = fun _a#5592 -> ((poly_stub_194)@(L(unit)))[@inline] in
let get_storage_of_address#522 =
  fun _a#5594 -> ((poly_stub_193)@(L(unit)))[@inline] in
let get_balance#523 = fun _a#5596 -> ((poly_stub_192)@(L(unit)))[@inline] in
let print#524 = fun _v#5598 -> ((poly_stub_191)@(L(unit)))[@inline] in
let eprint#525 = fun _v#5600 -> ((poly_stub_190)@(L(unit)))[@inline] in
let get_voting_power#526 =
  fun _kh#5602 -> ((poly_stub_189)@(L(unit)))[@inline] in
let nth_bootstrap_contract#527 =
  fun _i#5604 -> ((poly_stub_188)@(L(unit)))[@inline] in
let nth_bootstrap_account#528 =
  fun _i#5606 -> ((poly_stub_187)@(L(unit)))[@inline] in
let get_bootstrap_account#529 =
  fun _n#5608 -> ((poly_stub_186)@(L(unit)))[@inline] in
let last_originations#531 =
  fun _u#5612 -> ((poly_stub_185)@(L(unit)))[@inline] in
let new_account#533 = fun _u#5616 -> ((poly_stub_184)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#535 =
  fun _n#5620 -> ((poly_stub_183)@(L(unit)))[@inline] in
let register_delegate#537 =
  fun _kh#5624 -> ((poly_stub_182)@(L(unit)))[@inline] in
let register_constant#538 =
  fun _m#5626 -> ((poly_stub_181)@(L(unit)))[@inline] in
let constant_to_michelson_program#540 =
  fun _s#5630 -> ((poly_stub_180)@(L(unit)))[@inline] in
let restore_context#541 =
  fun _u#5632 -> ((poly_stub_179)@(L(unit)))[@inline] in
let save_context#542 = fun _u#5634 -> ((poly_stub_178)@(L(unit)))[@inline] in
let drop_context#543 = fun _u#5636 -> ((poly_stub_177)@(L(unit)))[@inline] in
let set_baker_policy#546 =
  fun _bp#5642 -> ((poly_stub_176)@(L(unit)))[@inline] in
let set_baker#547 = fun _a#5644 -> ((poly_stub_175)@(L(unit)))[@inline] in
let size#548 = fun _c#5646 -> ((poly_stub_174)@(L(unit)))[@inline] in
let read_contract_from_file#550 =
  fun _fn#5650 -> ((poly_stub_173)@(L(unit)))[@inline] in
let chr#551 = fun _n#5652 -> ((poly_stub_172)@(L(unit)))[@inline] in
let nl#552 = L("NEWLINE")[@inline] in
let println#553 = fun _v#5655 -> ((poly_stub_171)@(L(unit)))[@inline] in
let transfer#554 =
  fun _a#5657 ->
  (fun _s#5658 -> (fun _t#5659 -> ((poly_stub_170)@(L(unit)))))[@inline] in
let transfer_exn#555 =
  fun _a#5661 ->
  (fun _s#5662 -> (fun _t#5663 -> ((poly_stub_169)@(L(unit)))))[@inline] in
let reset_state#557 =
  fun _n#5667 -> (fun _l#5668 -> ((poly_stub_168)@(L(unit))))[@inline] in
let reset_state_at#558 =
  fun _t#5670 ->
  (fun _n#5671 -> (fun _l#5672 -> ((poly_stub_167)@(L(unit)))))[@inline] in
let save_mutation#561 =
  fun _s#5681 -> (fun _m#5682 -> ((poly_stub_166)@(L(unit))))[@inline] in
let sign#564 =
  fun _sk#5690 -> (fun _d#5691 -> ((poly_stub_165)@(L(unit))))[@inline] in
let add_account#565 =
  fun _s#5693 -> (fun _k#5694 -> ((poly_stub_164)@(L(unit))))[@inline] in
let baker_account#566 =
  fun _p#5696 -> (fun _o#5697 -> ((poly_stub_163)@(L(unit))))[@inline] in
let create_chest#568 =
  fun _b#5702 -> (fun _n#5703 -> ((poly_stub_162)@(L(unit))))[@inline] in
let create_chest_key#569 =
  fun _c#5705 -> (fun _n#5706 -> ((poly_stub_161)@(L(unit))))[@inline] in
let michelson_equal#572 =
  fun _m1#5716 -> (fun _m2#5717 -> ((poly_stub_160)@(L(unit))))[@inline] in
let originate_contract#574 =
  fun _c#5722 ->
  (fun _s#5723 -> (fun _t#5724 -> ((poly_stub_159)@(L(unit)))))[@inline] in
let compile_contract_from_file#576 =
  fun _fn#5730 ->
  (fun _e#5731 -> (fun _v#5732 -> ((poly_stub_158)@(L(unit)))))[@inline] in
let originate_from_file#577 =
  fun _fn#5734 ->
  (fun _e#5735 ->
   (fun _v#5736 ->
    (fun _s#5737 -> (fun _t#5738 -> ((poly_stub_157)@(L(unit)))))))[@inline] in
let toto#578 = L(44) in
let get_balance#579 =
  fun _u#5741 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#580 =
  fun _u#5743 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#581 = fun _u#5745 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#582 =
  fun _u#5747 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#583 =
  fun _u#5749 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#584 = fun _u#5751 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#585 = fun _u#5753 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#586 =
  fun _u#5755 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#587 =
  fun _u#5757 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#588 =
  fun _u#5759 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#599 =
  fun kh#5771 -> (({ VOTING_POWER })@(kh#5771))[@inline] in
let implicit_account#601 =
  fun kh#5775 -> (IMPLICIT_ACCOUNT(kh#5775))[@inline] in
let pairing_check#605 =
  fun l#5783 -> (({ PAIRING_CHECK })@(l#5783))[@inline] in
let set_delegate#607 = fun o#5787 -> (SET_DELEGATE(o#5787))[@inline] in
let open_chest#613 =
  fun ck#5803 ->
  (fun c#5804 -> (fun n#5805 -> (OPEN_CHEST(ck#5803 , c#5804 , n#5805))))[@inline] in
let xor#616 =
  fun l#5814 -> (fun r#5815 -> (XOR(l#5814 , r#5815)))[@inline] in
let shift_left#617 =
  fun l#5817 -> (fun r#5818 -> (LSL(l#5817 , r#5818)))[@inline] in
let shift_right#618 =
  fun l#5820 -> (fun r#5821 -> (LSR(l#5820 , r#5821)))[@inline] in
let length#659 = fun b#5951 -> (({ SIZE })@(b#5951))[@inline] in
let concat#660 =
  fun b1#5953 ->
  (fun b2#5954 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5953 , b2#5954))))[@inline] in
let sub#661 =
  fun s#5956 ->
  (fun l#5957 ->
   (fun b#5958 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5956 ,
                                                                   l#5957) ,
                                                              b#5958)))))[@inline] in
let length#666 = fun b#5969 -> (({ SIZE })@(b#5969))[@inline] in
let concat#667 =
  fun b1#5971 ->
  (fun b2#5972 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5971 , b2#5972))))[@inline] in
let sub#668 =
  fun s#5974 ->
  (fun l#5975 ->
   (fun b#5976 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5974 ,
                                                                   l#5975) ,
                                                              b#5976)))))[@inline] in
let blake2b#669 = fun b#5978 -> (({ BLAKE2B })@(b#5978))[@inline] in
let sha256#670 = fun b#5980 -> (({ SHA256 })@(b#5980))[@inline] in
let sha512#671 = fun b#5982 -> (({ SHA512 })@(b#5982))[@inline] in
let sha3#672 = fun b#5984 -> (({ SHA3 })@(b#5984))[@inline] in
let keccak#673 = fun b#5986 -> (({ KECCAK })@(b#5986))[@inline] in
let hash_key#674 = fun k#5988 -> (({ HASH_KEY })@(k#5988))[@inline] in
let check#675 =
  fun k#5990 ->
  (fun s#5991 ->
   (fun b#5992 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5990 , s#5991) ,
                                                   b#5992)))))[@inline] in
let assert#676 =
  fun b#5994 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5994))[@inline] in
let abs#679 = fun i#6000 -> (({ ABS })@(i#6000))[@inline] in
let is_nat#680 = fun i#6002 -> (({ ISNAT })@(i#6002))[@inline] in
let true#681 = TRUE()[@inline] in
let false#682 = FALSE()[@inline] in
let unit#683 = UNIT()[@inline] in
let assert_with_error#686 =
  fun b#6010 ->
  (fun s#6011 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6010 , s#6011))))[@inline] in
let poly_stub_156 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_155 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_154 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_153 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_152 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_151 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_150 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_149 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_148 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_147 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_146 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_145 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_144 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_143 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_142 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_141 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_140 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_139 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_138 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_137 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_136 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_135 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_134 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_133 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_132 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_131 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_130 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_129 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_128 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_127 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_126 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_125 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_124 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_123 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_122 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_121 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_120 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_119 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let poly_stub_118 = fun x#6022 -> (({ FAILWITH })@(x#6022))[@inline] in
let get_total_voting_power#694 =
  fun _u#6031 -> ((poly_stub_156)@(L(unit)))[@inline] in
let set_source#697 = fun _a#6037 -> ((poly_stub_155)@(L(unit)))[@inline] in
let get_storage_of_address#698 =
  fun _a#6039 -> ((poly_stub_154)@(L(unit)))[@inline] in
let get_balance#699 = fun _a#6041 -> ((poly_stub_153)@(L(unit)))[@inline] in
let print#700 = fun _v#6043 -> ((poly_stub_152)@(L(unit)))[@inline] in
let eprint#701 = fun _v#6045 -> ((poly_stub_151)@(L(unit)))[@inline] in
let get_voting_power#702 =
  fun _kh#6047 -> ((poly_stub_150)@(L(unit)))[@inline] in
let nth_bootstrap_contract#703 =
  fun _i#6049 -> ((poly_stub_149)@(L(unit)))[@inline] in
let nth_bootstrap_account#704 =
  fun _i#6051 -> ((poly_stub_148)@(L(unit)))[@inline] in
let get_bootstrap_account#705 =
  fun _n#6053 -> ((poly_stub_147)@(L(unit)))[@inline] in
let last_originations#707 =
  fun _u#6057 -> ((poly_stub_146)@(L(unit)))[@inline] in
let new_account#709 = fun _u#6061 -> ((poly_stub_145)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#711 =
  fun _n#6065 -> ((poly_stub_144)@(L(unit)))[@inline] in
let register_delegate#713 =
  fun _kh#6069 -> ((poly_stub_143)@(L(unit)))[@inline] in
let register_constant#714 =
  fun _m#6071 -> ((poly_stub_142)@(L(unit)))[@inline] in
let constant_to_michelson_program#716 =
  fun _s#6075 -> ((poly_stub_141)@(L(unit)))[@inline] in
let restore_context#717 =
  fun _u#6077 -> ((poly_stub_140)@(L(unit)))[@inline] in
let save_context#718 = fun _u#6079 -> ((poly_stub_139)@(L(unit)))[@inline] in
let drop_context#719 = fun _u#6081 -> ((poly_stub_138)@(L(unit)))[@inline] in
let set_baker_policy#722 =
  fun _bp#6087 -> ((poly_stub_137)@(L(unit)))[@inline] in
let set_baker#723 = fun _a#6089 -> ((poly_stub_136)@(L(unit)))[@inline] in
let size#724 = fun _c#6091 -> ((poly_stub_135)@(L(unit)))[@inline] in
let read_contract_from_file#726 =
  fun _fn#6095 -> ((poly_stub_134)@(L(unit)))[@inline] in
let chr#727 = fun _n#6097 -> ((poly_stub_133)@(L(unit)))[@inline] in
let nl#728 = L("NEWLINE")[@inline] in
let println#729 = fun _v#6100 -> ((poly_stub_132)@(L(unit)))[@inline] in
let transfer#730 =
  fun _a#6102 ->
  (fun _s#6103 -> (fun _t#6104 -> ((poly_stub_131)@(L(unit)))))[@inline] in
let transfer_exn#731 =
  fun _a#6106 ->
  (fun _s#6107 -> (fun _t#6108 -> ((poly_stub_130)@(L(unit)))))[@inline] in
let reset_state#733 =
  fun _n#6112 -> (fun _l#6113 -> ((poly_stub_129)@(L(unit))))[@inline] in
let reset_state_at#734 =
  fun _t#6115 ->
  (fun _n#6116 -> (fun _l#6117 -> ((poly_stub_128)@(L(unit)))))[@inline] in
let save_mutation#737 =
  fun _s#6126 -> (fun _m#6127 -> ((poly_stub_127)@(L(unit))))[@inline] in
let sign#740 =
  fun _sk#6135 -> (fun _d#6136 -> ((poly_stub_126)@(L(unit))))[@inline] in
let add_account#741 =
  fun _s#6138 -> (fun _k#6139 -> ((poly_stub_125)@(L(unit))))[@inline] in
let baker_account#742 =
  fun _p#6141 -> (fun _o#6142 -> ((poly_stub_124)@(L(unit))))[@inline] in
let create_chest#744 =
  fun _b#6147 -> (fun _n#6148 -> ((poly_stub_123)@(L(unit))))[@inline] in
let create_chest_key#745 =
  fun _c#6150 -> (fun _n#6151 -> ((poly_stub_122)@(L(unit))))[@inline] in
let michelson_equal#748 =
  fun _m1#6161 -> (fun _m2#6162 -> ((poly_stub_121)@(L(unit))))[@inline] in
let originate_contract#750 =
  fun _c#6167 ->
  (fun _s#6168 -> (fun _t#6169 -> ((poly_stub_120)@(L(unit)))))[@inline] in
let compile_contract_from_file#752 =
  fun _fn#6175 ->
  (fun _e#6176 -> (fun _v#6177 -> ((poly_stub_119)@(L(unit)))))[@inline] in
let originate_from_file#753 =
  fun _fn#6179 ->
  (fun _e#6180 ->
   (fun _v#6181 ->
    (fun _s#6182 -> (fun _t#6183 -> ((poly_stub_118)@(L(unit)))))))[@inline] in
let toto#754 = L(43) in
let get_balance#755 =
  fun _u#6186 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#756 =
  fun _u#6188 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#757 = fun _u#6190 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#758 =
  fun _u#6192 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#759 =
  fun _u#6194 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#760 = fun _u#6196 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#761 = fun _u#6198 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#762 =
  fun _u#6200 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#763 =
  fun _u#6202 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#764 =
  fun _u#6204 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#775 =
  fun kh#6216 -> (({ VOTING_POWER })@(kh#6216))[@inline] in
let implicit_account#777 =
  fun kh#6220 -> (IMPLICIT_ACCOUNT(kh#6220))[@inline] in
let pairing_check#781 =
  fun l#6228 -> (({ PAIRING_CHECK })@(l#6228))[@inline] in
let set_delegate#783 = fun o#6232 -> (SET_DELEGATE(o#6232))[@inline] in
let open_chest#789 =
  fun ck#6248 ->
  (fun c#6249 -> (fun n#6250 -> (OPEN_CHEST(ck#6248 , c#6249 , n#6250))))[@inline] in
let xor#792 =
  fun l#6259 -> (fun r#6260 -> (XOR(l#6259 , r#6260)))[@inline] in
let shift_left#793 =
  fun l#6262 -> (fun r#6263 -> (LSL(l#6262 , r#6263)))[@inline] in
let shift_right#794 =
  fun l#6265 -> (fun r#6266 -> (LSR(l#6265 , r#6266)))[@inline] in
let length#835 = fun b#6396 -> (({ SIZE })@(b#6396))[@inline] in
let concat#836 =
  fun b1#6398 ->
  (fun b2#6399 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6398 , b2#6399))))[@inline] in
let sub#837 =
  fun s#6401 ->
  (fun l#6402 ->
   (fun b#6403 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6401 ,
                                                                   l#6402) ,
                                                              b#6403)))))[@inline] in
let length#842 = fun b#6414 -> (({ SIZE })@(b#6414))[@inline] in
let concat#843 =
  fun b1#6416 ->
  (fun b2#6417 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6416 , b2#6417))))[@inline] in
let sub#844 =
  fun s#6419 ->
  (fun l#6420 ->
   (fun b#6421 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6419 ,
                                                                   l#6420) ,
                                                              b#6421)))))[@inline] in
let blake2b#845 = fun b#6423 -> (({ BLAKE2B })@(b#6423))[@inline] in
let sha256#846 = fun b#6425 -> (({ SHA256 })@(b#6425))[@inline] in
let sha512#847 = fun b#6427 -> (({ SHA512 })@(b#6427))[@inline] in
let sha3#848 = fun b#6429 -> (({ SHA3 })@(b#6429))[@inline] in
let keccak#849 = fun b#6431 -> (({ KECCAK })@(b#6431))[@inline] in
let hash_key#850 = fun k#6433 -> (({ HASH_KEY })@(k#6433))[@inline] in
let check#851 =
  fun k#6435 ->
  (fun s#6436 ->
   (fun b#6437 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6435 , s#6436) ,
                                                   b#6437)))))[@inline] in
let assert#852 =
  fun b#6439 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6439))[@inline] in
let abs#855 = fun i#6445 -> (({ ABS })@(i#6445))[@inline] in
let is_nat#856 = fun i#6447 -> (({ ISNAT })@(i#6447))[@inline] in
let true#857 = TRUE()[@inline] in
let false#858 = FALSE()[@inline] in
let unit#859 = UNIT()[@inline] in
let assert_with_error#862 =
  fun b#6455 ->
  (fun s#6456 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6455 , s#6456))))[@inline] in
let poly_stub_117 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_116 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_115 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_114 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_113 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_112 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_111 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_110 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_109 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_108 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_107 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_106 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_105 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_104 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_103 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_102 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_101 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_100 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_99 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_98 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_97 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_96 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_95 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_94 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_93 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_92 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_91 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_90 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_89 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_88 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_87 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_86 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_85 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_84 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_83 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_82 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_81 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_80 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let poly_stub_79 = fun x#6467 -> (({ FAILWITH })@(x#6467))[@inline] in
let get_total_voting_power#870 =
  fun _u#6476 -> ((poly_stub_117)@(L(unit)))[@inline] in
let set_source#873 = fun _a#6482 -> ((poly_stub_116)@(L(unit)))[@inline] in
let get_storage_of_address#874 =
  fun _a#6484 -> ((poly_stub_115)@(L(unit)))[@inline] in
let get_balance#875 = fun _a#6486 -> ((poly_stub_114)@(L(unit)))[@inline] in
let print#876 = fun _v#6488 -> ((poly_stub_113)@(L(unit)))[@inline] in
let eprint#877 = fun _v#6490 -> ((poly_stub_112)@(L(unit)))[@inline] in
let get_voting_power#878 =
  fun _kh#6492 -> ((poly_stub_111)@(L(unit)))[@inline] in
let nth_bootstrap_contract#879 =
  fun _i#6494 -> ((poly_stub_110)@(L(unit)))[@inline] in
let nth_bootstrap_account#880 =
  fun _i#6496 -> ((poly_stub_109)@(L(unit)))[@inline] in
let get_bootstrap_account#881 =
  fun _n#6498 -> ((poly_stub_108)@(L(unit)))[@inline] in
let last_originations#883 =
  fun _u#6502 -> ((poly_stub_107)@(L(unit)))[@inline] in
let new_account#885 = fun _u#6506 -> ((poly_stub_106)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#887 =
  fun _n#6510 -> ((poly_stub_105)@(L(unit)))[@inline] in
let register_delegate#889 =
  fun _kh#6514 -> ((poly_stub_104)@(L(unit)))[@inline] in
let register_constant#890 =
  fun _m#6516 -> ((poly_stub_103)@(L(unit)))[@inline] in
let constant_to_michelson_program#892 =
  fun _s#6520 -> ((poly_stub_102)@(L(unit)))[@inline] in
let restore_context#893 =
  fun _u#6522 -> ((poly_stub_101)@(L(unit)))[@inline] in
let save_context#894 = fun _u#6524 -> ((poly_stub_100)@(L(unit)))[@inline] in
let drop_context#895 = fun _u#6526 -> ((poly_stub_99)@(L(unit)))[@inline] in
let set_baker_policy#898 =
  fun _bp#6532 -> ((poly_stub_98)@(L(unit)))[@inline] in
let set_baker#899 = fun _a#6534 -> ((poly_stub_97)@(L(unit)))[@inline] in
let size#900 = fun _c#6536 -> ((poly_stub_96)@(L(unit)))[@inline] in
let read_contract_from_file#902 =
  fun _fn#6540 -> ((poly_stub_95)@(L(unit)))[@inline] in
let chr#903 = fun _n#6542 -> ((poly_stub_94)@(L(unit)))[@inline] in
let nl#904 = L("NEWLINE")[@inline] in
let println#905 = fun _v#6545 -> ((poly_stub_93)@(L(unit)))[@inline] in
let transfer#906 =
  fun _a#6547 -> (fun _s#6548 -> (fun _t#6549 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let transfer_exn#907 =
  fun _a#6551 -> (fun _s#6552 -> (fun _t#6553 -> ((poly_stub_91)@(L(unit)))))[@inline] in
let reset_state#909 =
  fun _n#6557 -> (fun _l#6558 -> ((poly_stub_90)@(L(unit))))[@inline] in
let reset_state_at#910 =
  fun _t#6560 -> (fun _n#6561 -> (fun _l#6562 -> ((poly_stub_89)@(L(unit)))))[@inline] in
let save_mutation#913 =
  fun _s#6571 -> (fun _m#6572 -> ((poly_stub_88)@(L(unit))))[@inline] in
let sign#916 =
  fun _sk#6580 -> (fun _d#6581 -> ((poly_stub_87)@(L(unit))))[@inline] in
let add_account#917 =
  fun _s#6583 -> (fun _k#6584 -> ((poly_stub_86)@(L(unit))))[@inline] in
let baker_account#918 =
  fun _p#6586 -> (fun _o#6587 -> ((poly_stub_85)@(L(unit))))[@inline] in
let create_chest#920 =
  fun _b#6592 -> (fun _n#6593 -> ((poly_stub_84)@(L(unit))))[@inline] in
let create_chest_key#921 =
  fun _c#6595 -> (fun _n#6596 -> ((poly_stub_83)@(L(unit))))[@inline] in
let michelson_equal#924 =
  fun _m1#6606 -> (fun _m2#6607 -> ((poly_stub_82)@(L(unit))))[@inline] in
let originate_contract#926 =
  fun _c#6612 -> (fun _s#6613 -> (fun _t#6614 -> ((poly_stub_81)@(L(unit)))))[@inline] in
let compile_contract_from_file#928 =
  fun _fn#6620 ->
  (fun _e#6621 -> (fun _v#6622 -> ((poly_stub_80)@(L(unit)))))[@inline] in
let originate_from_file#929 =
  fun _fn#6624 ->
  (fun _e#6625 ->
   (fun _v#6626 ->
    (fun _s#6627 -> (fun _t#6628 -> ((poly_stub_79)@(L(unit)))))))[@inline] in
let tata#930 = ADD(toto#224 , titi#401) in
let foo#931 = (f#402)@(PAIR(L(unit) , L(3))) in
let get_balance#932 =
  fun _u#6632 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#933 =
  fun _u#6634 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#934 = fun _u#6636 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#935 =
  fun _u#6638 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#936 =
  fun _u#6640 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#937 = fun _u#6642 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#938 = fun _u#6644 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#939 =
  fun _u#6646 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#940 =
  fun _u#6648 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#941 =
  fun _u#6650 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#952 =
  fun kh#6662 -> (({ VOTING_POWER })@(kh#6662))[@inline] in
let implicit_account#954 =
  fun kh#6666 -> (IMPLICIT_ACCOUNT(kh#6666))[@inline] in
let pairing_check#958 =
  fun l#6674 -> (({ PAIRING_CHECK })@(l#6674))[@inline] in
let set_delegate#960 = fun o#6678 -> (SET_DELEGATE(o#6678))[@inline] in
let open_chest#966 =
  fun ck#6694 ->
  (fun c#6695 -> (fun n#6696 -> (OPEN_CHEST(ck#6694 , c#6695 , n#6696))))[@inline] in
let xor#969 =
  fun l#6705 -> (fun r#6706 -> (XOR(l#6705 , r#6706)))[@inline] in
let shift_left#970 =
  fun l#6708 -> (fun r#6709 -> (LSL(l#6708 , r#6709)))[@inline] in
let shift_right#971 =
  fun l#6711 -> (fun r#6712 -> (LSR(l#6711 , r#6712)))[@inline] in
let length#1012 = fun b#6842 -> (({ SIZE })@(b#6842))[@inline] in
let concat#1013 =
  fun b1#6844 ->
  (fun b2#6845 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6844 , b2#6845))))[@inline] in
let sub#1014 =
  fun s#6847 ->
  (fun l#6848 ->
   (fun b#6849 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6847 ,
                                                                   l#6848) ,
                                                              b#6849)))))[@inline] in
let length#1019 = fun b#6860 -> (({ SIZE })@(b#6860))[@inline] in
let concat#1020 =
  fun b1#6862 ->
  (fun b2#6863 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6862 , b2#6863))))[@inline] in
let sub#1021 =
  fun s#6865 ->
  (fun l#6866 ->
   (fun b#6867 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6865 ,
                                                                   l#6866) ,
                                                              b#6867)))))[@inline] in
let blake2b#1022 = fun b#6869 -> (({ BLAKE2B })@(b#6869))[@inline] in
let sha256#1023 = fun b#6871 -> (({ SHA256 })@(b#6871))[@inline] in
let sha512#1024 = fun b#6873 -> (({ SHA512 })@(b#6873))[@inline] in
let sha3#1025 = fun b#6875 -> (({ SHA3 })@(b#6875))[@inline] in
let keccak#1026 = fun b#6877 -> (({ KECCAK })@(b#6877))[@inline] in
let hash_key#1027 = fun k#6879 -> (({ HASH_KEY })@(k#6879))[@inline] in
let check#1028 =
  fun k#6881 ->
  (fun s#6882 ->
   (fun b#6883 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6881 , s#6882) ,
                                                   b#6883)))))[@inline] in
let assert#1029 =
  fun b#6885 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6885))[@inline] in
let abs#1032 = fun i#6891 -> (({ ABS })@(i#6891))[@inline] in
let is_nat#1033 = fun i#6893 -> (({ ISNAT })@(i#6893))[@inline] in
let true#1034 = TRUE()[@inline] in
let false#1035 = FALSE()[@inline] in
let unit#1036 = UNIT()[@inline] in
let assert_with_error#1039 =
  fun b#6901 ->
  (fun s#6902 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6901 , s#6902))))[@inline] in
let poly_stub_78 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_77 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_76 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_75 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_74 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_73 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_72 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_71 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_70 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_69 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_68 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_67 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_66 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_65 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_64 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_63 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_62 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_61 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_60 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_59 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_58 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_57 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_56 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_55 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_54 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_53 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_52 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_51 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_50 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_49 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_48 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_47 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_46 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_45 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_44 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_43 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_42 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_41 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let poly_stub_40 = fun x#6913 -> (({ FAILWITH })@(x#6913))[@inline] in
let get_total_voting_power#1047 =
  fun _u#6922 -> ((poly_stub_78)@(L(unit)))[@inline] in
let set_source#1050 = fun _a#6928 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#1051 =
  fun _a#6930 -> ((poly_stub_76)@(L(unit)))[@inline] in
let get_balance#1052 = fun _a#6932 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#1053 = fun _v#6934 -> ((poly_stub_74)@(L(unit)))[@inline] in
let eprint#1054 = fun _v#6936 -> ((poly_stub_73)@(L(unit)))[@inline] in
let get_voting_power#1055 =
  fun _kh#6938 -> ((poly_stub_72)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1056 =
  fun _i#6940 -> ((poly_stub_71)@(L(unit)))[@inline] in
let nth_bootstrap_account#1057 =
  fun _i#6942 -> ((poly_stub_70)@(L(unit)))[@inline] in
let get_bootstrap_account#1058 =
  fun _n#6944 -> ((poly_stub_69)@(L(unit)))[@inline] in
let last_originations#1060 =
  fun _u#6948 -> ((poly_stub_68)@(L(unit)))[@inline] in
let new_account#1062 = fun _u#6952 -> ((poly_stub_67)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1064 =
  fun _n#6956 -> ((poly_stub_66)@(L(unit)))[@inline] in
let register_delegate#1066 =
  fun _kh#6960 -> ((poly_stub_65)@(L(unit)))[@inline] in
let register_constant#1067 =
  fun _m#6962 -> ((poly_stub_64)@(L(unit)))[@inline] in
let constant_to_michelson_program#1069 =
  fun _s#6966 -> ((poly_stub_63)@(L(unit)))[@inline] in
let restore_context#1070 =
  fun _u#6968 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#1071 = fun _u#6970 -> ((poly_stub_61)@(L(unit)))[@inline] in
let drop_context#1072 = fun _u#6972 -> ((poly_stub_60)@(L(unit)))[@inline] in
let set_baker_policy#1075 =
  fun _bp#6978 -> ((poly_stub_59)@(L(unit)))[@inline] in
let set_baker#1076 = fun _a#6980 -> ((poly_stub_58)@(L(unit)))[@inline] in
let size#1077 = fun _c#6982 -> ((poly_stub_57)@(L(unit)))[@inline] in
let read_contract_from_file#1079 =
  fun _fn#6986 -> ((poly_stub_56)@(L(unit)))[@inline] in
let chr#1080 = fun _n#6988 -> ((poly_stub_55)@(L(unit)))[@inline] in
let nl#1081 = L("NEWLINE")[@inline] in
let println#1082 = fun _v#6991 -> ((poly_stub_54)@(L(unit)))[@inline] in
let transfer#1083 =
  fun _a#6993 -> (fun _s#6994 -> (fun _t#6995 -> ((poly_stub_53)@(L(unit)))))[@inline] in
let transfer_exn#1084 =
  fun _a#6997 -> (fun _s#6998 -> (fun _t#6999 -> ((poly_stub_52)@(L(unit)))))[@inline] in
let reset_state#1086 =
  fun _n#7003 -> (fun _l#7004 -> ((poly_stub_51)@(L(unit))))[@inline] in
let reset_state_at#1087 =
  fun _t#7006 -> (fun _n#7007 -> (fun _l#7008 -> ((poly_stub_50)@(L(unit)))))[@inline] in
let save_mutation#1090 =
  fun _s#7017 -> (fun _m#7018 -> ((poly_stub_49)@(L(unit))))[@inline] in
let sign#1093 =
  fun _sk#7026 -> (fun _d#7027 -> ((poly_stub_48)@(L(unit))))[@inline] in
let add_account#1094 =
  fun _s#7029 -> (fun _k#7030 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#1095 =
  fun _p#7032 -> (fun _o#7033 -> ((poly_stub_46)@(L(unit))))[@inline] in
let create_chest#1097 =
  fun _b#7038 -> (fun _n#7039 -> ((poly_stub_45)@(L(unit))))[@inline] in
let create_chest_key#1098 =
  fun _c#7041 -> (fun _n#7042 -> ((poly_stub_44)@(L(unit))))[@inline] in
let michelson_equal#1101 =
  fun _m1#7052 -> (fun _m2#7053 -> ((poly_stub_43)@(L(unit))))[@inline] in
let originate_contract#1103 =
  fun _c#7058 -> (fun _s#7059 -> (fun _t#7060 -> ((poly_stub_42)@(L(unit)))))[@inline] in
let compile_contract_from_file#1105 =
  fun _fn#7066 ->
  (fun _e#7067 -> (fun _v#7068 -> ((poly_stub_41)@(L(unit)))))[@inline] in
let originate_from_file#1106 =
  fun _fn#7070 ->
  (fun _e#7071 ->
   (fun _v#7072 ->
    (fun _s#7073 -> (fun _t#7074 -> ((poly_stub_40)@(L(unit)))))))[@inline] in
let toto#1107 = L(10) in
let foo#1108 = L("bar") in
let get_balance#1109 =
  fun _u#7078 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1110 =
  fun _u#7080 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1111 = fun _u#7082 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1112 =
  fun _u#7084 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#1113 =
  fun _u#7086 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1114 =
  fun _u#7088 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1115 = fun _u#7090 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1116 =
  fun _u#7092 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1117 =
  fun _u#7094 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#1118 =
  fun _u#7096 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#1129 =
  fun kh#7108 -> (({ VOTING_POWER })@(kh#7108))[@inline] in
let implicit_account#1131 =
  fun kh#7112 -> (IMPLICIT_ACCOUNT(kh#7112))[@inline] in
let pairing_check#1135 =
  fun l#7120 -> (({ PAIRING_CHECK })@(l#7120))[@inline] in
let set_delegate#1137 = fun o#7124 -> (SET_DELEGATE(o#7124))[@inline] in
let open_chest#1143 =
  fun ck#7140 ->
  (fun c#7141 -> (fun n#7142 -> (OPEN_CHEST(ck#7140 , c#7141 , n#7142))))[@inline] in
let xor#1146 =
  fun l#7151 -> (fun r#7152 -> (XOR(l#7151 , r#7152)))[@inline] in
let shift_left#1147 =
  fun l#7154 -> (fun r#7155 -> (LSL(l#7154 , r#7155)))[@inline] in
let shift_right#1148 =
  fun l#7157 -> (fun r#7158 -> (LSR(l#7157 , r#7158)))[@inline] in
let length#1189 = fun b#7288 -> (({ SIZE })@(b#7288))[@inline] in
let concat#1190 =
  fun b1#7290 ->
  (fun b2#7291 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7290 , b2#7291))))[@inline] in
let sub#1191 =
  fun s#7293 ->
  (fun l#7294 ->
   (fun b#7295 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7293 ,
                                                                   l#7294) ,
                                                              b#7295)))))[@inline] in
let length#1196 = fun b#7306 -> (({ SIZE })@(b#7306))[@inline] in
let concat#1197 =
  fun b1#7308 ->
  (fun b2#7309 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7308 , b2#7309))))[@inline] in
let sub#1198 =
  fun s#7311 ->
  (fun l#7312 ->
   (fun b#7313 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7311 ,
                                                                   l#7312) ,
                                                              b#7313)))))[@inline] in
let blake2b#1199 = fun b#7315 -> (({ BLAKE2B })@(b#7315))[@inline] in
let sha256#1200 = fun b#7317 -> (({ SHA256 })@(b#7317))[@inline] in
let sha512#1201 = fun b#7319 -> (({ SHA512 })@(b#7319))[@inline] in
let sha3#1202 = fun b#7321 -> (({ SHA3 })@(b#7321))[@inline] in
let keccak#1203 = fun b#7323 -> (({ KECCAK })@(b#7323))[@inline] in
let hash_key#1204 = fun k#7325 -> (({ HASH_KEY })@(k#7325))[@inline] in
let check#1205 =
  fun k#7327 ->
  (fun s#7328 ->
   (fun b#7329 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7327 , s#7328) ,
                                                   b#7329)))))[@inline] in
let assert =
  fun b#7331 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7331))[@inline] in
let abs = fun i#7337 -> (({ ABS })@(i#7337))[@inline] in
let is_nat = fun i#7339 -> (({ ISNAT })@(i#7339))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b#7347 ->
  (fun s#7348 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7347 , s#7348))))[@inline] in
let poly_stub_39 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_38 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_37 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_36 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_35 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_34 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_33 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_32 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_31 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_30 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_29 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_28 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_27 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_26 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_25 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_24 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_23 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_22 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_21 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_20 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_19 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_18 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_17 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_16 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_15 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_14 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_13 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_12 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_11 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_10 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_9 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_8 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_7 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_6 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_5 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_4 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_3 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_2 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let poly_stub_1 = fun x#7359 -> (({ FAILWITH })@(x#7359))[@inline] in
let get_total_voting_power#1210 =
  fun _u#7368 -> ((poly_stub_39)@(L(unit)))[@inline] in
let set_source#1213 = fun _a#7374 -> ((poly_stub_38)@(L(unit)))[@inline] in
let get_storage_of_address#1214 =
  fun _a#7376 -> ((poly_stub_37)@(L(unit)))[@inline] in
let get_balance#1215 = fun _a#7378 -> ((poly_stub_36)@(L(unit)))[@inline] in
let print#1216 = fun _v#7380 -> ((poly_stub_35)@(L(unit)))[@inline] in
let eprint#1217 = fun _v#7382 -> ((poly_stub_34)@(L(unit)))[@inline] in
let get_voting_power#1218 =
  fun _kh#7384 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1219 =
  fun _i#7386 -> ((poly_stub_32)@(L(unit)))[@inline] in
let nth_bootstrap_account#1220 =
  fun _i#7388 -> ((poly_stub_31)@(L(unit)))[@inline] in
let get_bootstrap_account#1221 =
  fun _n#7390 -> ((poly_stub_30)@(L(unit)))[@inline] in
let last_originations#1223 =
  fun _u#7394 -> ((poly_stub_29)@(L(unit)))[@inline] in
let new_account#1225 = fun _u#7398 -> ((poly_stub_28)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1227 =
  fun _n#7402 -> ((poly_stub_27)@(L(unit)))[@inline] in
let register_delegate#1229 =
  fun _kh#7406 -> ((poly_stub_26)@(L(unit)))[@inline] in
let register_constant#1230 =
  fun _m#7408 -> ((poly_stub_25)@(L(unit)))[@inline] in
let constant_to_michelson_program#1232 =
  fun _s#7412 -> ((poly_stub_24)@(L(unit)))[@inline] in
let restore_context#1233 =
  fun _u#7414 -> ((poly_stub_23)@(L(unit)))[@inline] in
let save_context#1234 = fun _u#7416 -> ((poly_stub_22)@(L(unit)))[@inline] in
let drop_context#1235 = fun _u#7418 -> ((poly_stub_21)@(L(unit)))[@inline] in
let set_baker_policy#1238 =
  fun _bp#7424 -> ((poly_stub_20)@(L(unit)))[@inline] in
let set_baker#1239 = fun _a#7426 -> ((poly_stub_19)@(L(unit)))[@inline] in
let size#1240 = fun _c#7428 -> ((poly_stub_18)@(L(unit)))[@inline] in
let read_contract_from_file#1242 =
  fun _fn#7432 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#1243 = fun _n#7434 -> ((poly_stub_16)@(L(unit)))[@inline] in
let nl#1244 = L("NEWLINE")[@inline] in
let println#1245 = fun _v#7437 -> ((poly_stub_15)@(L(unit)))[@inline] in
let transfer#1246 =
  fun _a#7439 -> (fun _s#7440 -> (fun _t#7441 -> ((poly_stub_14)@(L(unit)))))[@inline] in
let transfer_exn#1247 =
  fun _a#7443 -> (fun _s#7444 -> (fun _t#7445 -> ((poly_stub_13)@(L(unit)))))[@inline] in
let reset_state#1249 =
  fun _n#7449 -> (fun _l#7450 -> ((poly_stub_12)@(L(unit))))[@inline] in
let reset_state_at#1250 =
  fun _t#7452 -> (fun _n#7453 -> (fun _l#7454 -> ((poly_stub_11)@(L(unit)))))[@inline] in
let save_mutation#1253 =
  fun _s#7463 -> (fun _m#7464 -> ((poly_stub_10)@(L(unit))))[@inline] in
let sign#1256 =
  fun _sk#7472 -> (fun _d#7473 -> ((poly_stub_9)@(L(unit))))[@inline] in
let add_account#1257 =
  fun _s#7475 -> (fun _k#7476 -> ((poly_stub_8)@(L(unit))))[@inline] in
let baker_account#1258 =
  fun _p#7478 -> (fun _o#7479 -> ((poly_stub_7)@(L(unit))))[@inline] in
let create_chest#1260 =
  fun _b#7484 -> (fun _n#7485 -> ((poly_stub_6)@(L(unit))))[@inline] in
let create_chest_key#1261 =
  fun _c#7487 -> (fun _n#7488 -> ((poly_stub_5)@(L(unit))))[@inline] in
let michelson_equal#1264 =
  fun _m1#7498 -> (fun _m2#7499 -> ((poly_stub_4)@(L(unit))))[@inline] in
let originate_contract#1266 =
  fun _c#7504 -> (fun _s#7505 -> (fun _t#7506 -> ((poly_stub_3)@(L(unit)))))[@inline] in
let compile_contract_from_file#1268 =
  fun _fn#7512 -> (fun _e#7513 -> (fun _v#7514 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let originate_from_file#1269 =
  fun _fn#7516 ->
  (fun _e#7517 ->
   (fun _v#7518 ->
    (fun _s#7519 -> (fun _t#7520 -> ((poly_stub_1)@(L(unit)))))))[@inline] in
let toto = ADD(toto#1107 , toto#224) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#7524 ->
  (let (gen#7530, gen#7531) = gen#7524 in
   let p#7525 = gen#7530 in
   let s#7526 = gen#7531 in
   let s#7527 = ADD(ADD(p#7525 , s#7526) , toto) in
   PAIR(LIST_EMPTY() , s#7527)) in
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
