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
  [%expect {xxx|
    module C =
      Mangled_module_____________________test__contracts__build__C____mligo.
    module E =
      Mangled_module_____________________test__contracts__build__E____mligo.
    const toto : int = ADD(E.toto , C.B.A.toto)
    const fb : record[tata -> int , tete -> int , titi -> int , toto -> int] =
      record[tata -> 2 , tete -> 3 , titi -> 1 , toto -> toto]
    const main : ( int * int ) -> ( list (operation) * int ) =
      lambda (gen#4( int * int ))( list (operation) * int ) return  match
                                                                     gen#4 with
                                                                     | ( p : int , s : int ) ->
                                                                     let s : int =
                                                                       ADD
                                                                       (ADD
                                                                        (p ,
                                                                        s) ,
                                                                        toto) in
                                                                     ( LIST_EMPTY
                                                                       () ,
                                                                       s ) |xxx}]

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
  [%expect {xxx|
    module Errors =
      Mangled_module_____________________test__contracts__build__instance____________common__errors____mligo.
    module Storage =
      Mangled_module_____________________test__contracts__build__instance____________common__storage____mligo.
    const main : ( unit * string ) -> ( list (operation) * string ) =
      lambda (gen#2( unit * string ))( list (operation) * string ) return
       match gen#2 with
        | ( _#4 : unit , _#3 : string ) ->
        ( LIST_EMPTY() , CONCAT(Errors.undefined_token , Storage.s) ) |xxx}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let get_balance#116 =
  fun _u#1622 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#117 =
  fun _u#1624 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#118 = fun _u#1626 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#119 =
  fun _u#1628 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#120 =
  fun _u#1630 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#121 = fun _u#1632 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#122 = fun _u#1634 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#123 =
  fun _u#1636 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#124 =
  fun _u#1638 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#125 =
  fun _u#1640 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#136 =
  fun kh#1652 -> (({ VOTING_POWER })@(kh#1652))[@inline] in
let implicit_account#138 =
  fun kh#1656 -> (IMPLICIT_ACCOUNT(kh#1656))[@inline] in
let pairing_check#142 =
  fun l#1664 -> (({ PAIRING_CHECK })@(l#1664))[@inline] in
let set_delegate#144 = fun o#1668 -> (SET_DELEGATE(o#1668))[@inline] in
let open_chest#152 =
  fun ck#1689 ->
  (fun c#1690 -> (fun n#1691 -> (OPEN_CHEST(ck#1689 , c#1690 , n#1691))))[@inline] in
let xor#161 =
  fun l#1725 -> (fun r#1726 -> (XOR(l#1725 , r#1726)))[@inline] in
let or#162 = fun l#1728 -> (fun r#1729 -> (OR(l#1728 , r#1729)))[@inline] in
let shift_left#163 =
  fun l#1731 -> (fun r#1732 -> (LSL(l#1731 , r#1732)))[@inline] in
let shift_right#164 =
  fun l#1734 -> (fun r#1735 -> (LSR(l#1734 , r#1735)))[@inline] in
let length#209 = fun b#1880 -> (({ SIZE })@(b#1880))[@inline] in
let concat#210 =
  fun b1#1882 ->
  (fun b2#1883 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1882 , b2#1883))))[@inline] in
let sub#211 =
  fun s#1885 ->
  (fun l#1886 ->
   (fun b#1887 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1885 ,
                                                                   l#1886) ,
                                                              b#1887)))))[@inline] in
let length#217 = fun b#1902 -> (({ SIZE })@(b#1902))[@inline] in
let concat#218 =
  fun b1#1904 ->
  (fun b2#1905 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1904 , b2#1905))))[@inline] in
let sub#219 =
  fun s#1907 ->
  (fun l#1908 ->
   (fun b#1909 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1907 ,
                                                                   l#1908) ,
                                                              b#1909)))))[@inline] in
let blake2b#220 = fun b#1911 -> (({ BLAKE2B })@(b#1911))[@inline] in
let sha256#221 = fun b#1913 -> (({ SHA256 })@(b#1913))[@inline] in
let sha512#222 = fun b#1915 -> (({ SHA512 })@(b#1915))[@inline] in
let sha3#223 = fun b#1917 -> (({ SHA3 })@(b#1917))[@inline] in
let keccak#224 = fun b#1919 -> (({ KECCAK })@(b#1919))[@inline] in
let hash_key#225 = fun k#1921 -> (({ HASH_KEY })@(k#1921))[@inline] in
let check#226 =
  fun k#1923 ->
  (fun s#1924 ->
   (fun b#1925 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#1923 , s#1924) ,
                                                   b#1925)))))[@inline] in
let assert#227 =
  fun b#1927 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#1927))[@inline] in
let abs#230 = fun i#1933 -> (({ ABS })@(i#1933))[@inline] in
let is_nat#231 = fun i#1935 -> (({ ISNAT })@(i#1935))[@inline] in
let true#232 = TRUE()[@inline] in
let false#233 = FALSE()[@inline] in
let unit#234 = UNIT()[@inline] in
let assert_with_error#238 =
  fun b#1945 ->
  (fun s#1946 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#1945 , s#1946))))[@inline] in
let poly_stub_78 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_77 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_76 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_75 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_74 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_73 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_72 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_71 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_70 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_69 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_68 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_67 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_66 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_65 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_64 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_63 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_62 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_61 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_60 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_59 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_58 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_57 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_56 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_55 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_54 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_53 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_52 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_51 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_50 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_49 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_48 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_47 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_46 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_45 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_44 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_43 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_42 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_41 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let poly_stub_40 = fun x#1957 -> (({ FAILWITH })@(x#1957))[@inline] in
let get_total_voting_power#246 =
  fun _u#1966 -> ((poly_stub_78)@(L(unit)))[@inline] in
let set_source#249 = fun _a#1972 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#250 =
  fun _a#1974 -> ((poly_stub_76)@(L(unit)))[@inline] in
let get_balance#251 = fun _a#1976 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#252 = fun _v#1978 -> ((poly_stub_74)@(L(unit)))[@inline] in
let eprint#253 = fun _v#1980 -> ((poly_stub_73)@(L(unit)))[@inline] in
let get_voting_power#254 =
  fun _kh#1982 -> ((poly_stub_72)@(L(unit)))[@inline] in
let nth_bootstrap_contract#255 =
  fun _i#1984 -> ((poly_stub_71)@(L(unit)))[@inline] in
let nth_bootstrap_account#256 =
  fun _i#1986 -> ((poly_stub_70)@(L(unit)))[@inline] in
let get_bootstrap_account#257 =
  fun _n#1988 -> ((poly_stub_69)@(L(unit)))[@inline] in
let last_originations#259 =
  fun _u#1992 -> ((poly_stub_68)@(L(unit)))[@inline] in
let new_account#261 = fun _u#1996 -> ((poly_stub_67)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#263 =
  fun _n#2000 -> ((poly_stub_66)@(L(unit)))[@inline] in
let register_delegate#265 =
  fun _kh#2004 -> ((poly_stub_65)@(L(unit)))[@inline] in
let register_constant#266 =
  fun _m#2006 -> ((poly_stub_64)@(L(unit)))[@inline] in
let constant_to_michelson_program#268 =
  fun _s#2010 -> ((poly_stub_63)@(L(unit)))[@inline] in
let restore_context#269 =
  fun _u#2012 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#270 = fun _u#2014 -> ((poly_stub_61)@(L(unit)))[@inline] in
let drop_context#271 = fun _u#2016 -> ((poly_stub_60)@(L(unit)))[@inline] in
let set_baker_policy#274 =
  fun _bp#2022 -> ((poly_stub_59)@(L(unit)))[@inline] in
let set_baker#275 = fun _a#2024 -> ((poly_stub_58)@(L(unit)))[@inline] in
let size#276 = fun _c#2026 -> ((poly_stub_57)@(L(unit)))[@inline] in
let read_contract_from_file#278 =
  fun _fn#2030 -> ((poly_stub_56)@(L(unit)))[@inline] in
let chr#279 = fun _n#2032 -> ((poly_stub_55)@(L(unit)))[@inline] in
let nl#280 = L("NEWLINE")[@inline] in
let println#281 = fun _v#2035 -> ((poly_stub_54)@(L(unit)))[@inline] in
let transfer#282 =
  fun _a#2037 -> (fun _s#2038 -> (fun _t#2039 -> ((poly_stub_53)@(L(unit)))))[@inline] in
let transfer_exn#283 =
  fun _a#2041 -> (fun _s#2042 -> (fun _t#2043 -> ((poly_stub_52)@(L(unit)))))[@inline] in
let reset_state#285 =
  fun _n#2047 -> (fun _l#2048 -> ((poly_stub_51)@(L(unit))))[@inline] in
let reset_state_at#286 =
  fun _t#2050 -> (fun _n#2051 -> (fun _l#2052 -> ((poly_stub_50)@(L(unit)))))[@inline] in
let save_mutation#289 =
  fun _s#2061 -> (fun _m#2062 -> ((poly_stub_49)@(L(unit))))[@inline] in
let sign#292 =
  fun _sk#2070 -> (fun _d#2071 -> ((poly_stub_48)@(L(unit))))[@inline] in
let add_account#293 =
  fun _s#2073 -> (fun _k#2074 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#294 =
  fun _p#2076 -> (fun _o#2077 -> ((poly_stub_46)@(L(unit))))[@inline] in
let create_chest#296 =
  fun _b#2082 -> (fun _n#2083 -> ((poly_stub_45)@(L(unit))))[@inline] in
let create_chest_key#297 =
  fun _c#2085 -> (fun _n#2086 -> ((poly_stub_44)@(L(unit))))[@inline] in
let michelson_equal#300 =
  fun _m1#2096 -> (fun _m2#2097 -> ((poly_stub_43)@(L(unit))))[@inline] in
let originate_contract#302 =
  fun _c#2102 -> (fun _s#2103 -> (fun _t#2104 -> ((poly_stub_42)@(L(unit)))))[@inline] in
let compile_contract_from_file#304 =
  fun _fn#2110 ->
  (fun _e#2111 -> (fun _v#2112 -> ((poly_stub_41)@(L(unit)))))[@inline] in
let originate_from_file#305 =
  fun _fn#2114 ->
  (fun _e#2115 ->
   (fun _v#2116 ->
    (fun _s#2117 -> (fun _t#2118 -> ((poly_stub_40)@(L(unit)))))))[@inline] in
let get_balance#306 =
  fun _u#2120 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#307 =
  fun _u#2122 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#308 = fun _u#2124 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#309 =
  fun _u#2126 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#310 =
  fun _u#2128 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#311 = fun _u#2130 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#312 = fun _u#2132 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#313 =
  fun _u#2134 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#314 =
  fun _u#2136 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#315 =
  fun _u#2138 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#326 =
  fun kh#2150 -> (({ VOTING_POWER })@(kh#2150))[@inline] in
let implicit_account#328 =
  fun kh#2154 -> (IMPLICIT_ACCOUNT(kh#2154))[@inline] in
let pairing_check#332 =
  fun l#2162 -> (({ PAIRING_CHECK })@(l#2162))[@inline] in
let set_delegate#334 = fun o#2166 -> (SET_DELEGATE(o#2166))[@inline] in
let open_chest#342 =
  fun gen#2190 ->
  (let (gen#2736, gen#2737) = gen#2190 in
   let (gen#2738, gen#2739) = gen#2736 in
   let ck#2191 = gen#2738 in
   let c#2192 = gen#2739 in
   let n#2193 = gen#2737 in OPEN_CHEST(ck#2191 , c#2192 , n#2193))[@inline] in
let xor#351 =
  fun gen#2236 ->
  (let (gen#2740, gen#2741) = gen#2236 in
   let l#2237 = gen#2740 in let r#2238 = gen#2741 in XOR(l#2237 , r#2238))[@inline] in
let or#352 =
  fun gen#2240 ->
  (let (gen#2742, gen#2743) = gen#2240 in
   let l#2241 = gen#2742 in let r#2242 = gen#2743 in OR(l#2241 , r#2242))[@inline] in
let shift_left#353 =
  fun gen#2244 ->
  (let (gen#2744, gen#2745) = gen#2244 in
   let l#2245 = gen#2744 in let r#2246 = gen#2745 in LSL(l#2245 , r#2246))[@inline] in
let shift_right#354 =
  fun gen#2248 ->
  (let (gen#2746, gen#2747) = gen#2248 in
   let l#2249 = gen#2746 in let r#2250 = gen#2747 in LSR(l#2249 , r#2250))[@inline] in
let length#399 = fun b#2426 -> (({ SIZE })@(b#2426))[@inline] in
let concat#400 =
  fun gen#2428 ->
  (let (gen#2748, gen#2749) = gen#2428 in
   let b1#2429 = gen#2748 in
   let b2#2430 = gen#2749 in ({ UNPAIR ; CONCAT })@(PAIR(b1#2429 , b2#2430)))[@inline] in
let sub#401 =
  fun gen#2432 ->
  (let (gen#2750, gen#2751) = gen#2432 in
   let (gen#2752, gen#2753) = gen#2750 in
   let s#2433 = gen#2752 in
   let l#2434 = gen#2753 in
   let b#2435 = gen#2751 in
   ({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2433 ,
                                                                 l#2434) ,
                                                            b#2435)))[@inline] in
let length#407 = fun b#2452 -> (({ SIZE })@(b#2452))[@inline] in
let concat#408 =
  fun gen#2454 ->
  (let (gen#2754, gen#2755) = gen#2454 in
   let b1#2455 = gen#2754 in
   let b2#2456 = gen#2755 in ({ UNPAIR ; CONCAT })@(PAIR(b1#2455 , b2#2456)))[@inline] in
let sub#409 =
  fun gen#2458 ->
  (let (gen#2756, gen#2757) = gen#2458 in
   let (gen#2758, gen#2759) = gen#2756 in
   let s#2459 = gen#2758 in
   let l#2460 = gen#2759 in
   let b#2461 = gen#2757 in
   ({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2459 ,
                                                                 l#2460) ,
                                                            b#2461)))[@inline] in
let blake2b#410 = fun b#2463 -> (({ BLAKE2B })@(b#2463))[@inline] in
let sha256#411 = fun b#2465 -> (({ SHA256 })@(b#2465))[@inline] in
let sha512#412 = fun b#2467 -> (({ SHA512 })@(b#2467))[@inline] in
let sha3#413 = fun b#2469 -> (({ SHA3 })@(b#2469))[@inline] in
let keccak#414 = fun b#2471 -> (({ KECCAK })@(b#2471))[@inline] in
let hash_key#415 = fun k#2473 -> (({ HASH_KEY })@(k#2473))[@inline] in
let check#416 =
  fun gen#2475 ->
  (let (gen#2760, gen#2761) = gen#2475 in
   let (gen#2762, gen#2763) = gen#2760 in
   let k#2476 = gen#2762 in
   let s#2477 = gen#2763 in
   let b#2478 = gen#2761 in
   ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#2476 , s#2477) ,
                                                 b#2478)))[@inline] in
let assert#417 =
  fun b#2480 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#2480))[@inline] in
let abs#420 = fun i#2486 -> (({ ABS })@(i#2486))[@inline] in
let is_nat#421 = fun i#2488 -> (({ ISNAT })@(i#2488))[@inline] in
let true#422 = TRUE()[@inline] in
let false#423 = FALSE()[@inline] in
let unit#424 = UNIT()[@inline] in
let assert_with_error#428 =
  fun gen#2498 ->
  (let (gen#2764, gen#2765) = gen#2498 in
   let b#2499 = gen#2764 in
   let s#2500 = gen#2765 in
   ({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#2499 , s#2500)))[@inline] in
let poly_stub_39 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_38 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_37 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_36 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_35 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_34 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_33 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_32 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_31 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_30 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_29 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_28 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_27 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_26 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_25 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_24 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_23 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_22 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_21 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_20 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_19 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_18 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_17 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_16 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_15 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_14 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_13 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_12 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_11 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_10 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_9 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_8 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_7 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_6 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_5 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_4 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_3 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_2 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let poly_stub_1 = fun x#2514 -> (({ FAILWITH })@(x#2514))[@inline] in
let get_total_voting_power#436 =
  fun _u#2524 -> ((poly_stub_39)@(L(unit)))[@inline] in
let set_source#439 = fun _a#2530 -> ((poly_stub_38)@(L(unit)))[@inline] in
let get_storage_of_address#440 =
  fun _a#2532 -> ((poly_stub_37)@(L(unit)))[@inline] in
let get_balance#441 = fun _a#2534 -> ((poly_stub_36)@(L(unit)))[@inline] in
let print#442 = fun _v#2536 -> ((poly_stub_35)@(L(unit)))[@inline] in
let eprint#443 = fun _v#2538 -> ((poly_stub_34)@(L(unit)))[@inline] in
let get_voting_power#444 =
  fun _kh#2540 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_contract#445 =
  fun _i#2542 -> ((poly_stub_32)@(L(unit)))[@inline] in
let nth_bootstrap_account#446 =
  fun _i#2544 -> ((poly_stub_31)@(L(unit)))[@inline] in
let get_bootstrap_account#447 =
  fun _n#2546 -> ((poly_stub_30)@(L(unit)))[@inline] in
let last_originations#449 =
  fun _u#2550 -> ((poly_stub_29)@(L(unit)))[@inline] in
let new_account#451 = fun _u#2554 -> ((poly_stub_28)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#453 =
  fun _n#2558 -> ((poly_stub_27)@(L(unit)))[@inline] in
let register_delegate#455 =
  fun _kh#2562 -> ((poly_stub_26)@(L(unit)))[@inline] in
let register_constant#456 =
  fun _m#2564 -> ((poly_stub_25)@(L(unit)))[@inline] in
let constant_to_michelson_program#458 =
  fun _s#2568 -> ((poly_stub_24)@(L(unit)))[@inline] in
let restore_context#459 =
  fun _u#2570 -> ((poly_stub_23)@(L(unit)))[@inline] in
let save_context#460 = fun _u#2572 -> ((poly_stub_22)@(L(unit)))[@inline] in
let drop_context#461 = fun _u#2574 -> ((poly_stub_21)@(L(unit)))[@inline] in
let set_baker_policy#464 =
  fun _bp#2580 -> ((poly_stub_20)@(L(unit)))[@inline] in
let set_baker#465 = fun _a#2582 -> ((poly_stub_19)@(L(unit)))[@inline] in
let size#466 = fun _c#2584 -> ((poly_stub_18)@(L(unit)))[@inline] in
let read_contract_from_file#468 =
  fun _fn#2588 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#469 = fun _n#2590 -> ((poly_stub_16)@(L(unit)))[@inline] in
let nl#470 = L("NEWLINE")[@inline] in
let println#471 = fun _v#2593 -> ((poly_stub_15)@(L(unit)))[@inline] in
let transfer#472 =
  fun gen#2595 ->
  (let (gen#2766, gen#2767) = gen#2595 in
   let (gen#2768, gen#2769) = gen#2766 in
   let _a#2596 = gen#2768 in
   let _s#2597 = gen#2769 in
   let _t#2598 = gen#2767 in (poly_stub_14)@(L(unit)))[@inline] in
let transfer_exn#473 =
  fun gen#2600 ->
  (let (gen#2770, gen#2771) = gen#2600 in
   let (gen#2772, gen#2773) = gen#2770 in
   let _a#2601 = gen#2772 in
   let _s#2602 = gen#2773 in
   let _t#2603 = gen#2771 in (poly_stub_13)@(L(unit)))[@inline] in
let reset_state#475 =
  fun gen#2607 ->
  (let (gen#2774, gen#2775) = gen#2607 in
   let _n#2608 = gen#2774 in
   let _l#2609 = gen#2775 in (poly_stub_12)@(L(unit)))[@inline] in
let reset_state_at#476 =
  fun gen#2611 ->
  (let (gen#2776, gen#2777) = gen#2611 in
   let (gen#2778, gen#2779) = gen#2776 in
   let _t#2612 = gen#2778 in
   let _n#2613 = gen#2779 in
   let _l#2614 = gen#2777 in (poly_stub_11)@(L(unit)))[@inline] in
let save_mutation#479 =
  fun gen#2625 ->
  (let (gen#2780, gen#2781) = gen#2625 in
   let _s#2626 = gen#2780 in
   let _m#2627 = gen#2781 in (poly_stub_10)@(L(unit)))[@inline] in
let sign#482 =
  fun gen#2637 ->
  (let (gen#2782, gen#2783) = gen#2637 in
   let _sk#2638 = gen#2782 in
   let _d#2639 = gen#2783 in (poly_stub_9)@(L(unit)))[@inline] in
let add_account#483 =
  fun gen#2641 ->
  (let (gen#2784, gen#2785) = gen#2641 in
   let _s#2642 = gen#2784 in
   let _k#2643 = gen#2785 in (poly_stub_8)@(L(unit)))[@inline] in
let baker_account#484 =
  fun gen#2645 ->
  (let (gen#2786, gen#2787) = gen#2645 in
   let _p#2646 = gen#2786 in
   let _o#2647 = gen#2787 in (poly_stub_7)@(L(unit)))[@inline] in
let create_chest#486 =
  fun gen#2653 ->
  (let (gen#2788, gen#2789) = gen#2653 in
   let _b#2654 = gen#2788 in
   let _n#2655 = gen#2789 in (poly_stub_6)@(L(unit)))[@inline] in
let create_chest_key#487 =
  fun gen#2657 ->
  (let (gen#2790, gen#2791) = gen#2657 in
   let _c#2658 = gen#2790 in
   let _n#2659 = gen#2791 in (poly_stub_5)@(L(unit)))[@inline] in
let michelson_equal#490 =
  fun gen#2671 ->
  (let (gen#2792, gen#2793) = gen#2671 in
   let _m1#2672 = gen#2792 in
   let _m2#2673 = gen#2793 in (poly_stub_4)@(L(unit)))[@inline] in
let originate_contract#492 =
  fun gen#2679 ->
  (let (gen#2794, gen#2795) = gen#2679 in
   let (gen#2796, gen#2797) = gen#2794 in
   let _c#2680 = gen#2796 in
   let _s#2681 = gen#2797 in
   let _t#2682 = gen#2795 in (poly_stub_3)@(L(unit)))[@inline] in
let compile_contract_from_file#494 =
  fun gen#2689 ->
  (let (gen#2798, gen#2799) = gen#2689 in
   let (gen#2800, gen#2801) = gen#2798 in
   let _fn#2690 = gen#2800 in
   let _e#2691 = gen#2801 in
   let _v#2692 = gen#2799 in (poly_stub_2)@(L(unit)))[@inline] in
let originate_from_file#495 =
  fun gen#2694 ->
  (let (gen#2802, gen#2803) = gen#2694 in
   let (gen#2804, gen#2805) = gen#2802 in
   let (gen#2808, gen#2809) = gen#2804 in
   let _fn#2695 = gen#2808 in
   let _e#2696 = gen#2809 in
   let (gen#2806, gen#2807) = gen#2805 in
   let _v#2697 = gen#2806 in
   let _s#2698 = gen#2807 in
   let _t#2699 = gen#2803 in (poly_stub_1)@(L(unit)))[@inline] in
let assert = assert#227[@inline] in
let abs = abs#230[@inline] in
let is_nat = is_nat#231[@inline] in
let true = true#232[@inline] in
let false = false#233[@inline] in
let unit = unit#234[@inline] in
let assert_with_error = assert_with_error#238[@inline] in
let toto#496 = L(1) in
let toto#497 = L(32) in
let titi#498 = ADD(toto#496 , L(42)) in
let f#499 =
  fun gen#2719 ->
  (let (gen#2810, gen#2811) = gen#2719 in
   let gen#2720 = gen#2810 in
   let x#2721 = gen#2811 in
   let x#2722 = ADD(ADD(x#2721 , toto#496) , titi#498) in
   PAIR(LIST_EMPTY() , x#2722)) in
let toto#500 = L(44) in
let toto#501 = L(43) in
let tata#502 = ADD(toto#496 , titi#498) in
let foo#503 = (f#499)@(PAIR(L(unit) , L(3))) in
let toto#504 = L(10) in
let foo#505 = L("bar") in
let toto = ADD(toto#504 , toto#496) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#2732 ->
  (let (gen#2812, gen#2813) = gen#2732 in
   let p#2733 = gen#2812 in
   let s#2734 = gen#2813 in
   let s#2735 = ADD(ADD(p#2733 , s#2734) , toto) in
   PAIR(LIST_EMPTY() , s#2735)) in
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
