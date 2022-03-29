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
      lambda (gen#6) return  match gen#6 with
                              | ( p , s ) ->
                              let s = ADD(ADD(p , s) ,
                              toto) in ( LIST_EMPTY() , s ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let #../../test/contracts/build/A.mligo#Bytes#concat#121 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c)))) in
let #../../test/contracts/build/A.mligo#Bytes#sub#122 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input))))) in
let #../../test/contracts/build/A.mligo#toto#128 = L(1) in
let #../../test/contracts/build/B.mligo#Bytes#concat#129 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c)))) in
let #../../test/contracts/build/B.mligo#Bytes#sub#130 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input))))) in
let #../../test/contracts/build/B.mligo#toto#136 = L(32) in
let #../../test/contracts/build/B.mligo#titi#137 =
  ADD(#../../test/contracts/build/A.mligo#toto#128 , L(42)) in
let #../../test/contracts/build/B.mligo#f#138 =
  fun gen#22 ->
  (let (gen#180, gen#181) = gen#22 in
   let gen#23 = gen#180 in
   let x = gen#181 in
   let x =
     ADD(ADD(x , #../../test/contracts/build/A.mligo#toto#128) ,
         #../../test/contracts/build/B.mligo#titi#137) in
   PAIR(LIST_EMPTY() , x)) in
let #../../test/contracts/build/F.mligo#Bytes#concat#139 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c)))) in
let #../../test/contracts/build/F.mligo#Bytes#sub#140 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input))))) in
let #../../test/contracts/build/F.mligo#toto#146 = L(44) in
let #../../test/contracts/build/G.mligo#Bytes#concat#147 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c)))) in
let #../../test/contracts/build/G.mligo#Bytes#sub#148 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input))))) in
let #../../test/contracts/build/G.mligo#toto#154 = L(43) in
let #../../test/contracts/build/C.mligo#Bytes#concat#155 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c)))) in
let #../../test/contracts/build/C.mligo#Bytes#sub#156 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input))))) in
let #../../test/contracts/build/C.mligo#tata#162 =
  ADD(#../../test/contracts/build/A.mligo#toto#128 ,
      #../../test/contracts/build/B.mligo#titi#137) in
let #../../test/contracts/build/C.mligo#foo#163 =
  (#../../test/contracts/build/B.mligo#f#138)@(PAIR(L(unit) , L(3))) in
let #../../test/contracts/build/E.mligo#Bytes#concat#164 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c)))) in
let #../../test/contracts/build/E.mligo#Bytes#sub#165 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input))))) in
let #../../test/contracts/build/E.mligo#toto#171 = L(10) in
let #../../test/contracts/build/E.mligo#foo#172 = L("bar") in
let #Bytes#concat#173 =
  fun b -> (fun c -> (({ UNPAIR ; CONCAT })@(PAIR(b , c)))) in
let #Bytes#sub#174 =
  fun start ->
  (fun length ->
   (fun input ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(start ,
                                                                   length) ,
                                                              input))))) in
let toto =
  ADD(#../../test/contracts/build/E.mligo#toto#171 ,
      #../../test/contracts/build/A.mligo#toto#128) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#75 ->
  (let (gen#182, gen#183) = gen#75 in
   let p = gen#182 in
   let s = gen#183 in
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
