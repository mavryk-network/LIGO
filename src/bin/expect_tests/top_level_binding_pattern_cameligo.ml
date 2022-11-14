open Cli_expect
let contract file = test ("top_level_patterns/contracts/" ^ file)

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "cameligo/nested_record.mligo" ] ;
  [%expect{|
    { parameter unit ;
      storage (pair (pair nat int) string) ;
      code { DROP ;
             PUSH string "O" ;
             PUSH string "O" ;
             CONCAT ;
             PUSH string "L" ;
             CONCAT ;
             PUSH string "L" ;
             CONCAT ;
             PUSH string "E" ;
             CONCAT ;
             PUSH string "H" ;
             CONCAT ;
             PUSH int 6 ;
             PUSH int 5 ;
             PUSH int 4 ;
             PUSH int 3 ;
             PUSH int 2 ;
             PUSH int 1 ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             PUSH nat 6 ;
             PUSH nat 5 ;
             PUSH nat 4 ;
             PUSH nat 3 ;
             PUSH nat 2 ;
             PUSH nat 1 ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "cameligo/nested_tuple.mligo" ] ;
  [%expect{|
    { parameter unit ;
      storage (pair (pair nat int) string) ;
      code { DROP ;
             PUSH string "World" ;
             PUSH string "O" ;
             CONCAT ;
             PUSH string "L" ;
             CONCAT ;
             PUSH string "Hello" ;
             CONCAT ;
             PUSH string "E" ;
             CONCAT ;
             PUSH string "H" ;
             CONCAT ;
             PUSH int 6 ;
             PUSH int 5 ;
             PUSH int 4 ;
             PUSH int 3 ;
             PUSH int 2 ;
             PUSH int 1 ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             PUSH nat 6 ;
             PUSH nat 5 ;
             PUSH nat 4 ;
             PUSH nat 3 ;
             PUSH nat 2 ;
             PUSH nat 1 ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "cameligo/record_tuple.mligo" ] ;
  [%expect{|
    { parameter unit ;
      storage (pair (pair nat int) string) ;
      code { DROP ;
             PUSH string "Hello" ;
             PUSH string "E" ;
             CONCAT ;
             PUSH string "H" ;
             CONCAT ;
             PUSH string "H" ;
             CONCAT ;
             PUSH string "E" ;
             CONCAT ;
             PUSH string "Hello" ;
             CONCAT ;
             PUSH int 6 ;
             PUSH int 5 ;
             PUSH int 4 ;
             PUSH int 3 ;
             PUSH int 2 ;
             PUSH int 1 ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             PUSH nat 6 ;
             PUSH nat 5 ;
             PUSH nat 4 ;
             PUSH nat 3 ;
             PUSH nat 2 ;
             PUSH nat 1 ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "cameligo/record.mligo" ] ;
  [%expect{|
    { parameter unit ;
      storage (pair (pair nat int) string) ;
      code { DROP ;
             PUSH string "World" ;
             PUSH string "Hello" ;
             CONCAT ;
             PUSH int 2 ;
             PUSH int 1 ;
             ADD ;
             PUSH nat 2 ;
             PUSH nat 1 ;
             ADD ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "cameligo/ticket_record.mligo" ] ;
  [%expect{|
    { parameter unit ;
      storage (pair (pair (ticket int) (ticket string)) (ticket nat)) ;
      code { DROP ;
             PUSH nat 10 ;
             PUSH nat 1 ;
             TICKET ;
             PUSH nat 10 ;
             PUSH string "one" ;
             TICKET ;
             PUSH nat 10 ;
             PUSH int 1 ;
             TICKET ;
             PUSH nat 10 ;
             PUSH nat 3 ;
             TICKET ;
             PUSH nat 10 ;
             PUSH string "TWO" ;
             TICKET ;
             PUSH nat 10 ;
             PUSH int 2 ;
             TICKET ;
             DIG 3 ;
             PAIR ;
             JOIN_TICKETS ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             SWAP ;
             DIG 3 ;
             PAIR ;
             JOIN_TICKETS ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             DIG 2 ;
             DIG 3 ;
             PAIR ;
             JOIN_TICKETS ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             SWAP ;
             DIG 2 ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "cameligo/ticket_tuple.mligo" ] ;
  [%expect{|
    { parameter unit ;
      storage (pair (pair (ticket int) (ticket string)) (ticket nat)) ;
      code { DROP ;
             PUSH nat 10 ;
             PUSH nat 1 ;
             TICKET ;
             PUSH nat 10 ;
             PUSH string "one" ;
             TICKET ;
             PUSH nat 10 ;
             PUSH int 1 ;
             TICKET ;
             PUSH nat 10 ;
             PUSH nat 3 ;
             TICKET ;
             PUSH nat 10 ;
             PUSH string "TWO" ;
             TICKET ;
             PUSH nat 10 ;
             PUSH int 2 ;
             TICKET ;
             DIG 3 ;
             PAIR ;
             JOIN_TICKETS ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             SWAP ;
             DIG 3 ;
             PAIR ;
             JOIN_TICKETS ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             DIG 2 ;
             DIG 3 ;
             PAIR ;
             JOIN_TICKETS ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             SWAP ;
             DIG 2 ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "cameligo/tuple_record.mligo" ] ;
  [%expect{|
    { parameter unit ;
      storage (pair (pair nat int) string) ;
      code { DROP ;
             PUSH string "O" ;
             PUSH string "L" ;
             CONCAT ;
             PUSH string "L" ;
             CONCAT ;
             PUSH string "L" ;
             CONCAT ;
             PUSH string "E" ;
             CONCAT ;
             PUSH string "H" ;
             CONCAT ;
             PUSH int 6 ;
             PUSH int 5 ;
             PUSH int 4 ;
             PUSH int 3 ;
             PUSH int 2 ;
             PUSH int 1 ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             PUSH nat 6 ;
             PUSH nat 5 ;
             PUSH nat 4 ;
             PUSH nat 3 ;
             PUSH nat 2 ;
             PUSH nat 1 ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "cameligo/tuple.mligo" ] ;
  [%expect{|
    { parameter unit ;
      storage (pair (pair nat int) string) ;
      code { DROP ;
             PUSH string "World" ;
             PUSH string "Hello" ;
             CONCAT ;
             PUSH int 2 ;
             PUSH int 1 ;
             ADD ;
             PUSH nat 2 ;
             PUSH nat 1 ;
             ADD ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "cameligo/constr_tuple_destructuring.mligo" ] ;
  [%expect{|
    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH string "hey" ;
             SIZE ;
             PUSH int 2 ;
             PUSH int 1 ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "cameligo/constr_record_destructuring.mligo" ] ;
  [%expect{|
    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH string "hey" ;
             SIZE ;
             PUSH int 2 ;
             PUSH int 1 ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]

(* Testing *)

let test_ file = test ("top_level_patterns/interpreter/" ^ file)

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "cameligo/nested_record.mligo" ] ;
  [%expect{|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "cameligo/nested_tuple.mligo" ] ;
  [%expect{|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "cameligo/record_tuple.mligo" ] ;
  [%expect{|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "cameligo/tuple_record.mligo" ] ;
  [%expect{|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]
    
let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "cameligo/record.mligo" ] ;
  [%expect{|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "cameligo/tuple.mligo" ] ;
  [%expect{|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]
    
let%expect_test _ =
    run_ligo_good [ "run" ; "test" ; test_ "cameligo/constr_tuple_destructuring.mligo" ] ;
    [%expect{|
      "Once"
      Everything at the top-level was executed.
      - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "cameligo/constr_record_destructuring.mligo" ] ;
  [%expect{|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

(* Negative - linearity *)

let contract file = test ("top_level_patterns/negative/" ^ file)

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "cameligo/nested_record.mligo" ] ;
  [%expect{|
    File "../../test/contracts/top_level_patterns/negative/cameligo/nested_record.mligo", line 8, character 4 to line 11, character 5:
      7 |         }
      8 | let { a = { c = c1 ; d = d1 ; e = e1 }
      9 |     ; b = { c = c2 ; d = d2 ; e = e2 }
     10 |     ; c = { c = c3 ; d = c1 ; e = e3 }
     11 |     } = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "cameligo/nested_tuple.mligo" ] ;
  [%expect{|
    File "../../test/contracts/top_level_patterns/negative/cameligo/nested_tuple.mligo", line 2, characters 5-45:
      1 | let r = ((1n, 1, "H"), (2n, 2, "E"), (3n, 3, "Hello"))
      2 | let ((a1, a2, a3), (b1, a2, b3), (c1, c2, c3)) = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "cameligo/record.mligo" ] ;
  [%expect{|
    File "../../test/contracts/top_level_patterns/negative/cameligo/record.mligo", line 4, characters 4-21:
      3 | let r = { a = 1n ; b = 1 ; c = "Hello" }
      4 | let { a ; b = a ; c } = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "cameligo/tuple.mligo" ] ;
  [%expect{|
    File "../../test/contracts/top_level_patterns/negative/cameligo/tuple.mligo", line 2, characters 5-12:
      1 | let r = (1n, 1, "Hello")
      2 | let (a, a, c) = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "cameligo/record_tuple.mligo" ] ;
  [%expect{|
    File "../../test/contracts/top_level_patterns/negative/cameligo/record_tuple.mligo", line 10, character 4 to line 13, character 5:
      9 |         }
     10 | let { a = (a1, a2, a3)
     11 |     ; b = (b1, a2, b3)
     12 |     ; c = (c1, c2, c3)
     13 |     } = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "cameligo/tuple_record.mligo" ] ;
  [%expect{|
    File "../../test/contracts/top_level_patterns/negative/cameligo/tuple_record.mligo", line 7, character 6 to line 9, character 34:
      6 |         )
      7 | let ( { a = a1 ; b = b1 ; c = c1 }
      8 |     , { a = a2 ; b = b2 ; c = a2 }
      9 |     , { a = a3 ; b = b3 ; c = c3 }
     10 |     ) = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

(* Negative - much use *)

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "cameligo/ticket_record.mligo" ; "--werror" ; "--disable-michelson-typechecking" ] ;
  [%expect{|
    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_record.mligo", line 3, characters 6-7:
      2 |
      3 | let { b } = { b = Tezos.create_ticket "one" 10n }
      4 |
    :
    Warning: variable "b" cannot be used more than once.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_record.mligo", line 3, characters 6-7:
      2 |
      3 | let { b } = { b = Tezos.create_ticket "one" 10n }
      4 |
    :
    Warning: variable "b" cannot be used more than once.
    { parameter unit ;
      storage (ticket string) ;
      code { DROP ;
             PUSH nat 10 ;
             PUSH string "one" ;
             TICKET ;
             DUP ;
             PAIR ;
             JOIN_TICKETS ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "cameligo/ticket_tuple.mligo" ; "--werror" ; "--disable-michelson-typechecking" ] ;
  [%expect{|
    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_tuple.mligo", line 1, characters 5-6:
      1 | let (b, _) = (Tezos.create_ticket "one" 10n, 1)
      2 |
    :
    Warning: variable "b" cannot be used more than once.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_tuple.mligo", line 1, characters 5-6:
      1 | let (b, _) = (Tezos.create_ticket "one" 10n, 1)
      2 |
    :
    Warning: variable "b" cannot be used more than once.
    { parameter unit ;
      storage (ticket string) ;
      code { DROP ;
             PUSH int 1 ;
             PUSH nat 10 ;
             PUSH string "one" ;
             TICKET ;
             SWAP ;
             DROP ;
             DUP ;
             PAIR ;
             JOIN_TICKETS ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "cameligo/constr_record_destructuring.mligo" ; "--werror" ; "--disable-michelson-typechecking" ] ;
  [%expect{|
    File "../../test/contracts/top_level_patterns/negative/cameligo/constr_record_destructuring.mligo", line 4, characters 4-26:
      3 |
      4 | let { a ; b = (Foo x) ; c} = { a = 1 ; b = Foo 2 ; c = "hey" }
      5 |

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - {c = _; b = Bar; a = _} |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "cameligo/constr_tuple_destructuring.mligo" ; "--werror" ; "--disable-michelson-typechecking" ] ;
  [%expect{|
    File "../../test/contracts/top_level_patterns/negative/cameligo/constr_tuple_destructuring.mligo", line 3, characters 5-19:
      2 |
      3 | let (a,  (Foo x), c) = (1, Foo 2, "hey")
      4 |

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - (_, Bar, _) |}]

let%expect_test _ =
  run_ligo_bad [ "run" ; "test" ; contract "cameligo/constr_let_in.mligo" ] ;
  [%expect{|
    File "../../test/contracts/top_level_patterns/negative/cameligo/constr_let_in.mligo", line 4, characters 6-7:
      3 | let test =
      4 |   let B = B in
      5 |   ()

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - A |}]

let%expect_test _ =
  run_ligo_bad [ "run" ; "test" ; contract "cameligo/constr_let_in2.mligo" ] ;
  [%expect{|
    File "../../test/contracts/top_level_patterns/negative/cameligo/constr_let_in2.mligo", line 2, characters 6-10:
      1 | let test =
      2 |   let True = true in
      3 |   42

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - False |}]