(* 
(* compilation *)
tuple
record
nested tuple
tuple record
nested record
record tuple

top-level ticket tuple
top-level ticket record

---
(* interpreter *)

top-level tuple destructuring Test.log
top-lvel record destructuring Test.log
top-lvel record tuple destructuring Test.log
top-lvel tuple record destructuring Test.log
top-lvel nested record destructuring Test.log
top-lvel nested record destructuring Test.log

---
(* negative *)

top-level record pattern linearity
*)
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
    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 3, characters 10-12:
      2 |
      3 | let { a = a1 ; b = b1 ; c = c1 }
      4 |     = { a = Tezos.create_ticket 1 10n
    :
    Warning: variable "a1" cannot be used more than once.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 3, characters 19-21:
      2 |
      3 | let { a = a1 ; b = b1 ; c = c1 }
      4 |     = { a = Tezos.create_ticket 1 10n
    :
    Warning: variable "b1" cannot be used more than once.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 3, characters 28-30:
      2 |
      3 | let { a = a1 ; b = b1 ; c = c1 }
      4 |     = { a = Tezos.create_ticket 1 10n
    :
    Warning: variable "c1" cannot be used more than once.

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
    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 1, characters 5-7:
      1 | let (a1, a2, a3)
      2 |   = ( Tezos.create_ticket 1 10n
    :
    Warning: variable "a1" cannot be used more than once.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 1, characters 9-11:
      1 | let (a1, a2, a3)
      2 |   = ( Tezos.create_ticket 1 10n
    :
    Warning: variable "a2" cannot be used more than once.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 1, characters 13-15:
      1 | let (a1, a2, a3)
      2 |   = ( Tezos.create_ticket 1 10n
    :
    Warning: variable "a3" cannot be used more than once.

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


(* Negative *)