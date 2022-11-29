open Cli_expect

let contract file = test ("top_level_patterns/contracts/" ^ file)

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "pascaligo/nested_record.ligo" ];
  [%expect
    {|
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
  run_ligo_good [ "compile"; "contract"; contract "pascaligo/nested_tuple.ligo" ];
  [%expect
    {|
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
  run_ligo_good [ "compile"; "contract"; contract "pascaligo/record_tuple.ligo" ];
  [%expect
    {|
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
  run_ligo_good [ "compile"; "contract"; contract "pascaligo/record.ligo" ];
  [%expect
    {|
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
  run_ligo_good [ "compile"; "contract"; contract "pascaligo/ticket_record.ligo" ];
  [%expect
    {|
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
  run_ligo_good [ "compile"; "contract"; contract "pascaligo/ticket_tuple.ligo" ];
  [%expect
    {|
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
  run_ligo_good [ "compile"; "contract"; contract "pascaligo/tuple_record.ligo" ];
  [%expect
    {|
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
  run_ligo_good [ "compile"; "contract"; contract "pascaligo/tuple.ligo" ];
  [%expect
    {|
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
  run_ligo_good [ "run"; "test"; test_ "pascaligo/nested_record.ligo" ];
  [%expect
    {|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "pascaligo/nested_tuple.ligo" ];
  [%expect
    {|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "pascaligo/record_tuple.ligo" ];
  [%expect
    {|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "pascaligo/tuple_record.ligo" ];
  [%expect
    {|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "pascaligo/record.ligo" ];
  [%expect
    {|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "pascaligo/tuple.ligo" ];
  [%expect
    {|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

(* Negative - linearity *)

let contract file = test ("top_level_patterns/negative/" ^ file)

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "pascaligo/nested_record.ligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/pascaligo/nested_record.ligo", line 10, character 6 to line 14, character 9:
      9 |
     10 | const record
     11 |         [ a = record[ c = c1 ; d = d1 ; e = e1 ]
     12 |         ; b = record[ c = c2 ; d = d2 ; e = e2 ]
     13 |         ; c = record[ c = c3 ; d = c1 ; e = e3 ]
     14 |         ] = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "pascaligo/nested_tuple.ligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/pascaligo/nested_tuple.ligo", line 2, characters 6-48:
      1 | const r = ((1n, 1, "H"), (2n, 2, "E"), (3n, 3, "Hello"))
      2 | const ((a1, a2, a3), (b1, a2, b3), (c1, c2, c3)) = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "pascaligo/record.ligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/pascaligo/record.ligo", line 4, characters 6-29:
      3 | const r = record[ a = 1n ; b = 1 ; c = "Hello" ]
      4 | const record[ a ; b = a ; c ] = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "pascaligo/tuple.ligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/pascaligo/tuple.ligo", line 2, characters 6-15:
      1 | const r = (1n, 1, "Hello")
      2 | const (a, a, c) = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "pascaligo/record_tuple.ligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/pascaligo/record_tuple.ligo", line 12, character 6 to line 16, character 9:
     11 |         ]
     12 | const record
     13 |         [ a = (a1, a2, a3)
     14 |         ; b = (b1, a2, b3)
     15 |         ; c = (c1, c2, c3)
     16 |         ] = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "pascaligo/tuple_record.ligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/pascaligo/tuple_record.ligo", line 7, character 6 to line 10, character 7:
      6 |           )
      7 | const ( record[ a = a1 ; b = b1 ; c = c1 ]
      8 |       , record[ a = a2 ; b = b2 ; c = a2 ]
      9 |       , record[ a = a3 ; b = b3 ; c = c3 ]
     10 |       ) = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

(* Negative - much use *)

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; contract "pascaligo/ticket_record.ligo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/pascaligo/ticket_record.ligo", line 3, characters 14-15:
      2 |
      3 | const record[ b ] = record[ b = Tezos.create_ticket ("one", 10n) ]
      4 |
    :
    Warning: variable "b" cannot be used more than once. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; contract "pascaligo/ticket_tuple.ligo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/pascaligo/ticket_tuple.ligo", line 1, characters 7-8:
      1 | const (b, _) = (Tezos.create_ticket ("one", 10n), 1)
      2 |
    :
    Warning: variable "b" cannot be used more than once. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; contract "pascaligo/constr_record_destructuring.ligo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/pascaligo/constr_record_destructuring.ligo", line 4, characters 6-37:
      3 |
      4 | const record[ a ; b = (Foo (x)) ; c ] = record[ a = 1 ; b = Foo (2) ; c = "hey" ]
      5 |

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - record [c = _; b = Bar; a = _] |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; contract "pascaligo/constr_tuple_destructuring.ligo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/pascaligo/constr_tuple_destructuring.ligo", line 3, characters 6-23:
      2 |
      3 | const (a, (Foo (x)), c) = (1, Foo (2), "hey")
      4 |

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - (_, Bar, _) |}]
