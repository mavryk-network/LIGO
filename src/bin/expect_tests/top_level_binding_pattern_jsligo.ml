open Cli_expect

let contract file = test ("top_level_patterns/contracts/" ^ file)

(* let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "jsligo/nested_record.jsligo" ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {| TODO |}] *)

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "jsligo/nested_tuple.jsligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (pair (pair nat int) string) ;
      code { DROP ;
             PUSH string "World" ;
             PUSH string "O" ;
             PUSH string "L" ;
             PUSH string "Hello" ;
             PUSH string "E" ;
             PUSH string "H" ;
             CONCAT ;
             CONCAT ;
             CONCAT ;
             CONCAT ;
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

(* let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "jsligo/record_tuple.jsligo" ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {| TODO |}] *)

(* let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "jsligo/record.jsligo" ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {| TODO|}] *)

(* let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "jsligo/ticket_record.jsligo" ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {| TODO|}] *)

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "jsligo/ticket_tuple.jsligo" ];
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

(* let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "jsligo/tuple_record.jsligo" ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {| TODO|}] *)

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "jsligo/tuple.jsligo" ];
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

(* let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/nested_record.jsligo" ] ;
  [%expect{| TODO |}] *)

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "jsligo/nested_tuple.jsligo" ];
  [%expect
    {|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

(* let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/record_tuple.jsligo" ] ;
  [%expect{| Everything at the top-level was executed. |}] *)

(* let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/tuple_record.jsligo" ] ;
  [%expect{| Everything at the top-level was executed. |}] *)

(* let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/record.jsligo" ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {| TODO |}] *)

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "jsligo/tuple.jsligo" ];
  [%expect
    {|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

(* Negative - linearity *)

let contract file = test ("top_level_patterns/negative/" ^ file)

(* let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "jsligo/nested_record.jsligo" ] ;
  [%expect{|
    Internal error: Entrypoint main does not exist |}] *)

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "jsligo/nested_tuple.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/jsligo/nested_tuple.jsligo", line 2, characters 6-48:
      1 | const r = [[1 as nat, 1, "H"], [2 as nat, 2, "E"], [3 as nat, 3, "Hello"]]
      2 | const [[a1, a2, a3], [b1, a2, b3], [c1, c2, c3]] = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

(* let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "jsligo/record.jsligo" ] ;
  [%expect{|
    Internal error: Entrypoint main does not exist |}] *)

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "jsligo/tuple.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/jsligo/tuple.jsligo", line 2, characters 6-15:
      1 | const r = [1 as nat, 1, "Hello"]
      2 | const [a, a, c] = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

(* let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "jsligo/record_tuple.jsligo" ] ;
  [%expect{|
    Internal error: Entrypoint main does not exist |}] *)

(* let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "jsligo/tuple_record.jsligo" ] ;
  [%expect{|
    Internal error: Entrypoint main does not exist |}] *)

(* Negative - much use *)

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; contract "jsligo/ticket_record.jsligo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_record.jsligo", line 3, characters 8-9:
      2 |
      3 | const { b } = { b : Tezos.create_ticket("one", 10 as nat) }
      4 |
    :
    Warning: variable "b" cannot be used more than once. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; contract "jsligo/ticket_tuple.jsligo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_tuple.jsligo", line 1, characters 7-8:
      1 | const [b, _] = [Tezos.create_ticket("one", 10 as nat), 1]
      2 |
    :
    Warning: variable "b" cannot be used more than once. |}]
