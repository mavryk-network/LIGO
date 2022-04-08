open Cli_expect

let bad_test s = (bad_test "")^"/deep_pattern_matching/"^s
let good_test s = (test "")^"/deep_pattern_matching/"^s

(* Negatives *)

(* wrong fields on record pattern *)
(* wrong type on constructor argument pattern *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail16.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail16.mligo", line 6, characters 4-25:
      5 |   match action with
      6 |   | {one = _ ; three = _} -> 0

    Pattern not of the expected type record[one -> int , two -> int] |}]

(* wrong type on constructor argument pattern *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail15.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail15.mligo", line 8, characters 15-19:
      7 |   match action with
      8 |   | Increment (n, m) -> 0
      9 |   | Reset            -> 0

    Pattern not of the expected type ( int * int * int ) |}]

(* wrong unit pattern in a let destructuring *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail14.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail14.mligo", line 2, characters 6-8:
      1 | let main (_ : unit * unit) : operation list * unit =
      2 |   let () = 42n in
      3 |   (([] : operation list), ())

    Variant pattern argument is expected of type nat but is of type unit. |}]


(* Trying to match on values *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail10.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail10.mligo", line 5, characters 8-9:
      4 |   match x with
      5 |   | One 1 -> 2
      6 |   | Two -> 1

    Invalid pattern.
    Can't match on values. |}]

(* unbound variable *)

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail9.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail9.mligo", line 6, characters 11-12:
      5 |   | One a -> 2
      6 |   | Two -> a

    Variable "a" not found. |}]

(* wrong patterns type *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail1.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail1.mligo", line 6, characters 10-33:
      5 |   match x with
      6 |   | Nil , {a = a ; b = b ; c = c} -> 1
      7 |   | xs  , Nil -> 2

    Pattern not of the expected type sum[Cons -> ( int * int ) , Nil -> unit] |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail2.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail2.mligo", line 5, characters 11-16:
      4 |   match x with
      5 |   | Nil , (a,b,c) -> 1
      6 |   | xs  , Nil -> 2

    Pattern not of the expected type sum[Cons -> ( int * int ) , Nil -> unit] |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail5.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail5.mligo", line 5, characters 14-15:
      4 |   match x with
      5 |   | Some_fake x -> x
      6 |   | None_fake -> 1

    Pattern not of the expected type option (int) |}]

(* wrong body type *)

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail7.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail7.mligo", line 6, characters 9-10:
      5 |   | A -> "hey"
      6 |   | B -> 2

    Invalid type(s).
    Expected: "string", but got: "int". |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail8.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail8.mligo", line 19, characters 22-31:
     18 |         f (b+1)
     19 |       | Cons (a,b) -> "invalid"
     20 |     in

    Invalid type(s).
    Expected: "int", but got: "string". |}]


(* rendundancy detected while compiling the pattern matching *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail3.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail3.mligo", line 4, character 2 to line 6, character 21:
      3 | let t = fun (x: myt * ( int * int * int)) ->
      4 |   match x with
      5 |   | xs , (a,b,c) -> 1
      6 |   | xs , (c,b,a) -> 2

    Redundant pattern matching |}]

(* anomaly detected in the pattern matching self_ast_typed pass *)

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail11.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail11.mligo", line 2, character 2 to line 4, character 11:
      1 | let t12 = fun (x : int list) ->
      2 |   match x with
      3 |   | hd::(hd2::tl) -> hd + hd2
      4 |   | [] -> 0

    Pattern matching anomaly (redundant, or non exhaustive). |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail12.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail12.mligo", line 4, character 2 to line 6, character 40:
      3 | let t13 = fun (x:recordi) ->
      4 |   match x with
      5 |   | { a = Some ([]) ; b = (hd::tl) } -> hd
      6 |   | { a = Some (hd::tl) ; b = [] } -> hd

    Pattern matching anomaly (redundant, or non exhaustive). |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail13.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail13.mligo", line 7, characters 5-14:
      6 |    | Increment n -> s +1
      7 |    | Decrement -> s -1
      8 |  in ([] : operation list), stor

    Variant pattern argument is expected of type nat but is of type unit. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail4.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail4.mligo", line 4, character 2 to line 6, character 18:
      3 | let t = fun (x: myt * myt) ->
      4 |   match x with
      5 |   | Nil , ys  -> 1
      6 |   | xs  , Nil -> 2

    Pattern matching anomaly (redundant, or non exhaustive). |}]

(* Positives *)

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1 (Nil,Nil)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 180, characters 2-102
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1 (Nil,Cons(1,2))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 203, characters 2-108
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1 (Cons(1,2),Nil)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 226, characters 2-108
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1 (Cons(1,2),Cons(3,4))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 249, characters 2-114
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t2 Nil Nil" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 272, characters 2-100
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t2 Nil (Cons (1,2))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 295, characters 2-109
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t2 (Cons(1,2)) (Cons(1,2))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 318, characters 2-116
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t2 (Cons(1,2)) Nil" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 341, characters 2-108
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t3 (One (Nil))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 364, characters 2-104
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t3 (One (Cons(1,2)))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 387, characters 2-110
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t3 (Two {a = 1 ; b = 2n ; c = \"tri\"})" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 410, characters 2-129
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t2_3 (Cons(1,2)) Nil (One(Nil))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 433, characters 2-121
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t4 (One(Nil)) (One (Nil))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 456, characters 2-115
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t4 (One(Nil)) (Two {a=1;b=2n;c=\"tri\"})" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 479, characters 2-130
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t4 (One(Cons(1,2))) (Two {a=1;b=2n;c=\"tri\"})" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 502, characters 2-136
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t4 (Two {a=0;b=0n;c=\"\"}) (Two {a=1;b=2n;c=\"tri\"})" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 525, characters 2-143
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t5 1" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 548, characters 2-94
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t6 42" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 571, characters 2-95
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t7 (Some 10)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 594, characters 2-102
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t7 (None: int option)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 617, characters 2-111
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t8 (Some (1,2)) 2" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 640, characters 2-107
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t8 (None:(int * int) option) 2" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 663, characters 2-120
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t9 (None:int option) (None:int option)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 686, characters 2-128
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t9 (None:int option) (Some 1)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 709, characters 2-119
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t9 (Some 1) (None:int option)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 732, characters 2-119
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t9 (Some 1) (Some 2)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 755, characters 2-110
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t10 (Consi(None:int option)) (Consi(Some 100))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 778, characters 2-136
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t11 (Consi(None:int option)) (Consi(Some 100))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 801, characters 2-136
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t12 ([]: int list)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 824, characters 2-108
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t12 [1]" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 847, characters 2-97
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t12 [1;2]" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 870, characters 2-99
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t12 [1;2;3]" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 893, characters 2-101
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t12 [1;2;3;4]" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 916, characters 2-103
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t13 none_a some_a" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 939, characters 2-107
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t13 some_a a_empty_b_not" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 962, characters 2-114
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t13 some_a b_empty_a_not" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 985, characters 2-114
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t13 some_a some_a" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 1008, characters 2-107
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.mligo", line 163, characters 25-34:
  162 | let none_a = { a = (None:int list option) ; b = [42] }
  163 | let some_a = { a = Some ([1;2;3;4]) ; b = [42] }
  164 | let a_empty_b_not = { a = Some (([]: int list)) ; b = [111] }

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (good_test "pm_ticket.mligo") ] ;
  [%expect{|
    File "../../test/contracts//deep_pattern_matching/pm_ticket.mligo", line 7, characters 14-17:
      6 |   match p with
      7 |     | { myt = myt ; mynat = mynat } , None -> (([]: operation list), mynat)
      8 |     | { myt = myt ; mynat = mynat } , Some x -> (([]: operation list), x)
    :
    Warning: unused variable "myt".
    Hint: replace it by "_myt" to prevent this warning.

    File "../../test/contracts//deep_pattern_matching/pm_ticket.mligo", line 5, characters 18-19:
      4 |
      5 | let main = fun (p,s: parameter * storage) ->
      6 |   match p with
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    { parameter (pair (pair (nat %mynat) (ticket %myt int)) (option nat)) ;
      storage nat ;
      code { CAR ;
             UNPAIR ;
             CAR ;
             SWAP ;
             IF_NONE { NIL operation ; PAIR } { SWAP ; DROP ; NIL operation ; PAIR } } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (good_test "list_pattern.mligo") ] ;
  [%expect{|
    const a =
       match CONS(1 , LIST_EMPTY()) with
        | [  ] -> 1
        | a :: b :: c :: [  ] -> 2
        | _#2 -> 3 |}]


let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (good_test "pm_test.religo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Deep_pattern_matching.(fun) in file "src/bin/expect_tests/deep_pattern_matching.ml", line 1068, characters 2-72
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts//deep_pattern_matching/pm_test.religo", line 192, characters 24-36:
  191 |
  192 | let some_a = {a: (Some(([1, 2, 3, 4]))), b: [42] };
  193 |

  Invalid type(s).
  Expected: "int", but got: "list (int)". |}]
