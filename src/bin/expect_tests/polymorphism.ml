open Cli_expect

let test basename = "./" ^ basename
let pwd = Sys.getcwd ()
let () = Sys.chdir "../../test/contracts/polymorphism/"

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "pascaligo" ; "zip(list [1;2;3], list [4n;5n;6n])" ; "--init-file" ; (test "comb.ligo") ] ;
  [%expect{| { Pair 1 4 ; Pair 2 5 ; Pair 3 6 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "pascaligo" ; "zip (zip(list [1;2;3], list [4n;5n;6n]), list [\"a\";\"b\";\"c\"])" ; "--init-file" ; (test "comb.ligo") ] ;
  [%expect{| { Pair (Pair 1 4) "a" ; Pair (Pair 2 5) "b" ; Pair (Pair 3 6) "c" } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "zip [1;2;3] [4n;5n;6n]" ; "--init-file" ; (test "comb.mligo") ] ;
  [%expect{| { Pair 1 4 ; Pair 2 5 ; Pair 3 6 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "zip (zip [1;2;3] [4n;5n;6n]) [\"a\";\"b\";\"c\"]" ; "--init-file" ; (test "comb.mligo") ] ;
  [%expect{| { Pair (Pair 1 4) "a" ; Pair (Pair 2 5) "b" ; Pair (Pair 3 6) "c" } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "let (x, y) = diag 4 in x + y" ; "--init-file" ; (test "comb.mligo") ] ;
  [%expect{|
    Toplevel let declaration are silently change to const declaration.

    8 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "v" ; "--init-file" ; (test "comb.mligo") ] ;
  [%expect{| { Pair "a" "a" ; Pair "b" "b" } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "w" ; "--init-file" ; (test "comb.mligo") ] ;
  [%expect{| { Pair 1 4 ; Pair 2 5 ; Pair 3 6 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "(zip([1,2,3]))([4n,5n,6n])" ; "--init-file" ; (test "comb.religo") ] ;
  [%expect{|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    { Pair 1 4 ; Pair 2 5 ; Pair 3 6 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "(zip((zip([1,2,3]))([4n,5n,6n])))([\"a\",\"b\",\"c\"])" ; "--init-file" ; (test "comb.religo") ] ;
  [%expect{|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    { Pair (Pair 1 4) "a" ; Pair (Pair 2 5) "b" ; Pair (Pair 3 6) "c" } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "jsligo" ; "(zip(list([1,2,3])))(list([(4 as nat),(5 as nat),(6 as nat)]))" ; "--init-file" ; (test "comb.jsligo") ] ;
  [%expect{|
    File "./comb.jsligo", line 3, characters 4-8:
      2 |
      3 | let diag : <T>((x : T) => dup<T>) = (x : T) => [x, x];
      4 |

    Toplevel let declaration are silently change to const declaration.

    File "./comb.jsligo", line 5, characters 4-8:
      4 |
      5 | let _rev : <T>((p : [list<T>, list<T>]) => list<T>) = ([xs, acc] : [list<T>, list<T>]) : list<T> =>
      6 |   match(xs, list([

    Toplevel let declaration are silently change to const declaration.

    File "./comb.jsligo", line 11, characters 4-7:
     10 |
     11 | let rev : <T>((xs : list<T>) => list<T>) = (xs : list<T>) : list<T> => _rev([xs, (list([]) as list<T>)]);
     12 |

    Toplevel let declaration are silently change to const declaration.

    File "./comb.jsligo", line 14, characters 4-8:
     13 |
     14 | let _zip : <T,U>((p : [list<T>, list<U>, list<[T, U]>]) => list<[T, U]>) = ([xs, ys, acc] : [list<T>, list<U>, list<[T, U]>]) : list<[T, U]> =>
     15 |   match(xs, list([

    Toplevel let declaration are silently change to const declaration.

    File "./comb.jsligo", line 28, characters 4-7:
     27 |
     28 | let zip : <T,U>((xs : list<T>) => ((ys : list<U>) => list<[T, U]>)) = (xs : list<T>) => ((ys : list<U>) : list<[T, U]> => rev (_zip ([xs, ys, (list([]) as list<[T, U]>)])));
     29 |

    Toplevel let declaration are silently change to const declaration.

    File "./comb.jsligo", line 30, characters 4-12:
     29 |
     30 | let self_zip : <T>((xs : list<T>) => list<[T, T]>) = (xs : list<T>) : list<[T, T]> => {
     31 |   let [xs, ys] = diag(xs);

    Toplevel let declaration are silently change to const declaration.

    File "./comb.jsligo", line 35, characters 4-5:
     34 |
     35 | let v : list<[string, string]> = self_zip (list(["a","b"]));
     36 | let w : list<[int, nat]> = (zip (list([1,2,3])))(list([(4 as nat),(5 as nat),(6 as nat)]));

    Toplevel let declaration are silently change to const declaration.

    File "./comb.jsligo", line 36, characters 4-5:
     35 | let v : list<[string, string]> = self_zip (list(["a","b"]));
     36 | let w : list<[int, nat]> = (zip (list([1,2,3])))(list([(4 as nat),(5 as nat),(6 as nat)]));

    Toplevel let declaration are silently change to const declaration.

    { Pair 1 4 ; Pair 2 5 ; Pair 3 6 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (test "identity.jsligo") ] ;
  [%expect{|
    { parameter unit ;
      storage int ;
      code { DROP ; PUSH int 1 ; PUSH int 1 ; ADD ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "jsligo" ; "(zip((zip(list([1,2,3])))(list([(4 as nat),(5 as nat),(6 as nat)]))))(list([\"a\",\"b\",\"c\"]))" ; "--init-file" ; (test "comb.jsligo") ] ;
  [%expect{|
    File "./comb.jsligo", line 3, characters 4-8:
      2 |
      3 | let diag : <T>((x : T) => dup<T>) = (x : T) => [x, x];
      4 |

    Toplevel let declaration are silently change to const declaration.

    File "./comb.jsligo", line 5, characters 4-8:
      4 |
      5 | let _rev : <T>((p : [list<T>, list<T>]) => list<T>) = ([xs, acc] : [list<T>, list<T>]) : list<T> =>
      6 |   match(xs, list([

    Toplevel let declaration are silently change to const declaration.

    File "./comb.jsligo", line 11, characters 4-7:
     10 |
     11 | let rev : <T>((xs : list<T>) => list<T>) = (xs : list<T>) : list<T> => _rev([xs, (list([]) as list<T>)]);
     12 |

    Toplevel let declaration are silently change to const declaration.

    File "./comb.jsligo", line 14, characters 4-8:
     13 |
     14 | let _zip : <T,U>((p : [list<T>, list<U>, list<[T, U]>]) => list<[T, U]>) = ([xs, ys, acc] : [list<T>, list<U>, list<[T, U]>]) : list<[T, U]> =>
     15 |   match(xs, list([

    Toplevel let declaration are silently change to const declaration.

    File "./comb.jsligo", line 28, characters 4-7:
     27 |
     28 | let zip : <T,U>((xs : list<T>) => ((ys : list<U>) => list<[T, U]>)) = (xs : list<T>) => ((ys : list<U>) : list<[T, U]> => rev (_zip ([xs, ys, (list([]) as list<[T, U]>)])));
     29 |

    Toplevel let declaration are silently change to const declaration.

    File "./comb.jsligo", line 30, characters 4-12:
     29 |
     30 | let self_zip : <T>((xs : list<T>) => list<[T, T]>) = (xs : list<T>) : list<[T, T]> => {
     31 |   let [xs, ys] = diag(xs);

    Toplevel let declaration are silently change to const declaration.

    File "./comb.jsligo", line 35, characters 4-5:
     34 |
     35 | let v : list<[string, string]> = self_zip (list(["a","b"]));
     36 | let w : list<[int, nat]> = (zip (list([1,2,3])))(list([(4 as nat),(5 as nat),(6 as nat)]));

    Toplevel let declaration are silently change to const declaration.

    File "./comb.jsligo", line 36, characters 4-5:
     35 | let v : list<[string, string]> = self_zip (list(["a","b"]));
     36 | let w : list<[int, nat]> = (zip (list([1,2,3])))(list([(4 as nat),(5 as nat),(6 as nat)]));

    Toplevel let declaration are silently change to const declaration.

    { Pair (Pair 1 4) "a" ; Pair (Pair 2 5) "b" ; Pair (Pair 3 6) "c" } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (test "ctrct.mligo") ] ;
  [%expect{|
    { parameter unit ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; (test "test.mligo") ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "bar" ; "--init-file" ; (test "modules.mligo") ] ;
  [%expect{|
    (Pair (Some 1) (Some "hello")) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "bar" ; "--init-file" ; (test "modules.religo") ] ;
  [%expect{|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    (Pair (Some 1) (Some "hello")) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "foo" ; "--init-file" ; (test "use_nelist.mligo") ] ;
  [%expect{|
    { 2 ; 4 ; 6 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "bar" ; "--init-file" ; (test "use_nelist.mligo") ] ;
  [%expect{|
    12 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "v" ; "--init-file" ; (test "cases_annotation1.mligo") ] ;
  [%expect{|
    "hello" |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "v" ; "--init-file" ; (test "cases_annotation2.mligo") ] ;
  [%expect{|
    "hello" |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "solve 5" ; "--init-file" ; (test "use_monad.mligo") ] ;
  [%expect{| { Pair (Pair 3 4) 5 ; Pair (Pair 4 3) 5 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "solve 5" ; "--init-file" ; (test "use_monad_set.mligo") ] ;
  [%expect{| { Pair (Pair 3 4) 5 ; Pair (Pair 4 3) 5 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "auto" ; "solve(10)" ; "--init-file" ; (test "use_monad.jsligo") ] ;
  [%expect{|
    File "./use_monad.jsligo", line 11, characters 4-12:
     10 |
     11 | let interval = ([x,y] : [int, int]) : t<int> => {
     12 |   let aux = ([x, c, acc] : [int, int, t<int>]) : t<int> => {

    Toplevel let declaration are silently change to const declaration.

    File "./use_monad.jsligo", line 22, characters 4-11:
     21 |
     22 | let triples = (n : int) : t<[int, int, int]> =>
     23 |   (M.bind(interval([1, n])))((x : int) : t<[int, int, int]> =>

    Toplevel let declaration are silently change to const declaration.

    File "./use_monad.jsligo", line 32, characters 4-9:
     31 |
     32 | let solve = (n : int) : t<[int, int, int]> =>
     33 |   (M.bind(triples(n)))(([x, y, z] : [int, int, int]) : t<[int, int, int]> => {

    Toplevel let declaration are silently change to const declaration.

    { Pair (Pair 3 4) 5 ; Pair (Pair 6 8) 10 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "map (fun (f : (string -> int -> int)) -> f \"hello\" 4) (uhms : (string -> int -> int) list)" ; "--init-file" ; (test "map.mligo") ] ;
  [%expect{|
    { 4 ; 4 } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "bar 5" ; "--init-file" ; (test "use_error.mligo") ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "List.map (fun (x : int) -> x + 1) [1;2]" ; "--init-file" ; (test "map.mligo") ] ;
  [%expect{|
    CONS(2 , CONS(3 , LIST_EMPTY())) |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; (test "module_k.mligo") ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_helpers exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; (test "use_rec.jsligo") ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value 51. |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "foo" ; "--init-file" ; (test "lambda.mligo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (test "record_sapling.mligo") ] ;
  [%expect{|
    { parameter string ;
      storage (pair (string %name) (pair %state int (sapling_state 8))) ;
      code { UNPAIR ; SWAP ; CDR ; SWAP ; PAIR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "try_transfer (\"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx\" : address) 0 (Build_state (Map.empty :(address, tokenValue) map))" ; "--init-file" ; (test "map_or_big_map.mligo") ] ;
  [%expect{|
    (Some { Elt "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" 0 }) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (test "annotate.mligo") ] ;
  [%expect{| const f = Λ a ->  lambda (x : a) return x |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; (test "same_vars.mligo") ] ;
  [%expect{| 4 |}]

let () = Sys.chdir pwd ;
         Sys.chdir "../../test/contracts/negative/polymorphism/"

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (test "annotate2.mligo") ] ;
  [%expect{|
    File "./annotate2.mligo", line 1, characters 11-13:
      1 | let f (x : _a) = x

    Type "_a" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (test "constants.mligo") ] ;
  [%expect{|
    Invalid type(s)
    Cannot unify string with int. |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "f" ; "--init-file" ; (test "cases_annotation.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_bad)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_bad in file "src/bin/expect_tests/cli_expect.ml", line 37, characters 7-28
  Called from Cli_expect_tests__Polymorphism.(fun) in file "src/bin/expect_tests/polymorphism.ml", line 337, characters 2-112
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  { LAMBDA
      (pair bool string)
      string
      { UNPAIR ;
        SWAP ;
        PUSH int 2 ;
        PUSH int 40 ;
        ADD ;
        SWAP ;
        DIG 2 ;
        IF { LAMBDA
               string
               (lambda int string)
               { LAMBDA (pair string int) string { CAR } ; DUP 2 ; APPLY ; SWAP ; DROP } }
           { LAMBDA
               string
               (lambda int string)
               { LAMBDA (pair string int) string { CAR } ; DUP 2 ; APPLY ; SWAP ; DROP } } ;
        SWAP ;
        EXEC ;
        SWAP ;
        EXEC } ;
    DUP 2 ;
    APPLY ;
    SWAP ;
    DROP } |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "bar 0" ; "--init-file" ; (test "use_error.mligo") ] ;
  [%expect{|
    An error occurred while evaluating an expression: "Division by zero" |}]

(* Unresolved polymorphism *)

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; (test "unresolved/contract.mligo") ] ;
  [%expect{xxx|
    Underspecified type ^gen#240.
    Please add additional annotations. |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; (test "unresolved/contract2.mligo") ] ;
  [%expect{xxx|
    File "./unresolved/contract2.mligo", line 4, characters 13-15:
      3 | let main (_, _ : int list * nat) : (operation list * nat) =
      4 |     [], (one [])

    Underspecified type ^gen#238.
    Please add additional annotations. |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "storage" ; (test "unresolved/storage.mligo") ; "s" ] ;
  [%expect{xxx|
    Underspecified type ^gen#235.
    Please add additional annotations. |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "parameter" ; (test "unresolved/parameter.mligo") ; "p" ] ;
  [%expect{xxx|
    File "./unresolved/parameter.mligo", line 1, characters 8-10:
      1 | let p = []
      2 |

    Underspecified type ^gen#235.
    Please add additional annotations. |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "[]" ] ;
  [%expect{|
    Underspecified type ^gen#2.
    Please add additional annotations. |}]

let () = Sys.chdir pwd
