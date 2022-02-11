open Cli_expect

let contract = test

let%expect_test _ =
  run_ligo_good ["compile"; "contract" ; contract "unused_recursion.mligo" ] ;
  [%expect {|
    File "../../test/contracts/unused_recursion.mligo", line 9, characters 10-14:
      8 |
      9 |   let rec toto : int -> int = fun (toto:int) : int -> let number = toto in number + 1 in
     10 |
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "toto" to prevent this warning.

    File "../../test/contracts/unused_recursion.mligo", line 7, characters 10-13:
      6 |
      7 |   let rec foo : (int -> int) -> int = fun (foo : (int -> int)) -> let foo = foo 0 in foo in
      8 |
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "foo" to prevent this warning.

    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |} ]
(* TODO : Add flag for warning about unused-rec
let%expect_test _ =
  run_ligo_good ["compile"; "contract" ; contract "unused_recursion.mligo" ; "--warn-unused-rec" ] ;
  [%expect {|
    File "../../test/contracts/unused_recursion.mligo", line 3, characters 10-14:
      2 |   let number = 2 in
      3 |   let rec toto : int -> int = fun (toto:int) : int -> let number = toto in number + 1 in
      4 |   toto (number)
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "toto" to prevent this warning.

    { parameter unit ;
      storage int ;
      code { DROP ; PUSH int 1 ; PUSH int 2 ; ADD ; NIL operation ; PAIR } } |} ] *)