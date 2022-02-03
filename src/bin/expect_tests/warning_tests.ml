open Cli_expect

let contract = test

let%expect_test _ =
  run_ligo_good ["compile"; "contract" ; contract "unused_recursion.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH int 2 ;
             LEFT int ;
             LOOP_LEFT { PUSH int 1 ; ADD ; RIGHT int } ;
             NIL operation ;
             PAIR } } |} ]

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
      code { DROP ;
             PUSH int 2 ;
             LEFT int ;
             LOOP_LEFT { PUSH int 1 ; ADD ; RIGHT int } ;
             NIL operation ;
             PAIR } } |} ]