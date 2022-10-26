open Cli_expect

let%expect_test _ =
  run_ligo_good ["compile" ; "contract" ; test "timelock.mligo" ] ;
  [%expect {|
    Warning: Error(s) occurred while type checking the produced michelson contract:
    At (unshown) location 2, ill formed type:
      01: { parameter (pair chest_key chest) ;
      02:   storage bytes ;
      03:   code { CAR ;
      04:          UNPAIR ;
      05:          PUSH nat 1000 ;
      06:          DUG 2 ;
      07:          OPEN_CHEST ;
      08:          IF_LEFT
      09:            { RIGHT (or unit unit) }
      10:            { IF { UNIT ; LEFT unit } { UNIT ; RIGHT unit } ; LEFT bytes } ;
      11:          IF_LEFT
      12:            { IF_LEFT { DROP ; PUSH bytes 0x01 } { DROP ; PUSH bytes 0x00 } }
      13:            {} ;
      14:          NIL operation ;
      15:          PAIR } }
    Use of deprecated instruction: chest_key
    Note: You compiled your contract with protocol kathmandu although we internally use protocol lima to typecheck the produced Michelson contract
    so you might want to ignore this error if related to a breaking change in protocol lima

    { parameter (pair chest_key chest) ;
      storage bytes ;
      code { CAR ;
             UNPAIR ;
             PUSH nat 1000 ;
             DUG 2 ;
             OPEN_CHEST ;
             IF_LEFT
               { RIGHT (or unit unit) }
               { IF { UNIT ; LEFT unit } { UNIT ; RIGHT unit } ; LEFT bytes } ;
             IF_LEFT
               { IF_LEFT { DROP ; PUSH bytes 0x01 } { DROP ; PUSH bytes 0x00 } }
               {} ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good ["compile" ; "contract" ; test "open_chest_result.mligo" ] ;
  [%expect {|
    { parameter (or (or (unit %fail_d) (unit %fail_t)) (bytes %ok_o)) ;
      storage (or (or (unit %fail_decrypt) (unit %fail_timelock)) (bytes %ok_opening)) ;
      code { CAR ;
             IF_LEFT
               { IF_LEFT { DROP ; UNIT ; LEFT unit } { DROP ; UNIT ; RIGHT unit } ;
                 LEFT bytes }
               { RIGHT (or unit unit) } ;
             NIL operation ;
             PAIR } } |}]