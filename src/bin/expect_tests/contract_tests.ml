open Cli_expect

let contract = test
let contract_resource name = test ("res/" ^ name)
let bad_contract = bad_test

(* avoid pretty printing *)
let () = Unix.putenv ~key:"TERM" ~data:"dumb"

let%expect_test _ =
  run_ligo_good [ "info" ; "measure-contract" ; contract "coase.ligo" ] ;
  [%expect{| 1175 bytes |}] ;

  run_ligo_good [ "info" ; "measure-contract" ; contract "multisig.ligo" ] ;
  [%expect{| 583 bytes |}] ;

  run_ligo_good [ "info" ; "measure-contract" ; contract "multisig-v2.ligo" ] ;
  [%expect{| 1639 bytes |}] ;

  run_ligo_good [ "info" ; "measure-contract" ; contract "vote.mligo" ] ;
  [%expect{| 430 bytes |}] ;

  run_ligo_good [ "compile" ; "parameter" ; contract "coase.ligo" ; "Buy_single (record [ card_to_buy = 1n ])" ] ;
  [%expect{| (Left (Left 1)) |}] ;

  run_ligo_good [ "compile" ; "storage" ; contract "coase.ligo" ; "record [ cards = (map [] : cards) ; card_patterns = (map [] : card_patterns) ; next_id = 3n ]" ] ;
  [%expect{| (Pair (Pair {} {}) 3) |}] ;

  run_ligo_bad [ "compile" ; "storage" ; contract "coase.ligo" ; "Buy_single (record [ card_to_buy = 1n ])" ] ;
  [%expect{|
    Invalid command line argument.
    The provided storage does not have the correct type for the contract.
    File "../../test/contracts/coase.ligo", line 124, character 0 to line 129, character 3:
    123 |
    124 | function main (const action : parameter; const s : storage) : return is
    125 |   case action of [
    126 |     Buy_single (bs)      -> buy_single (bs, s)
    127 |   | Sell_single (as)     -> sell_single (as, s)
    128 |   | Transfer_single (at) -> transfer_single (at, s)
    129 |   ]

    Invalid type(s).
    Expected: "storage", but got: "parameter". |}] ;

  run_ligo_bad [ "compile" ; "parameter" ; contract "coase.ligo" ; "record [ cards = (map [] : cards) ; card_patterns = (map [] : card_patterns) ; next_id = 3n ]" ] ;
  [%expect{|
    Invalid command line argument.
    The provided parameter does not have the correct type for the given entrypoint.
    File "../../test/contracts/coase.ligo", line 124, character 0 to line 129, character 3:
    123 |
    124 | function main (const action : parameter; const s : storage) : return is
    125 |   case action of [
    126 |     Buy_single (bs)      -> buy_single (bs, s)
    127 |   | Sell_single (as)     -> sell_single (as, s)
    128 |   | Transfer_single (at) -> transfer_single (at, s)
    129 |   ]

    Invalid type(s).
    Expected: "parameter", but got: "storage". |}] ;

  ()

let%expect_test _  =
  run_ligo_good [ "compile" ; "storage" ; contract "timestamp.ligo" ; "Tezos.now" ; "--now" ; "2042-01-01T00:00:00Z" ] ;
  [%expect {|
    File "../../test/contracts/timestamp.ligo", line 3, characters 21-22:
      2 |
      3 | function main (const p : unit; const s : storage_) :
      4 |   list (operation) * storage_ is ((nil: list (operation)), Tezos.now)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/timestamp.ligo", line 3, characters 37-38:
      2 |
      3 | function main (const p : unit; const s : storage_) :
      4 |   list (operation) * storage_ is ((nil: list (operation)), Tezos.now)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    "2042-01-01T00:00:29Z" |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "coase.ligo" ] ;
  [%expect{|
    { parameter
        (or (or (nat %buy_single) (nat %sell_single))
            (pair %transfer_single (nat %card_to_transfer) (address %destination))) ;
      storage
        (pair (pair (map %card_patterns nat (pair (mutez %coefficient) (nat %quantity)))
                    (map %cards nat (pair (address %card_owner) (nat %card_pattern))))
              (nat %next_id)) ;
      code { UNPAIR ;
             IF_LEFT
               { IF_LEFT
                   { SWAP ;
                     DUP ;
                     DUG 2 ;
                     CAR ;
                     CAR ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     GET ;
                     IF_NONE { PUSH string "buy_single: No card pattern." ; FAILWITH } {} ;
                     PUSH nat 1 ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     ADD ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CAR ;
                     MUL ;
                     AMOUNT ;
                     SWAP ;
                     COMPARE ;
                     GT ;
                     IF { PUSH string "Not enough money" ; FAILWITH } {} ;
                     PUSH nat 1 ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     ADD ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     DUP 3 ;
                     CDR ;
                     DUP 4 ;
                     CAR ;
                     CDR ;
                     DIG 4 ;
                     CAR ;
                     CAR ;
                     DIG 3 ;
                     DUP 5 ;
                     SWAP ;
                     SOME ;
                     SWAP ;
                     UPDATE ;
                     PAIR ;
                     PAIR ;
                     DUP ;
                     CAR ;
                     CDR ;
                     DIG 2 ;
                     SENDER ;
                     PAIR ;
                     DUP 3 ;
                     CDR ;
                     SWAP ;
                     SOME ;
                     SWAP ;
                     UPDATE ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     SWAP ;
                     DIG 2 ;
                     CAR ;
                     CAR ;
                     PAIR ;
                     PAIR ;
                     PUSH nat 1 ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     ADD ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     NIL operation ;
                     PAIR }
                   { SWAP ;
                     DUP ;
                     DUG 2 ;
                     CAR ;
                     CDR ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     GET ;
                     IF_NONE { PUSH string "sell_single: No card." ; FAILWITH } {} ;
                     SENDER ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CAR ;
                     COMPARE ;
                     NEQ ;
                     IF { PUSH string "This card doesn't belong to you" ; FAILWITH } {} ;
                     DUP 3 ;
                     CAR ;
                     CAR ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     GET ;
                     IF_NONE { PUSH string "sell_single: No card pattern." ; FAILWITH } {} ;
                     PUSH nat 1 ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     SUB ;
                     ABS ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     DUP 4 ;
                     CDR ;
                     DUP 5 ;
                     CAR ;
                     CDR ;
                     DIG 5 ;
                     CAR ;
                     CAR ;
                     DUP 4 ;
                     DIG 5 ;
                     CDR ;
                     SWAP ;
                     SOME ;
                     SWAP ;
                     UPDATE ;
                     PAIR ;
                     PAIR ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     DIG 2 ;
                     CAR ;
                     MUL ;
                     SENDER ;
                     CONTRACT unit ;
                     IF_NONE { PUSH string "sell_single: No contract." ; FAILWITH } {} ;
                     SWAP ;
                     UNIT ;
                     TRANSFER_TOKENS ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     DUP 3 ;
                     CAR ;
                     CDR ;
                     DIG 4 ;
                     NONE (pair address nat) ;
                     SWAP ;
                     UPDATE ;
                     DIG 3 ;
                     CAR ;
                     CAR ;
                     PAIR ;
                     PAIR ;
                     NIL operation ;
                     DIG 2 ;
                     CONS ;
                     PAIR } }
               { SWAP ;
                 DUP ;
                 DUG 2 ;
                 CAR ;
                 CDR ;
                 DUP ;
                 DUP 3 ;
                 CAR ;
                 GET ;
                 IF_NONE { PUSH string "transfer_single: No card." ; FAILWITH } {} ;
                 SENDER ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CAR ;
                 COMPARE ;
                 NEQ ;
                 IF { PUSH string "This card doesn't belong to you" ; FAILWITH } {} ;
                 DUP 4 ;
                 CDR ;
                 DUG 2 ;
                 CDR ;
                 DUP 4 ;
                 CDR ;
                 PAIR ;
                 DIG 3 ;
                 CAR ;
                 SWAP ;
                 SOME ;
                 SWAP ;
                 UPDATE ;
                 DIG 2 ;
                 CAR ;
                 CAR ;
                 PAIR ;
                 PAIR ;
                 NIL operation ;
                 PAIR } } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "multisig.ligo" ] ;
  [%expect{|
    { parameter
        (pair (pair (nat %counter) (lambda %message unit (list operation)))
              (list %signatures (pair key_hash signature))) ;
      storage
        (pair (pair (list %auth key) (nat %counter)) (pair (string %id) (nat %threshold))) ;
      code { UNPAIR ;
             DUP ;
             CAR ;
             CDR ;
             DUP 3 ;
             CAR ;
             CDR ;
             DUP 3 ;
             CAR ;
             CAR ;
             COMPARE ;
             NEQ ;
             IF { SWAP ; DROP ; PUSH string "Counters does not match" ; FAILWITH }
                { CHAIN_ID ;
                  DUP 4 ;
                  CDR ;
                  CAR ;
                  PAIR ;
                  DUP 3 ;
                  CAR ;
                  CAR ;
                  DUP 3 ;
                  PAIR ;
                  PAIR ;
                  UNIT ;
                  PUSH nat 0 ;
                  DUP 6 ;
                  CAR ;
                  CAR ;
                  PAIR ;
                  PAIR ;
                  DIG 3 ;
                  CDR ;
                  ITER { SWAP ;
                         CAR ;
                         UNPAIR ;
                         DUP ;
                         IF_CONS
                           { DIG 2 ;
                             DROP ;
                             DUP ;
                             HASH_KEY ;
                             DUP 5 ;
                             CAR ;
                             COMPARE ;
                             EQ ;
                             IF { DUP 5 ;
                                  PACK ;
                                  DIG 4 ;
                                  CDR ;
                                  DIG 2 ;
                                  CHECK_SIGNATURE ;
                                  IF { PUSH nat 1 ; DIG 2 ; ADD ; UNIT ; SWAP ; DIG 2 ; PAIR ; PAIR }
                                     { PUSH string "Invalid signature" ; FAILWITH } }
                                { DIG 3 ; DROP 2 ; UNIT ; DUG 2 ; PAIR ; PAIR } }
                           { DIG 2 ; DROP ; UNIT ; DUG 2 ; PAIR ; PAIR } } ;
                  SWAP ;
                  DROP ;
                  CAR ;
                  CDR ;
                  DUP 3 ;
                  CDR ;
                  CDR ;
                  SWAP ;
                  COMPARE ;
                  LT ;
                  IF { PUSH string "Not enough signatures passed the check" ; FAILWITH }
                     { SWAP ;
                       DUP ;
                       DUG 2 ;
                       CDR ;
                       PUSH nat 1 ;
                       DUP 4 ;
                       CAR ;
                       CDR ;
                       ADD ;
                       DIG 3 ;
                       CAR ;
                       CAR ;
                       PAIR ;
                       PAIR ;
                       UNIT ;
                       SWAP ;
                       PAIR } } ;
             CAR ;
             UNIT ;
             DIG 2 ;
             SWAP ;
             EXEC ;
             PAIR } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "multisig-v2.ligo" ] ;
  [%expect{|
    { parameter
        (or (or (unit %default) (lambda %send bytes (list operation)))
            (lambda %withdraw bytes (list operation))) ;
      storage
        (pair (pair (pair (set %authorized_addresses address) (nat %max_message_size))
                    (pair (nat %max_proposal) (map %message_store bytes (set address))))
              (pair (pair (map %proposal_counters address nat) (bytes %state_hash))
                    (nat %threshold))) ;
      code { UNPAIR ;
             IF_LEFT
               { IF_LEFT
                   { DROP ; NIL operation ; PAIR }
                   { SWAP ;
                     DUP ;
                     DUG 2 ;
                     CAR ;
                     CAR ;
                     CAR ;
                     SENDER ;
                     MEM ;
                     NOT ;
                     IF { PUSH string "Unauthorized address" ; FAILWITH } {} ;
                     DUP ;
                     PACK ;
                     DUP 3 ;
                     CAR ;
                     CAR ;
                     CDR ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     SIZE ;
                     COMPARE ;
                     GT ;
                     IF { PUSH string "Message size exceed maximum limit" ; FAILWITH } {} ;
                     DUP 3 ;
                     CAR ;
                     CDR ;
                     CDR ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     GET ;
                     IF_NONE
                       { DUP 3 ;
                         CDR ;
                         CDR ;
                         DUP 4 ;
                         CDR ;
                         CAR ;
                         CDR ;
                         DUP 5 ;
                         CDR ;
                         CAR ;
                         CAR ;
                         PUSH nat 1 ;
                         DUP 7 ;
                         CDR ;
                         CAR ;
                         CAR ;
                         SENDER ;
                         GET ;
                         IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                         ADD ;
                         SOME ;
                         SENDER ;
                         UPDATE ;
                         PAIR ;
                         PAIR ;
                         DIG 3 ;
                         CAR ;
                         PAIR ;
                         EMPTY_SET address ;
                         PUSH bool True ;
                         SENDER ;
                         UPDATE ;
                         UNIT ;
                         DUG 2 ;
                         PAIR ;
                         PAIR }
                       { DUP ;
                         SENDER ;
                         MEM ;
                         IF { UNIT ; DIG 4 ; PAIR }
                            { DUP 4 ;
                              CDR ;
                              CDR ;
                              DUP 5 ;
                              CDR ;
                              CAR ;
                              CDR ;
                              DUP 6 ;
                              CDR ;
                              CAR ;
                              CAR ;
                              PUSH nat 1 ;
                              DUP 8 ;
                              CDR ;
                              CAR ;
                              CAR ;
                              SENDER ;
                              GET ;
                              IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                              ADD ;
                              SOME ;
                              SENDER ;
                              UPDATE ;
                              PAIR ;
                              PAIR ;
                              DIG 4 ;
                              CAR ;
                              PAIR ;
                              UNIT ;
                              SWAP ;
                              PAIR } ;
                         CAR ;
                         SWAP ;
                         SENDER ;
                         PAIR ;
                         UNIT ;
                         DUG 2 ;
                         UNPAIR ;
                         PUSH bool True ;
                         SWAP ;
                         UPDATE ;
                         PAIR ;
                         PAIR } ;
                     CAR ;
                     UNPAIR ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     CAR ;
                     CAR ;
                     SENDER ;
                     GET ;
                     IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                     DUP 3 ;
                     CAR ;
                     CDR ;
                     CAR ;
                     SWAP ;
                     COMPARE ;
                     GT ;
                     IF { PUSH string "Maximum number of proposal reached" ; FAILWITH } {} ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     CDR ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     SIZE ;
                     COMPARE ;
                     GE ;
                     IF { SWAP ;
                          DUP ;
                          DUG 2 ;
                          CDR ;
                          DUP 3 ;
                          CAR ;
                          CDR ;
                          CDR ;
                          DUP 5 ;
                          NONE (set address) ;
                          SWAP ;
                          UPDATE ;
                          DUP 4 ;
                          CAR ;
                          CDR ;
                          CAR ;
                          PAIR ;
                          DIG 3 ;
                          CAR ;
                          CAR ;
                          PAIR ;
                          PAIR ;
                          DUP ;
                          CDR ;
                          CAR ;
                          CDR ;
                          DIG 4 ;
                          SWAP ;
                          EXEC ;
                          SWAP ;
                          DUP ;
                          DUG 2 ;
                          CDR ;
                          CDR ;
                          DIG 4 ;
                          DUP 4 ;
                          CDR ;
                          CAR ;
                          CDR ;
                          CONCAT ;
                          SHA256 ;
                          DUP 4 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          PAIR ;
                          PAIR ;
                          DIG 2 ;
                          CAR ;
                          PAIR ;
                          UNIT ;
                          SWAP ;
                          DUP ;
                          DUG 2 ;
                          DIG 3 ;
                          PAIR ;
                          PAIR ;
                          SWAP ;
                          CDR ;
                          CAR ;
                          CAR ;
                          ITER { SWAP ;
                                 CAR ;
                                 UNPAIR ;
                                 DIG 2 ;
                                 UNPAIR ;
                                 DUP 5 ;
                                 SWAP ;
                                 DUP ;
                                 DUG 2 ;
                                 MEM ;
                                 IF { DUP 4 ;
                                      CDR ;
                                      CDR ;
                                      DUP 5 ;
                                      CDR ;
                                      CAR ;
                                      CDR ;
                                      DUP 6 ;
                                      CDR ;
                                      CAR ;
                                      CAR ;
                                      PUSH nat 1 ;
                                      DIG 5 ;
                                      SUB ;
                                      ABS ;
                                      DIG 4 ;
                                      SWAP ;
                                      SOME ;
                                      SWAP ;
                                      UPDATE ;
                                      PAIR ;
                                      PAIR ;
                                      DIG 2 ;
                                      CAR ;
                                      PAIR ;
                                      UNIT ;
                                      SWAP ;
                                      DIG 2 ;
                                      PAIR ;
                                      PAIR }
                                    { DROP 2 ; UNIT ; DUG 2 ; PAIR ; PAIR } } ;
                          SWAP ;
                          DROP }
                        { DIG 3 ;
                          DROP ;
                          UNIT ;
                          DUP 3 ;
                          CDR ;
                          DUP 4 ;
                          CAR ;
                          CDR ;
                          CDR ;
                          DIG 3 ;
                          DIG 5 ;
                          SWAP ;
                          SOME ;
                          SWAP ;
                          UPDATE ;
                          DUP 4 ;
                          CAR ;
                          CDR ;
                          CAR ;
                          PAIR ;
                          DIG 3 ;
                          CAR ;
                          CAR ;
                          PAIR ;
                          PAIR ;
                          NIL operation ;
                          PAIR ;
                          PAIR } ;
                     CAR } }
               { PACK ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CAR ;
                 CDR ;
                 CDR ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 GET ;
                 IF_NONE
                   { DROP ; UNIT ; SWAP ; PAIR }
                   { DUP ;
                     SENDER ;
                     PUSH bool False ;
                     SWAP ;
                     UPDATE ;
                     DUP ;
                     SIZE ;
                     DIG 2 ;
                     SIZE ;
                     COMPARE ;
                     NEQ ;
                     IF { DUP 3 ;
                          CDR ;
                          CDR ;
                          DUP 4 ;
                          CDR ;
                          CAR ;
                          CDR ;
                          DUP 5 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          PUSH nat 1 ;
                          DUP 7 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          SENDER ;
                          GET ;
                          IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                          SUB ;
                          ABS ;
                          SOME ;
                          SENDER ;
                          UPDATE ;
                          PAIR ;
                          PAIR ;
                          DIG 3 ;
                          CAR ;
                          PAIR ;
                          UNIT ;
                          SWAP ;
                          PAIR }
                        { UNIT ; DIG 3 ; PAIR } ;
                     CAR ;
                     PUSH nat 0 ;
                     DUP 3 ;
                     SIZE ;
                     COMPARE ;
                     EQ ;
                     IF { SWAP ;
                          DROP ;
                          UNIT ;
                          SWAP ;
                          DUP ;
                          DUG 2 ;
                          CDR ;
                          DUP 3 ;
                          CAR ;
                          CDR ;
                          CDR ;
                          DIG 4 ;
                          NONE (set address) ;
                          SWAP ;
                          UPDATE ;
                          DUP 4 ;
                          CAR ;
                          CDR ;
                          CAR ;
                          PAIR ;
                          DIG 3 ;
                          CAR ;
                          CAR ;
                          PAIR ;
                          PAIR ;
                          PAIR }
                        { UNIT ;
                          SWAP ;
                          DUP ;
                          DUG 2 ;
                          CDR ;
                          DUP 3 ;
                          CAR ;
                          CDR ;
                          CDR ;
                          DIG 4 ;
                          DIG 5 ;
                          SWAP ;
                          SOME ;
                          SWAP ;
                          UPDATE ;
                          DUP 4 ;
                          CAR ;
                          CDR ;
                          CAR ;
                          PAIR ;
                          DIG 3 ;
                          CAR ;
                          CAR ;
                          PAIR ;
                          PAIR ;
                          PAIR } } ;
                 CAR ;
                 NIL operation ;
                 PAIR } } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "vote.mligo" ] ;
  [%expect {|
{ parameter
    (or (pair %reset (pair (timestamp %finish_time) (timestamp %start_time)) (string %title))
        (or %vote (unit %nay) (unit %yea))) ;
  storage
    (pair (pair (pair (timestamp %finish_time) (nat %nay))
                (pair (timestamp %start_time) (string %title)))
          (pair (set %voters address) (nat %yea))) ;
  code { UNPAIR ;
         IF_LEFT
           { SWAP ;
             DROP ;
             PUSH nat 0 ;
             EMPTY_SET address ;
             PAIR ;
             SWAP ;
             DUP ;
             DUG 2 ;
             CDR ;
             DUP 3 ;
             CAR ;
             CDR ;
             PAIR ;
             PUSH nat 0 ;
             DIG 3 ;
             CAR ;
             CAR ;
             PAIR ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR }
           { SENDER ;
             SWAP ;
             IF_LEFT
               { DROP ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 DUP 3 ;
                 CAR ;
                 CDR ;
                 PUSH nat 1 ;
                 DUP 5 ;
                 CAR ;
                 CAR ;
                 CDR ;
                 ADD ;
                 DIG 4 ;
                 CAR ;
                 CAR ;
                 CAR ;
                 PAIR ;
                 PAIR ;
                 PAIR }
               { DROP ;
                 PUSH nat 1 ;
                 DUP 3 ;
                 CDR ;
                 CDR ;
                 ADD ;
                 DUP 3 ;
                 CDR ;
                 CAR ;
                 PAIR ;
                 DIG 2 ;
                 CAR ;
                 PAIR } ;
             DUP ;
             CDR ;
             CDR ;
             SWAP ;
             DUP ;
             DUG 2 ;
             CDR ;
             CAR ;
             DIG 3 ;
             PUSH bool True ;
             SWAP ;
             UPDATE ;
             PAIR ;
             SWAP ;
             CAR ;
             PAIR ;
             NIL operation ;
             PAIR } } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "ticket_wallet.mligo" ] ;
  [%expect {|
{ parameter
    (or (ticket %receive unit)
        (pair %send (contract %destination (ticket unit)) (nat %amount) (address %ticketer))) ;
  storage (pair (address %manager) (big_map %tickets address (ticket unit))) ;
  code { PUSH mutez 0 ;
         AMOUNT ;
         COMPARE ;
         EQ ;
         IF {} { PUSH string "failed assertion" ; FAILWITH } ;
         UNPAIR ;
         SWAP ;
         UNPAIR ;
         DIG 2 ;
         IF_LEFT
           { READ_TICKET ;
             CAR ;
             DIG 3 ;
             NONE (ticket unit) ;
             DUP 3 ;
             GET_AND_UPDATE ;
             IF_NONE
               { DIG 2 }
               { DIG 3 ;
                 PAIR ;
                 JOIN_TICKETS ;
                 IF_NONE { PUSH string "impossible?" ; FAILWITH } {} } ;
             SOME ;
             DIG 2 ;
             GET_AND_UPDATE ;
             DROP ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR }
           { SWAP ;
             DUP ;
             DUG 2 ;
             SENDER ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             DIG 2 ;
             NONE (ticket unit) ;
             DUP 3 ;
             GET 4 ;
             GET_AND_UPDATE ;
             IF_NONE
               { DROP 3 ; PUSH string "no tickets" ; FAILWITH }
               { READ_TICKET ;
                 CDR ;
                 CDR ;
                 DUP 4 ;
                 GET 3 ;
                 DUP ;
                 DIG 2 ;
                 SUB ;
                 ISNAT ;
                 IF_NONE { PUSH string "not enough tickets" ; FAILWITH } {} ;
                 SWAP ;
                 PAIR ;
                 SWAP ;
                 SPLIT_TICKET ;
                 IF_NONE
                   { DROP 3 ; PUSH string "impossible?" ; FAILWITH }
                   { UNPAIR ;
                     DUG 2 ;
                     SOME ;
                     DUP 4 ;
                     GET 4 ;
                     GET_AND_UPDATE ;
                     DROP ;
                     DIG 2 ;
                     CAR ;
                     PUSH mutez 0 ;
                     DIG 3 ;
                     TRANSFER_TOKENS ;
                     SWAP ;
                     DIG 2 ;
                     PAIR ;
                     NIL operation ;
                     DIG 2 ;
                     CONS ;
                     PAIR } } } } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "ticket_builder.mligo" ] ;
  [%expect {|
File "../../test/contracts/ticket_builder.mligo", line 29, characters 28-34:
 28 |       begin
 29 |         let ((ticketer, _), ticket) = (Tezos.read_ticket ticket : (address * (unit * nat)) * unit ticket) in
 30 |         assert (ticketer = Tezos.self_address);
:
Warning: unused variable "ticket".
Hint: replace it by "_ticket" to prevent this warning.

{ parameter
    (or (ticket %burn unit)
        (pair %mint (contract %destination (ticket unit)) (nat %amount))) ;
  storage address ;
  code { PUSH mutez 0 ;
         AMOUNT ;
         COMPARE ;
         EQ ;
         IF {} { PUSH string "failed assertion" ; FAILWITH } ;
         UNPAIR ;
         IF_LEFT
           { READ_TICKET ;
             SWAP ;
             DROP ;
             CAR ;
             SELF_ADDRESS ;
             SWAP ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             NIL operation ;
             PAIR }
           { SWAP ;
             DUP ;
             DUG 2 ;
             SENDER ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             DUP ;
             CAR ;
             PUSH mutez 0 ;
             DIG 2 ;
             CDR ;
             UNIT ;
             TICKET ;
             TRANSFER_TOKENS ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } } |} ]

let%expect_test _ =
    run_ligo_good [ "compile" ; "contract" ; contract "implicit.mligo" ] ;
    [%expect {|
      File "../../test/contracts/implicit.mligo", line 2, characters 6-7:
        1 | let main2 (p : key_hash) (s : unit) =
        2 |   let c : unit contract = Tezos.implicit_account p
        3 |   in ([] : operation list), unit
      :
      Warning: unused variable "c".
      Hint: replace it by "_c" to prevent this warning.

      File "../../test/contracts/implicit.mligo", line 1, characters 26-27:
        1 | let main2 (p : key_hash) (s : unit) =
        2 |   let c : unit contract = Tezos.implicit_account p
      :
      Warning: unused variable "s".
      Hint: replace it by "_s" to prevent this warning.

      { parameter key_hash ;
        storage unit ;
        code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "amount_lambda.mligo" ] ;
  (* AMOUNT should occur inside the second lambda, but not the first lambda *)
  [%expect {|
    File "../../test/contracts/amount_lambda.mligo", line 10, characters 12-13:
      9 |
     10 | let main (b,s : bool * (unit -> tez)) : operation list * (unit -> tez) =
     11 |   (([] : operation list), (if b then f1 () else f2 ()))
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 8, characters 7-8:
      7 | let f2 (x : unit) : unit -> tez =
      8 |   fun (x : unit) -> Tezos.amount
      9 |
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 7, characters 8-9:
      6 | (* should return an impure function *)
      7 | let f2 (x : unit) : unit -> tez =
      8 |   fun (x : unit) -> Tezos.amount
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 4, characters 7-8:
      3 |   let amt : tez = Tezos.amount in
      4 |   fun (x : unit) -> amt
      5 |
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 2, characters 8-9:
      1 | (* should return a constant function *)
      2 | let f1 (x : unit) : unit -> tez =
      3 |   let amt : tez = Tezos.amount in
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    { parameter bool ;
      storage (lambda unit mutez) ;
      code { CAR ;
             IF { AMOUNT ; LAMBDA (pair mutez unit) mutez { CAR } ; SWAP ; APPLY }
                { LAMBDA unit mutez { DROP ; AMOUNT } } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "sequence.mligo" ; ];
  [%expect {|
    const y =
      lambda (_#2 : unit) return let _x : nat = +1 in let ()#5 : unit = let _x : nat = +2 in unit in let ()#4 : unit = let _x : nat = +23 in unit in let ()#3 : unit = let _x : nat = +42 in unit in _x |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "bad_address_format.religo" ] ;
  [%expect{|
    Warning: Error(s) occurred while type checking the produced michelson contract:
    Ill typed contract:
      1: { parameter int ;
      2:   storage address ;
      3:   code { DROP /* [] */ ; PUSH address "KT1badaddr" ; NIL operation ; PAIR } }
    At line 3 characters 38 to 50, value "KT1badaddr"
    is invalid for type address.
    Invalid contract notation "KT1badaddr"

            Note: You compiled your contract with protocol hangzhou although we internally use protocol ithaca to typecheck the produced Michelson contract
            so you might want to ignore this error if related to a breaking change in protocol ithaca

    { parameter int ;
      storage address ;
      code { DROP ; PUSH address "KT1badaddr" ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "bad_timestamp.ligo" ] ;
  [%expect {|
    File "../../test/contracts/bad_timestamp.ligo", line 7, characters 30-44:
      6 |   block {
      7 |     var stamp : timestamp := ("badtimestamp" : timestamp)
      8 |   }

    Ill-formed timestamp "badtimestamp".
    At this point, a string with a RFC3339 notation or the number of seconds since Epoch is expected. |}]

let%expect_test _ =
    run_ligo_good [ "run" ; "dry-run" ; contract "redeclaration.ligo" ; "unit" ; "0" ] ;
    [%expect {|
      File "../../test/contracts/redeclaration.ligo", line 6, characters 20-21:
        5 |
        6 | function foo (const p : unit) : int is 1
      :
      Warning: unused variable "p".
      Hint: replace it by "_p" to prevent this warning.

      File "../../test/contracts/redeclaration.ligo", line 3, characters 21-22:
        2 |
        3 | function main (const p : unit; const s : int) : list (operation) * int is
        4 |   ((nil : list (operation)), foo (unit))
      :
      Warning: unused variable "p".
      Hint: replace it by "_p" to prevent this warning.

      File "../../test/contracts/redeclaration.ligo", line 3, characters 37-38:
        2 |
        3 | function main (const p : unit; const s : int) : list (operation) * int is
        4 |   ((nil : list (operation)), foo (unit))
      :
      Warning: unused variable "s".
      Hint: replace it by "_s" to prevent this warning.

      File "../../test/contracts/redeclaration.ligo", line 1, characters 20-21:
        1 | function foo (const p : unit) : int is 0
        2 |
      :
      Warning: unused variable "p".
      Hint: replace it by "_p" to prevent this warning.

      ( LIST_EMPTY() , 0 ) |}]

let%expect_test _ =
    run_ligo_good [ "run" ; "dry-run" ; contract "double_main.ligo" ; "unit" ; "0" ] ;
    [%expect {|
      File "../../test/contracts/double_main.ligo", line 5, characters 20-21:
        4 |
        5 | function main(const p : parameter; const s : storage) : return is
        6 |   ((nil : list(operation)), s+1)
      :
      Warning: unused variable "p".
      Hint: replace it by "_p" to prevent this warning.

      ( LIST_EMPTY() , 2 ) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "subtle_nontail_fail.mligo" ] ;
  [%expect {|
    File "../../test/contracts/subtle_nontail_fail.mligo", line 1, characters 10-12:
      1 | let main (ps : unit * unit) : operation list * unit =
      2 |   if true
    :
    Warning: unused variable "ps".
    Hint: replace it by "_ps" to prevent this warning.

    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH bool True ;
             IF { PUSH string "This contract always fails" ; FAILWITH }
                { PUSH string "This contract still always fails" ; FAILWITH } } } |}]

let%expect_test _ =
  (* TODO should not be bad? *)
  run_ligo_good [ "run" ; "dry-run" ; contract "subtle_nontail_fail.mligo" ; "()" ; "()" ] ;
  [%expect {|
    File "../../test/contracts/subtle_nontail_fail.mligo", line 1, characters 10-12:
      1 | let main (ps : unit * unit) : operation list * unit =
      2 |   if true
    :
    Warning: unused variable "ps".
    Hint: replace it by "_ps" to prevent this warning.

    failwith("This contract always fails") |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "self_in_lambda.mligo" ] ;
  [%expect{| "Tezos.self" must be used directly and cannot be used via another function. |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "storage" ; contract "big_map.ligo" ; "(big_map1,unit)" ] ;
  [%expect {|
    (Pair { Elt 23 0 ; Elt 42 0 } Unit) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "key_hash_comparable.ligo" ] ;
  [%expect {|
    File "../../test/contracts/key_hash_comparable.ligo", line 8, characters 21-22:
      7 |
      8 | function main (const a : int; const store : storage) : return is
      9 |   ((nil : list (operation)), store)
    :
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.

    { parameter int ;
      storage (pair (map %one key_hash nat) (big_map %two key_hash bool)) ;
      code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "long_sum_type_names.ligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/long_sum_type_names.ligo", line 2, character 2 to line 4, character 18:
      1 | type action is
      2 | | Incrementttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt of int
      3 | // | Increment of int
      4 | | Decrement of int
      5 |

    Ill-formed data constructor "Incrementttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt".
    Data constructors have a maximum length of 32 characters, which is a limitation imposed by annotations in Tezos. |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "dry-run" ; contract "super-counter.mligo" ; "test_param" ; "test_storage" ] ;
  [%expect {|
    ( LIST_EMPTY() , 3 ) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "redundant_constructors.mligo" ] ;
  [%expect{| (Pair (Left (Left 42)) (Left 42)) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "redundant_constructors_but_annotated.mligo" ] ;
  [%expect{| (Pair {} (Left 1)) |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "create_contract_toplevel.mligo" ] ;
  [%expect {|
File "../../test/contracts/negative/create_contract_toplevel.mligo", line 5, characters 10-11:
  4 |   let toto : operation * address = Tezos.create_contract
  5 |     (fun (p, s : nat * string) -> (([] : operation list), store))
  6 |     (None: key_hash option)
:
Warning: unused variable "p".
Hint: replace it by "_p" to prevent this warning.

File "../../test/contracts/negative/create_contract_toplevel.mligo", line 5, characters 13-14:
  4 |   let toto : operation * address = Tezos.create_contract
  5 |     (fun (p, s : nat * string) -> (([] : operation list), store))
  6 |     (None: key_hash option)
:
Warning: unused variable "s".
Hint: replace it by "_s" to prevent this warning.

File "../../test/contracts/negative/create_contract_toplevel.mligo", line 3, characters 10-16:
  2 |
  3 | let main (action, store : string * string) : return =
  4 |   let toto : operation * address = Tezos.create_contract
:
Warning: unused variable "action".
Hint: replace it by "_action" to prevent this warning.

File "../../test/contracts/negative/create_contract_toplevel.mligo", line 4, character 35 to line 8, character 8:
  3 | let main (action, store : string * string) : return =
  4 |   let toto : operation * address = Tezos.create_contract
  5 |     (fun (p, s : nat * string) -> (([] : operation list), store))
  6 |     (None: key_hash option)
  7 |     300tz
  8 |     "un"
  9 |   in

Not all free variables could be inlined in Tezos.create_contract usage: gen#17. |}] ;

  run_ligo_good [ "compile" ; "contract" ; contract "create_contract_var.mligo" ] ;
  [%expect{|
    File "../../test/contracts/create_contract_var.mligo", line 7, characters 10-11:
      6 |   let toto : operation * address = Tezos.create_contract
      7 |     (fun (p, s : nat * int) -> (([] : operation list), a))
      8 |     (None: key_hash option)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/create_contract_var.mligo", line 7, characters 13-14:
      6 |   let toto : operation * address = Tezos.create_contract
      7 |     (fun (p, s : nat * int) -> (([] : operation list), a))
      8 |     (None: key_hash option)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/create_contract_var.mligo", line 5, characters 10-16:
      4 |
      5 | let main (action, store : string * string) : return =
      6 |   let toto : operation * address = Tezos.create_contract
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    { parameter string ;
      storage string ;
      code { CDR ;
             PUSH int 1 ;
             PUSH mutez 300000000 ;
             NONE key_hash ;
             CREATE_CONTRACT
               { parameter nat ;
                 storage int ;
                 code { DROP ; PUSH int 2 ; NIL operation ; PAIR } } ;
             PAIR ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CAR ;
             CONS ;
             PAIR } } |}] ;

  run_ligo_bad [ "compile" ; "contract" ; bad_contract "create_contract_modfv.mligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/create_contract_modfv.mligo", line 8, characters 10-11:
      7 |   let toto : operation * address = Tezos.create_contract
      8 |     (fun (p, s : nat * string) -> (([] : operation list), Foo.store))
      9 |     (None: key_hash option)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/negative/create_contract_modfv.mligo", line 8, characters 13-14:
      7 |   let toto : operation * address = Tezos.create_contract
      8 |     (fun (p, s : nat * string) -> (([] : operation list), Foo.store))
      9 |     (None: key_hash option)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/negative/create_contract_modfv.mligo", line 3, characters 10-16:
      2 |
      3 | let main (action, store : string * string) : return =
      4 |   module Foo = struct
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    File "../../test/contracts/negative/create_contract_modfv.mligo", line 7, character 35 to line 11, character 8:
      6 |   end in
      7 |   let toto : operation * address = Tezos.create_contract
      8 |     (fun (p, s : nat * string) -> (([] : operation list), Foo.store))
      9 |     (None: key_hash option)
     10 |     300tz
     11 |     "un"
     12 |   in

    Not all free variables could be inlined in Tezos.create_contract usage: gen#19. |}] ;

  run_ligo_bad [ "compile" ; "contract" ; bad_contract "create_contract_no_inline.mligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 9, characters 11-15:
      8 | let main (action, store : int * int) : return =
      9 |   let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
     10 |   let toto : operation list = [ op ] in
    :
    Warning: unused variable "addr".
    Hint: replace it by "_addr" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 8, characters 10-16:
      7 |
      8 | let main (action, store : int * int) : return =
      9 |   let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 8, characters 18-23:
      7 |
      8 | let main (action, store : int * int) : return =
      9 |   let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
    :
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 5, characters 20-21:
      4 |
      5 | let dummy_contract (p, s : nat * int) : return =
      6 |  (([] : operation list), foo)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 5, characters 23-24:
      4 |
      5 | let dummy_contract (p, s : nat * int) : return =
      6 |  (([] : operation list), foo)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 9, characters 19-89:
      8 | let main (action, store : int * int) : return =
      9 |   let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
     10 |   let toto : operation list = [ op ] in

    Not all free variables could be inlined in Tezos.create_contract usage: foo. |}] ;

  run_ligo_good [ "compile" ; "contract" ; contract "create_contract.mligo" ] ;
  [%expect{|
    File "../../test/contracts/create_contract.mligo", line 5, characters 10-11:
      4 |   let toto : operation * address = Tezos.create_contract
      5 |     (fun (p, s : nat * string) -> (([] : operation list), "one"))
      6 |     (None: key_hash option)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/create_contract.mligo", line 5, characters 13-14:
      4 |   let toto : operation * address = Tezos.create_contract
      5 |     (fun (p, s : nat * string) -> (([] : operation list), "one"))
      6 |     (None: key_hash option)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/create_contract.mligo", line 3, characters 10-16:
      2 |
      3 | let main (action, store : string * string) : return =
      4 |   let toto : operation * address = Tezos.create_contract
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    { parameter string ;
      storage string ;
      code { CDR ;
             PUSH string "un" ;
             PUSH mutez 300000000 ;
             NONE key_hash ;
             CREATE_CONTRACT
               { parameter nat ;
                 storage string ;
                 code { DROP ; PUSH string "one" ; NIL operation ; PAIR } } ;
             PAIR ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CAR ;
             CONS ;
             PAIR } } |}];

  run_ligo_good [ "compile" ; "contract" ; contract "tuples_no_annotation.religo" ] ;
  [%expect{|
    File "../../test/contracts/tuples_no_annotation.religo", line 5, characters 13-14:
      4 |
      5 | let main = ((p,storage): (parameter, storage)) => {
      6 | ([]: list (operation), (2, "2", 2n, false));
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/tuples_no_annotation.religo", line 5, characters 15-22:
      4 |
      5 | let main = ((p,storage): (parameter, storage)) => {
      6 | ([]: list (operation), (2, "2", 2n, false));
    :
    Warning: unused variable "storage".
    Hint: replace it by "_storage" to prevent this warning.

    { parameter int ;
      storage (pair (pair int string) (pair nat bool)) ;
      code { DROP ;
             PUSH bool False ;
             PUSH nat 2 ;
             PAIR ;
             PUSH string "2" ;
             PUSH int 2 ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "self_type_annotation.ligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/self_type_annotation.ligo", line 8, characters 10-23:
      7 |   block {
      8 |     const self_contract: contract(int) = Tezos.self ("%default");
      9 |   }
    :
    Warning: unused variable "self_contract".
    Hint: replace it by "_self_contract" to prevent this warning.

    File "../../test/contracts/negative/self_type_annotation.ligo", line 6, characters 21-22:
      5 |
      6 | function main (const p : parameter; const s : storage) : return is
      7 |   block {
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/negative/self_type_annotation.ligo", line 8, characters 41-64:
      7 |   block {
      8 |     const self_contract: contract(int) = Tezos.self ("%default");
      9 |   }

    Invalid type annotation.
    "contract (nat)" was given, but "contract (int)" was expected.
    Note that "Tezos.self" refers to this contract, so the parameters should be the same. |}] ;

  run_ligo_good [ "compile" ; "contract" ; contract "self_type_annotation.ligo" ] ;
  [%expect{|
    { parameter nat ;
      storage address ;
      code { DROP ; SELF %default ; ADDRESS ; NIL operation ; PAIR } } |}] ;

  run_ligo_good [ "compile" ; "contract" ; contract "self_default_with_variant_parameter.mligo" ] ;
    [%expect{|
      { parameter (or (address %one) (unit %two)) ;
        storage address ;
        code { DROP ; SELF %default ; ADDRESS ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "bad_contract.mligo" ] ;
  [%expect {|
File "../../test/contracts/negative/bad_contract.mligo", line 4, characters 10-16:
  3 |
  4 | let main (action, store : parameter * storage) : storage =
  5 |   store + 1
:
Warning: unused variable "action".
Hint: replace it by "_action" to prevent this warning.

File "../../test/contracts/negative/bad_contract.mligo", line 4, characters 10-23:
  3 |
  4 | let main (action, store : parameter * storage) : storage =
  5 |   store + 1

Invalid type for entrypoint "main".
An entrypoint must of type "parameter * storage -> operations list * storage". |}] ;

  run_ligo_bad [ "compile" ; "contract" ; bad_contract "bad_contract2.mligo" ] ;
  [%expect {|
File "../../test/contracts/negative/bad_contract2.mligo", line 5, characters 10-16:
  4 |
  5 | let main (action, store : parameter * storage) : return =
  6 |   ("bad",store + 1)
:
Warning: unused variable "action".
Hint: replace it by "_action" to prevent this warning.

File "../../test/contracts/negative/bad_contract2.mligo", line 5, characters 10-23:
  4 |
  5 | let main (action, store : parameter * storage) : return =
  6 |   ("bad",store + 1)

Invalid type for entrypoint "main".
An entrypoint must of type "parameter * storage -> operations list * storage".
We expected a list of operations but we got string |}] ;

  run_ligo_bad [ "compile" ; "contract" ; bad_contract "bad_contract3.mligo" ] ;
  [%expect {|
File "../../test/contracts/negative/bad_contract3.mligo", line 5, characters 10-16:
  4 |
  5 | let main (action, store : parameter * storage) : return =
  6 |   (([]: operation list),"bad")
:
Warning: unused variable "action".
Hint: replace it by "_action" to prevent this warning.

File "../../test/contracts/negative/bad_contract3.mligo", line 5, characters 18-23:
  4 |
  5 | let main (action, store : parameter * storage) : return =
  6 |   (([]: operation list),"bad")
:
Warning: unused variable "store".
Hint: replace it by "_store" to prevent this warning.

File "../../test/contracts/negative/bad_contract3.mligo", line 5, characters 10-23:
  4 |
  5 | let main (action, store : parameter * storage) : return =
  6 |   (([]: operation list),"bad")

Invalid type for entrypoint "main".
The storage type "int" of the function parameter must be the same as the storage type "string" of the return value. |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "self_with_entrypoint.ligo" ] ;
  [%expect {|
    File "../../test/contracts/self_with_entrypoint.ligo", line 6, characters 21-22:
      5 |
      6 | function main (const p : parameter; const s : storage) : return is
      7 |   block {
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    { parameter (or (unit %default) (int %toto)) ;
      storage nat ;
      code { CDR ;
             SELF %toto ;
             PUSH mutez 300000000 ;
             PUSH int 2 ;
             TRANSFER_TOKENS ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}] ;

  run_ligo_good [ "compile" ; "contract" ; contract "self_without_entrypoint.ligo" ] ;
  [%expect {|
    File "../../test/contracts/self_without_entrypoint.ligo", line 6, characters 21-22:
      5 |
      6 | function main (const p : parameter; const s : storage) : return is
      7 |   block {
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    { parameter int ;
      storage nat ;
      code { CDR ;
             SELF %default ;
             PUSH mutez 300000000 ;
             PUSH int 2 ;
             TRANSFER_TOKENS ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}] ;

  run_ligo_bad [ "compile" ; "contract" ; bad_contract "self_bad_entrypoint_format.ligo" ] ;
  [%expect {|
File "../../test/contracts/negative/self_bad_entrypoint_format.ligo", line 6, characters 21-22:
  5 |
  6 | function main (const p : parameter; const s : storage) : return is
  7 |   block {
:
Warning: unused variable "p".
Hint: replace it by "_p" to prevent this warning.

File "../../test/contracts/negative/self_bad_entrypoint_format.ligo", line 8, characters 52-58:
  7 |   block {
  8 |     const self_contract: contract(int) = Tezos.self("Toto") ;
  9 |     const op : operation = Tezos.transaction (2, 300tz, self_contract) ;

Invalid entrypoint "Toto". One of the following patterns is expected:
* "%bar" is expected for entrypoint "Bar"
* "%default" when no entrypoint is used. |}];

  run_ligo_bad ["compile" ; "contract"; bad_contract "nested_bigmap_1.religo"];
  [%expect {|
    File "../../test/contracts/negative/nested_bigmap_1.religo", line 1, characters 11-29:
      1 | type bar = big_map (nat, int);
      2 |

    Invalid big map nesting.
    A big map cannot be nested inside another big map. |}];

  run_ligo_bad ["compile" ; "contract"; bad_contract "nested_bigmap_2.religo"];
  [%expect {|
    File "../../test/contracts/negative/nested_bigmap_2.religo", line 2, characters 29-50:
      1 | /* this should result in an error as nested big_maps are not supported: */
      2 | type storage = big_map (nat, big_map (int, string));
      3 |

    Invalid big map nesting.
    A big map cannot be nested inside another big map. |}];

  run_ligo_bad ["compile" ; "contract"; bad_contract "nested_bigmap_3.religo"];
  [%expect {|
    File "../../test/contracts/negative/nested_bigmap_3.religo", line 1, characters 11-29:
      1 | type bar = big_map (nat, int);
      2 |

    Invalid big map nesting.
    A big map cannot be nested inside another big map. |}];

  run_ligo_bad ["compile" ; "contract"; bad_contract "nested_bigmap_4.religo"];
  [%expect {|
    File "../../test/contracts/negative/nested_bigmap_4.religo", line 2, characters 25-61:
      1 | /* this should result in an error as nested big_maps are not supported: */
      2 | type storage = map (int, big_map (nat, big_map (int, string)));
      3 |

    Invalid big map nesting.
    A big map cannot be nested inside another big map. |}];

  run_ligo_good ["print" ; "ast-imperative"; contract "letin.mligo"];
  [%expect {|
type storage = (int , int)
const main : (int , storage) -> (list (operation) , storage) =
  lambda (n : (int , storage)) : (list (operation) , storage) return
  let x : (int , int) = let x : int = 7 in
                        (ADD(x ,n.0) , ADD(n.1.0 ,n.1.1)) in
  (list[] : list (operation) , x)
const f0 = lambda (_a : string) return true
const f1 = lambda (_a : string) return true
const f2 = lambda (_a : string) return true
const letin_nesting =
  lambda (_#2 : unit) return let s = "test" in
                             let p0 = (f0)@(s) in
                             {
                                (assert)@(p0);
                                let p1 = (f1)@(s) in
                                {
                                   (assert)@(p1);
                                   let p2 = (f2)@(s) in
                                   {
                                      (assert)@(p2);
                                      s
                                   }
                                }
                             }
const letin_nesting2 =
  lambda (x : int) return let y = 2 in
                          let z = 3 in
                          ADD(ADD(x ,y) ,z)
const x =  match (+1 , (+2 , +3)) with
            | (_#3,(x,_#4)) -> x
    |}];

  run_ligo_good ["print" ; "ast-imperative"; contract "letin.religo"];
  [%expect {|
type storage = (int , int)
const main =
  lambda (n : (int , storage)) : (list (operation) , storage) return
  let x : (int , int) = let x : int = 7 in
                        (ADD(x ,n.0) , ADD(n.1.0 ,n.1.1)) in
  (list[] : list (operation) , x)
const f0 = lambda (_a : string) return true
const f1 = lambda (_a : string) return true
const f2 = lambda (_a : string) return true
const letin_nesting =
  lambda (_#2 : unit) return let s = "test" in
                             let p0 = (f0)@(s) in
                             {
                                (assert)@(p0);
                                let p1 = (f1)@(s) in
                                {
                                   (assert)@(p1);
                                   let p2 = (f2)@(s) in
                                   {
                                      (assert)@(p2);
                                      s
                                   }
                                }
                             }
const letin_nesting2 =
  lambda (x : int) return let y = 2 in
                          let z = 3 in
                          ADD(ADD(x ,y) ,z)
const x =  match (+1 , (+2 , +3)) with
            | (gen#3,(x,gen#4)) -> x
    |}];

  run_ligo_bad ["print" ; "ast-typed"; contract "existential.mligo"];
  [%expect {|
    File "../../test/contracts/existential.mligo", line 2, characters 21-22:
      1 | let a : 'a = 2
      2 | let b : _ ->'b = fun _ -> 2
      3 | let c : 'a -> 'a = fun x -> 2

    Missing a type annotation for argument "_". |}];
  run_ligo_bad ["print" ; "ast-typed"; bad_contract "missing_funarg_annotation.mligo"];
  [%expect {|
    File "../../test/contracts/negative/missing_funarg_annotation.mligo", line 5, characters 12-13:
      4 | let a ((b)) = b
      5 | let a = fun b -> b
      6 | let a = fun (b,c) -> b

    Missing a type annotation for argument "b". |}];
  run_ligo_bad ["print" ; "ast-typed"; bad_contract "missing_funarg_annotation.religo"];
  [%expect {|
File "../../test/contracts/negative/missing_funarg_annotation.religo", line 2, characters 8-9:
  1 | /* these should give a missing type annotation error */
  2 | let a = b => b
  3 | let a = (b,c) => b

Missing a type annotation for argument "b". |}];
  run_ligo_bad ["print" ; "ast-typed"; bad_contract "funarg_tuple_wrong.mligo"];
  [%expect {|
    File "../../test/contracts/negative/funarg_tuple_wrong.mligo", line 1, characters 7-14:
      1 | let a (b, c, d: int * int) = d
      2 | let a (((b, c, d)): ((((int))) * int)) = d

    Pattern not of the expected type ( int * int ) |}];
  run_ligo_bad ["print" ; "ast-typed"; bad_contract "funarg_tuple_wrong.religo"];
  [%expect {|
    Pattern (b,c,d) not of the expected type ( int * int ) |}];

  run_ligo_bad [ "compile" ; "contract" ; bad_contract "duplicate_record_field.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/duplicate_record_field.mligo", line 1, characters 9-34:
      1 | type r = { foo : int ; foo : int }
      2 |

    Duplicated field or variant name.
    Hint: Change the name. |}];

  ()

(* uncurrying example *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "uncurry_contract.mligo" ] ;
  let output = [%expect.output] in
  let lines = String.split_lines output in
  let lines = List.take lines 4 in
  let output = String.concat ~sep:"\n" lines in
  print_string output;
  [%expect {|
    { parameter unit ;
      storage unit ;
      code { LAMBDA (pair unit unit unit unit) unit { UNPAIR 4 ; DROP 4 ; UNIT } ;
             LAMBDA (pair nat nat) nat { UNPAIR ; MUL } ; |}]

(* old uncurry bugs: *)
let%expect_test _ =
  run_ligo_good [ "run" ; "interpret"; "let f (y : int) (x : int) (y : int) = (x, y) in f 1 2 3"; "-s"; "cameligo" ] ;
  [%expect {| ( 2 , 3 ) |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret"; "let f (x0 : int) (x1 : int) (x2 : int) (x3 : int) (x4 : int) (x5 : int) (x6 : int) (x7 : int) (x8 : int) (x9 : int) (x10 : int) : int list = [x0; x1; x2; x3; x4; x5; x6; x7; x8; x9; x10] in f 0 1 2 3 4 5 6 7 8 9 10" ; "-s"; "cameligo"] ;
  [%expect {|
    CONS(0 ,
         CONS(1 ,
              CONS(2 ,
                   CONS(3 ,
                        CONS(4 ,
                             CONS(5 ,
                                  CONS(6 ,
                                       CONS(7 ,
                                            CONS(8 ,
                                                 CONS(9 ,
                                                      CONS(10 , LIST_EMPTY()))))))))))) |}]

(* uncurrying w/ interpret (old bug) *)
let%expect_test _ =
  run_ligo_good [ "run" ; "interpret"; "mul 3n 4n" ; "--init-file"; contract "uncurry_contract.mligo"] ;
  [%expect {| +12 |}]

(* Edo combs example *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "edo_combs.mligo" ] ;
  [%expect {|
    File "../../test/contracts/edo_combs.mligo", line 10, characters 13-14:
      9 |
     10 | let main (p, s : param * int) : operation list * int =
     11 |   let { x = x; y = y; z = z; w = w } = p in
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    { parameter (pair (int %x) (int %y) (int %z) (int %w)) ;
      storage int ;
      code { CAR ; UNPAIR 4 ; ADD ; ADD ; ADD ; NIL operation ; PAIR } } |}]

(* warning unused variables example *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "warning_unused.mligo" ] ;
  [%expect {|
    File "../../test/contracts/warning_unused.mligo", line 11, characters 6-7:
     10 |   let x = s.x + 3 in
     11 |   let x = foo x in
     12 |   let x = bar s.x in
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    { parameter int ;
      storage (pair (int %x) (int %y)) ;
      code { CDR ;
             PUSH int 3 ;
             SWAP ;
             DUP ;
             DUG 2 ;
             CAR ;
             ADD ;
             DROP ;
             PUSH int 3 ;
             PUSH int 9 ;
             DUP 3 ;
             CAR ;
             MUL ;
             ADD ;
             SWAP ;
             CDR ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

(* warning non-duplicable variable used examples *)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "warning_duplicate.mligo" ] ;
  [%expect{|
    File "../../test/contracts/warning_duplicate.mligo", line 2, characters 2-50:
      1 | module Foo = struct
      2 |   let x : nat ticket = Tezos.create_ticket 42n 42n
      3 | end
    :
    Warning: variable "Foo.x" cannot be used more than once.

    Error(s) occurred while checking the contract:
    At (unshown) location 8, type ticket nat cannot be used here because it is not duplicable. Only duplicable types can be used with the DUP instruction and as view inputs and outputs.
    At (unshown) location 8, Ticket in unauthorized position (type error). |}]


let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "warning_duplicate2.mligo" ] ;
  [%expect{|
    File "../../test/contracts/warning_duplicate2.mligo", line 1, characters 4-5:
      1 | let x = Tezos.create_ticket 42n 42n
      2 | let x = (x, x)
    :
    Warning: variable "x" cannot be used more than once.

    Error(s) occurred while checking the contract:
    At (unshown) location 8, type ticket nat cannot be used here because it is not duplicable. Only duplicable types can be used with the DUP instruction and as view inputs and outputs.
    At (unshown) location 8, Ticket in unauthorized position (type error). |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "warning_duplicate3.mligo" ; "--protocol" ; "hangzhou" ] ;
  [%expect{|
    { parameter (pair (chest %c) (chest_key %ck)) ;
      storage int ;
      code { DROP ; PUSH int 1 ; NIL operation ; PAIR } } |}]

(* warning layout attribute on constructor *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "B 42n" ; "--init-file" ; contract "warning_layout.mligo" ] ;
  [%expect {|
    Warning: The type of this value is ambiguous: Inferred type is parameter_ok but could be of type parameter_warns.
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_layout.mligo", line 3, character 4 to line 6, character 13:
      2 |   [@layout:comb]
      3 |     B of nat
      4 |   | C of int
      5 |   | D of string
      6 |   | A of unit
      7 |

    Warning: layout attribute only applying to B, probably ignored.

    (Left 42)
  |}]

(* never test for PascaLIGO *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "never.ligo" ] ;
  [%expect {|
    { parameter (or (never %extend) (int %increment)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { SWAP ; DROP ; NEVER } { ADD } ;
             NIL operation ;
             PAIR } } |}]

(* never test for CameLIGO *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "never.mligo" ] ;
  [%expect {|
    { parameter (or (never %extend) (int %increment)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { SWAP ; DROP ; NEVER } { ADD } ;
             NIL operation ;
             PAIR } } |}]

(* never test for ReasonLIGO *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "never.religo" ] ;
  [%expect {|
    { parameter (or (never %extend) (int %increment)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { SWAP ; DROP ; NEVER } { ADD } ;
             NIL operation ;
             PAIR } } |}]

(* never test for JsLIGO *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "never.jsligo" ] ;
  [%expect {|
    File "../../test/contracts/never.jsligo", line 8, character 0 to line 15, character 1:
      7 |
      8 | let main = ([action, store] : [parameter, storage]) : [list<operation>, storage] => {
      9 |   return [
     10 |    (list([]) as list <operation>),
     11 |    (match (action, {
     12 |     Increment: (n : int) => store + n,
     13 |     Extend: (k : never) => (Tezos.never(k) as storage)}))
     14 |   ]
     15 | };

    Toplevel let declaration are silently change to const declaration.

    { parameter (or (never %extend) (int %increment)) ;
      storage int ;
      code { DUP ;
             CAR ;
             IF_LEFT { SWAP ; DROP ; NEVER } { SWAP ; CDR ; ADD } ;
             NIL operation ;
             PAIR } } |}]

(* annotations and self *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "self_annotations.mligo" ] ;
  [%expect {|
    { parameter (or (unit %foo) (unit %b)) ;
      storage unit ;
      code { DROP ;
             SELF %foo ;
             PUSH mutez 0 ;
             UNIT ;
             TRANSFER_TOKENS ;
             UNIT ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "error_self_annotations.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_self_annotations.mligo", line 6, characters 22-26:
      5 | let main (_,_ : param * unit) : operation list * unit =
      6 |   let c = (Tezos.self("%a") : unit contract) in
      7 |   let op = Tezos.transaction () 0mutez c in

    Invalid entrypoint value.
    The entrypoint value does not match a constructor of the contract parameter. |}]

(* entrypoint check *)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "bad_get_entrypoint.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/bad_get_entrypoint.mligo", line 3, characters 11-16:
      2 |   let v = (Tezos.get_entrypoint_opt
      3 |            "foo"
      4 |            ("tz1fakefakefakefakefakefakefakcphLA5" : address) : unit contract option) in

    Invalid entrypoint "foo". One of the following patterns is expected:
    * "%bar" is expected for entrypoint "Bar"
    * "%default" when no entrypoint is used. |}]

(* using test in compilation *)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "compile_test.mligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/compile_test.mligo", line 21, characters 14-30:
     20 |   let (taddr, _, _) = Test.originate main  initial_storage 0tez in
     21 |   let contr = Test.to_contract(taddr) in
     22 |   let _r = Test.transfer_to_contract_exn contr (Increment (32)) 1tez  in

    Can't infer the type of this value, please add a type annotation. |}]

(* remove unused declarations *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "remove_unused_module.mligo" ] ;
  [%expect{|
    { parameter unit ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "remove_unused_toptup.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage int ;
      code { CDR ; PUSH nat 2 ; PUSH nat 1 ; DIG 2 ; ADD ; ADD ; NIL operation ; PAIR } } |}]

(* wrong annotation in Bytes.unpack *)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; bad_contract "bad_annotation_unpack.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/bad_annotation_unpack.mligo", line 1, characters 9-21:
      1 | let x = (Bytes.unpack (Bytes.pack "hello") : string)

    Invalid type(s).
    Expected: "string", but got: "option (a)". |}]

(* check annotations' capitalization *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "annotation_cases.mligo" ; "-e" ; "main1" ] ;
  [%expect {|
    { parameter (pair (pair (nat %AAA) (nat %fooB)) (nat %cCC)) ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "annotation_cases.mligo" ; "-e" ; "main2" ] ;
  [%expect {|
    { parameter (or (or (nat %AAA) (nat %fooB)) (nat %cCC)) ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

(* remove recursion *)
let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "remove_recursion.mligo" ] ;
  [%expect {|
    const f =
      lambda (n : int) return let f : int -> int = rec (f:int -> int => lambda (n : int) return let gen#2[@var] = EQ(n ,
      0) in  match gen#2 with
              | False unit_proj#3 ->
                (f)@(C_POLYMORPHIC_SUB(n ,
                1))
              | True unit_proj#4 ->
                1 ) in (f)@(4)
    const g =
      rec (g:int -> int -> int -> int => lambda (f : int -> int) return (g)@(let h : int -> int = rec (h:int -> int => lambda (n : int) return let gen#5[@var] = EQ(n ,
      0) in  match gen#5 with
              | False unit_proj#6 ->
                (h)@(C_POLYMORPHIC_SUB(n ,
                1))
              | True unit_proj#7 ->
                1 ) in h) ) |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "reuse_variable_name_top.jsligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/reuse_variable_name_top.jsligo", line 2, characters 0-14:
      1 | let dog = 1;
      2 | let dog = true;

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/negative/reuse_variable_name_top.jsligo", line 1, characters 0-11:
      1 | let dog = 1;
      2 | let dog = true;

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/negative/reuse_variable_name_top.jsligo", line 2, characters 10-14:
      1 | let dog = 1;
      2 | let dog = true;

    Cannot redeclare block-scoped variable. |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "reuse_variable_name_block.jsligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/reuse_variable_name_block.jsligo", line 1, character 0 to line 5, character 1:
      1 | let foo = (): int => {
      2 |     let x = 2;
      3 |     let x = 2;
      4 |     return x;
      5 | }

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/negative/reuse_variable_name_block.jsligo", line 3, characters 8-13:
      2 |     let x = 2;
      3 |     let x = 2;
      4 |     return x;

    Cannot redeclare block-scoped variable. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "evaluate-call"; contract "assert.mligo"; "(false, ())"; "-e"; "with_error"];
  [%expect {| failwith("my custom error") |}]

let%expect_test _ =
  run_ligo_good [ "run"; "evaluate-call"; contract "assert.mligo"; "(None: unit option)"; "-e"; "some_with_error"];
  [%expect {| failwith("my custom error") |}]

let%expect_test _ =
  run_ligo_good [ "run"; "evaluate-call"; contract "assert.mligo"; "(Some (): unit option)"; "-e"; "none_with_error"];
  [%expect {| failwith("my custom error") |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "attributes.jsligo" ] ;
  [%expect {|
    const x = 1[@inline][@private]
    const foo = lambda (a : int) return let test[@var] = C_POLYMORPHIC_ADD(2 ,
      a)[@inline] in test[@inline][@private]
    const y = 1[@private]
    const bar =
      lambda (b : int) return let test[@var] = lambda (z : int) return C_POLYMORPHIC_ADD(C_POLYMORPHIC_ADD(2 ,
      b) , z)[@inline] in (test)@(b)[@private]
    const check = 4[@private] |}]

(* literal type "casting" inside modules *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "literal_type_cast.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage timestamp ;
      code { DROP ; PUSH timestamp 0 ; NIL operation ; PAIR } }
  |}]

(* JsLIGO export testing *)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "modules_export_type.jsligo" ] ;
    [%expect {|
      File "../../test/contracts/negative/modules_export_type.jsligo", line 5, characters 9-16:
        4 |
        5 | type a = Bar.foo

      Type "foo" not found. |}];
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "modules_export_const.jsligo" ] ;
    [%expect {|
      File "../../test/contracts/negative/modules_export_const.jsligo", line 5, characters 0-15:
        4 |
        5 | let a = Bar.foo;

      Toplevel let declaration are silently change to const declaration.

      File "../../test/contracts/negative/modules_export_const.jsligo", line 5, characters 8-15:
        4 |
        5 | let a = Bar.foo;

      Variable "foo" not found. |}];
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "modules_export_namespace.jsligo" ] ;
    [%expect {|
      File "../../test/contracts/negative/modules_export_namespace.jsligo", line 7, characters 17-20:
        6 |
        7 | import Foo = Bar.Foo

      Module "Foo" not found. |}]

(* Test compile contract with Big_map.get_and_update for Hangzhou *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "ticket_wallet.mligo" ; "--protocol"; "hangzhou" ] ;
  [%expect {|
{ parameter
    (or (ticket %receive unit)
        (pair %send (contract %destination (ticket unit)) (nat %amount) (address %ticketer))) ;
  storage (pair (address %manager) (big_map %tickets address (ticket unit))) ;
  code { PUSH mutez 0 ;
         AMOUNT ;
         COMPARE ;
         EQ ;
         IF {} { PUSH string "failed assertion" ; FAILWITH } ;
         UNPAIR ;
         SWAP ;
         UNPAIR ;
         DIG 2 ;
         IF_LEFT
           { READ_TICKET ;
             CAR ;
             DIG 3 ;
             NONE (ticket unit) ;
             DUP 3 ;
             GET_AND_UPDATE ;
             IF_NONE
               { DIG 2 }
               { DIG 3 ;
                 PAIR ;
                 JOIN_TICKETS ;
                 IF_NONE { PUSH string "impossible?" ; FAILWITH } {} } ;
             SOME ;
             DIG 2 ;
             GET_AND_UPDATE ;
             DROP ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR }
           { SWAP ;
             DUP ;
             DUG 2 ;
             SENDER ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             DIG 2 ;
             NONE (ticket unit) ;
             DUP 3 ;
             GET 4 ;
             GET_AND_UPDATE ;
             IF_NONE
               { DROP 3 ; PUSH string "no tickets" ; FAILWITH }
               { READ_TICKET ;
                 CDR ;
                 CDR ;
                 DUP 4 ;
                 GET 3 ;
                 DUP ;
                 DIG 2 ;
                 SUB ;
                 ISNAT ;
                 IF_NONE { PUSH string "not enough tickets" ; FAILWITH } {} ;
                 SWAP ;
                 PAIR ;
                 SWAP ;
                 SPLIT_TICKET ;
                 IF_NONE
                   { DROP 3 ; PUSH string "impossible?" ; FAILWITH }
                   { UNPAIR ;
                     DUG 2 ;
                     SOME ;
                     DUP 4 ;
                     GET 4 ;
                     GET_AND_UPDATE ;
                     DROP ;
                     DIG 2 ;
                     CAR ;
                     PUSH mutez 0 ;
                     DIG 3 ;
                     TRANSFER_TOKENS ;
                     SWAP ;
                     DIG 2 ;
                     PAIR ;
                     NIL operation ;
                     DIG 2 ;
                     CONS ;
                     PAIR } } } } } |} ]

(* source location comments *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "noop.mligo";
                  "--michelson-comments"; "location";
                  "--michelson-comments"; "env";
                ];
  [%expect {|
    { parameter unit ;
      storage unit ;
      code { { /* _ */ } ;
             CDR ;
             { /* _ */ } ;
             LAMBDA unit unit { { /* x */ } } ;
             { /* f, _ */ } ;
             DUP ;
             DUG 2 ;
             SWAP ;
             EXEC ;
             { /* s2, f */ } ;
             SWAP ;
             DUP ;
             DUG 2 ;
             SWAP ;
             EXEC ;
             { /* s3, f */ } ;
             EXEC ;
             { /* s */ } ;
             NIL operation
                 /* File "../../test/contracts/noop.mligo", line 6, characters 3-24 */
             /* File "../../test/contracts/noop.mligo", line 6, characters 3-24 */ ;
             PAIR
             /* File "../../test/contracts/noop.mligo", line 6, characters 3-27 */ } } |}]

(* JSON source location comments *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "noop.mligo";
                  "--michelson-format"; "json";
                  "--michelson-comments"; "location";
                  "--michelson-comments"; "env" ];
  [%expect{|
    { "types":
        [ { "type_content":
              [ "t_constant",
                { "language": "Michelson", "injection": "unit",
                  "parameters": [] } ], "type_meta": [ "None", null ],
            "location": [ "Virtual", "generated" ],
            "orig_var": [ "None", null ] },
          { "type_content":
              [ "t_arrow",
                { "type1":
                    { "type_content":
                        [ "t_constant",
                          { "language": "Michelson", "injection": "unit",
                            "parameters": [] } ], "type_meta": [ "None", null ],
                      "location": [ "Virtual", "generated" ],
                      "orig_var": [ "None", null ] },
                  "type2":
                    { "type_content":
                        [ "t_constant",
                          { "language": "Michelson", "injection": "unit",
                            "parameters": [] } ], "type_meta": [ "None", null ],
                      "location": [ "Virtual", "generated" ],
                      "orig_var": [ "None", null ] } } ],
            "type_meta": [ "None", null ],
            "location": [ "Virtual", "generated" ],
            "orig_var": [ "None", null ] } ],
      "michelson":
        { "expression":
            [ { "prim": "parameter", "args": [ { "prim": "unit" } ] },
              { "prim": "storage", "args": [ { "prim": "unit" } ] },
              { "prim": "code",
                "args":
                  [ [ [], { "prim": "CDR" }, [],
                      { "prim": "LAMBDA",
                        "args":
                          [ { "prim": "unit" }, { "prim": "unit" }, [ [] ] ] },
                      [], { "prim": "DUP" },
                      { "prim": "DUG", "args": [ { "int": "2" } ] },
                      { "prim": "SWAP" }, { "prim": "EXEC" }, [],
                      { "prim": "SWAP" }, { "prim": "DUP" },
                      { "prim": "DUG", "args": [ { "int": "2" } ] },
                      { "prim": "SWAP" }, { "prim": "EXEC" }, [],
                      { "prim": "EXEC" }, [],
                      { "prim": "NIL", "args": [ { "prim": "operation" } ] },
                      { "prim": "PAIR" } ] ] } ],
          "locations":
            [ {}, {}, {}, {}, {}, {}, {}, { "environment": [ null ] }, {},
              { "environment": [ { "source_type": "0" } ] }, {}, {}, {}, {},
              { "environment": [ { "name": "x", "source_type": "0" } ] },
              { "environment":
                  [ { "name": "f", "source_type": "1" }, { "source_type": "0" } ] },
              {}, {}, {}, {}, {},
              { "environment":
                  [ { "name": "s2", "source_type": "0" },
                    { "name": "f", "source_type": "1" } ] }, {}, {}, {}, {}, {},
              {},
              { "environment":
                  [ { "name": "s3", "source_type": "0" },
                    { "name": "f", "source_type": "1" } ] }, {},
              { "environment": [ { "name": "s", "source_type": "0" } ] },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "3" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "24" } } },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "3" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "24" } } },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "3" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "27" } } } ] } } |}]

(* Check that decl_pos is not taken into account when "inferring" about tuples (including long tuples) *)
let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "tuple_decl_pos.mligo" ] ;
  [%expect {|
const c =
  lambda (gen#5 : unit) return CREATE_CONTRACT(lambda (gen#2 : ( unit * unit )) return
   match gen#2 with
    | ( _#4 , _#3 ) ->
    ( LIST_EMPTY() , unit ) ,
  None(unit) , 0mutez , unit)
const foo =
  let gen#8[@var] = (c)@(unit) in  match gen#8 with
                                    | ( _a , _b ) ->
                                    unit
const c =
  lambda (gen#6 : unit) return ( 1 , "1" , +1 , 2 , "2" , +2 , 3 , "3" , +3 , 4 , "4" )
const foo =
  let gen#10[@var] = (c)@(unit) in  match gen#10 with
                                     | ( _i1 , _s1 , _n1 , _i2 , _s2 , _n2 , _i3 , _s3 , _n3 , _i4 , _s4 ) ->
                                     unit |} ]

let%expect_test _ =
  run_ligo_good [ "compile" ; "storage" ; contract "module_contract_complex.mligo" ; "{ number = 999 ; previous_action = Reset }" ] ;
  [%expect{| (Pair 999 (Left (Right Unit))) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "parameter" ; contract "module_contract_complex.mligo" ; "Add 999" ] ;
  [%expect{| (Left (Left 999)) |}]

(* Global constants *)

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "global_constant.mligo" ; "--disable-michelson-typechecking" ] ;
  [%expect {|
    { parameter unit ;
      storage int ;
      code { CDR ;
             constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2" ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "global_constant.mligo" ; "--protocol" ; "hangzhou" ; "--constants" ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }" ] ;
  [%expect {|
    { parameter unit ;
      storage int ;
      code { CDR ;
             constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2" ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "parameter" ; contract "global_constant.mligo" ; "()" ; "--protocol" ; "hangzhou" ; "--constants" ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }" ] ;
  [%expect {|
    Unit |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "storage" ; contract "global_constant.mligo" ; "v" ; "--protocol" ; "hangzhou" ; "--constants" ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }" ] ;
  [%expect {|
    128 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "storage" ; contract "global_constant.mligo" ; "42" ; "--protocol" ; "hangzhou" ; "--constants" ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "storage" ; contract "global_constant.mligo" ; "42" ; "--protocol" ; "hangzhou" ; "--file-constants" ; contract_resource "const.json" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "v" ; "--init-file" ; contract "global_constant.mligo" ; "--protocol" ; "hangzhou" ; "--constants" ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }" ] ;
  [%expect {|
    128 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "v" ; "--init-file" ; contract "global_constant.mligo" ; "--file-constants" ; contract_resource "const.json" ] ;
  [%expect {|
    128 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "storage" ; contract "global_constant_lambda.mligo" ; "s" ; "--constants" ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }" ] ;
  [%expect {|
    (Pair 1 { PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "storage" ; contract "global_constant_lambda.mligo" ; "s" ; "--file-constants" ; contract_resource "const.json" ] ;
  [%expect {|
    (Pair 1 { PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "constant" ; "cameligo" ; "fun (x : int) -> if x > 3 then x * 2 else x * String.length \"fja\" + 1" ; "--protocol" ; "hangzhou" ] ;
  [%expect {|
    Michelson constant as JSON string:
    "{ PUSH int 3 ;\n  SWAP ;\n  DUP ;\n  DUG 2 ;\n  COMPARE ;\n  GT ;\n  IF { PUSH int 2 ; SWAP ; MUL }\n     { PUSH int 1 ; PUSH string \"fja\" ; SIZE ; DIG 2 ; MUL ; ADD } }"
    This string can be passed in `--constants` argument when compiling a contract.

    Remember to register it in the network, e.g.:
    > tezos-client register global constant "{ PUSH int 3 ;
      SWAP ;
      DUP ;
      DUG 2 ;
      COMPARE ;
      GT ;
      IF { PUSH int 2 ; SWAP ; MUL }
         { PUSH int 1 ; PUSH string \"fja\" ; SIZE ; DIG 2 ; MUL ; ADD } }" from bootstrap1

    Constant hash:
    expruVivewmRtHKQn7h986tVPUzB65Z8w7QM1WP1X6DxcVWNs85USK |}]

(* Test pairing_check and bls12_381_g1/g2/fr literals *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "test" ; "--init-file" ; contract "pairing_check.mligo" ] ;
  [%expect{| Unit |}]

(* Test decompilation of bls12_381_g1/g2/fr literals *)
let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(alpha, beta, input_x)" ; "--init-file" ; contract "pairing_check.mligo" ] ;
  [%expect{|
    ( bls12_381_g1 0x024142bc89bf29017a38d0ee97711098639aa0bbc5b54b5104cc88b1c0fd09330fb8341e3da91e7a50f0da5c988517db0f52df51f745392ecdd3ffbb50f8a25fcdec6f48886b650de26821e244cb8ab69d49722d290a420ce1284b909d3e15a0 ,
      bls12_381_g2 0x0050b3ab4877c99ce7f180e879d91eb4df24b1e20ed88f1fdde42f91dfe0e7e451aa35d1457dd15ab507fc8f2b3180550ca7b4ea9b67810e346456c35060c8d542f37ee5fe2b1461e2f02fefac55a9863e94cab5c16befad3b866a42ee20835b1351f3f9c20a05586c1d647d756efb5c575d7ab23fbf5b3e1a6ffe024633a63a668a01fcab440866035ea2c0d4bfe30a1242f67119650e2aa605289ade2684287192382d6a01d7865fcd9e1507264a80f387b6441e37438c888159827a4efa67 ,
      bls12_381_fr 0xe406000000000000000000000000000000000000000000000000000000000000 ) |}]

(* Example contracts from getting-started *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "increment.mligo" ] ;
  [%expect{|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "increment.ligo" ] ;
  [%expect{|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "increment.religo" ] ;
  [%expect{|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "increment.jsligo" ] ;
  [%expect{|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}]

(* Test compiling parameter in a file which uses test primitives *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "parameter" ; contract "increment_with_test.mligo" ; "z.1" ] ;
  [%expect{| (Left (Right 32)) |}]

(* Test compiling storage in a file which uses test primitives *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "storage" ; contract "increment_with_test.mligo" ; "z.0 + 10" ] ;
  [%expect{| 42 |}]

(* Test compiling expression with curried recursive function *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "foo 2 \"titi\"" ; "--init-file" ; contract "recursion_uncurry.mligo" ] ;
  [%expect{|
    "tititotototo" |}]

(* Test compiling contract with curried recursive function *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "recursion_uncurry.mligo" ] ;
  [%expect{|
    { parameter int ;
      storage string ;
      code { LEFT string ;
             LOOP_LEFT
               { UNPAIR ;
                 PUSH int 0 ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 COMPARE ;
                 EQ ;
                 IF { DROP ; RIGHT (pair int string) }
                    { PUSH string "toto" ;
                      DIG 2 ;
                      CONCAT ;
                      PUSH int 1 ;
                      DIG 2 ;
                      SUB ;
                      PAIR ;
                      LEFT string } } ;
             NIL operation ;
             PAIR } } |}]

(* voting power *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "voting.mligo" ] ;
  [%expect{|
{ parameter key ;
  storage (pair nat nat) ;
  code { CAR ;
         HASH_KEY ;
         VOTING_POWER ;
         TOTAL_VOTING_POWER ;
         SWAP ;
         PAIR ;
         NIL operation ;
         PAIR } } |}]

(* check get contract with error typing *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "pascaligo" ; "cbo" ; "--init-file" ; contract "get_contract_with_error.ligo" ] ;
  [%expect{|
{ PUSH string "contract not found" ;
  SENDER ;
  CONTRACT unit ;
  IF_NONE { FAILWITH } { SWAP ; DROP } ;
  SWAP ;
  NIL operation ;
  DIG 2 ;
  PUSH mutez 0 ;
  UNIT ;
  TRANSFER_TOKENS ;
  CONS ;
  PAIR } |}]

(* some check about the warnings of the E_constructor cases *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "warning_ambiguous_ctor.mligo" ] ;
  [%expect{|
File "../../test/contracts/warning_ambiguous_ctor.mligo", line 9, characters 61-64:
  8 | (* here we expect a warning because both A constructor have the same parameter type *)
  9 | let main = fun (() , (_: union_b)) -> ([]: operation list) , A 1

Warning: The type of this value is ambiguous: Inferred type is union_b but could be of type union_a.
Hint: You might want to add a type annotation.

{ parameter unit ;
  storage (or (int %a) (nat %b)) ;
  code { DROP ; PUSH int 1 ; LEFT nat ; NIL operation ; PAIR } } |}];

  run_ligo_good [ "compile" ; "contract" ; contract "not_ambiguous_ctor.mligo" ] ;
  [%expect{|
{ parameter unit ;
  storage (or (nat %a) (nat %b)) ;
  code { DROP ; PUSH nat 1 ; LEFT nat ; NIL operation ; PAIR } } |}]

(* extend built-in modules *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "pascaligo" ; "y" ; "--init-file" ; contract "extend_builtin.ligo" ] ;
  [%expect{|
44 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "y" ; "--init-file" ; contract "extend_builtin.mligo" ] ;
  [%expect{|
44 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "jsligo" ; "y" ; "--init-file" ; contract "extend_builtin.jsligo" ] ;
  [%expect{|
File "../../test/contracts/extend_builtin.jsligo", line 6, characters 0-24:
  5 |
  6 | let y = Tezos.f(Tezos.x);

Toplevel let declaration are silently change to const declaration.

44 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "y" ; "--init-file" ; contract "extend_builtin.religo" ] ;
  [%expect{|
44 |}]

(* check compiling many (more than 10) views *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "views_many.ligo" ] ;
  [%expect{|
{ parameter unit ;
  storage nat ;
  code { CDR ; NIL operation ; PAIR } ;
  view "view_11" unit int { CDR ; PUSH int 11 ; ADD } ;
  view "view_10" unit int { CDR ; PUSH int 10 ; ADD } ;
  view "view_9" unit int { CDR ; PUSH int 9 ; ADD } ;
  view "view_8" unit int { CDR ; PUSH int 8 ; ADD } ;
  view "view_7" unit int { CDR ; PUSH int 7 ; ADD } ;
  view "view_6" unit int { CDR ; PUSH int 6 ; ADD } ;
  view "view_5" unit int { CDR ; PUSH int 5 ; ADD } ;
  view "view_4" unit int { CDR ; PUSH int 4 ; ADD } ;
  view "view_3" unit int { CDR ; PUSH int 3 ; ADD } ;
  view "view_2" unit int { CDR ; PUSH int 2 ; ADD } ;
  view "view_1" unit int { CDR ; PUSH int 1 ; ADD } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "call_view_impure.mligo" ] ;
  [%expect{|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH address "tz1fakefakefakefakefakefakefakcphLA5" ;
             SENDER ;
             VIEW "foo" unit ;
             IF_NONE { UNIT } {} ;
             NIL operation ;
             PAIR } } |}]
