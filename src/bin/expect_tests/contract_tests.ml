open Cli_expect

let contract = test
let contract_resource name = test ("res/" ^ name)
let bad_contract = bad_test

(* avoid pretty printing *)
let () = Unix.putenv ~key:"TERM" ~data:"dumb"

let%expect_test _ =
  run_ligo_good [ "info" ; "measure-contract" ; contract "coase.ligo" ] ;
  [%expect{|
    1107 bytes |}] ;

  run_ligo_good [ "info" ; "measure-contract" ; contract "multisig.ligo" ] ;
  [%expect{|
    577 bytes |}] ;

  run_ligo_good [ "info" ; "measure-contract" ; contract "multisig-v2.ligo" ] ;
  [%expect{|
    1589 bytes |}] ;

  run_ligo_good [ "info" ; "measure-contract" ; contract "vote.mligo" ] ;
  [%expect{|
    420 bytes |}] ;

  run_ligo_good [ "compile" ; "parameter" ; contract "coase.ligo" ; "Buy_single (record [ card_to_buy = 1n ])" ] ;
  [%expect{|
    (Left (Left 1)) |}] ;

  run_ligo_good [ "compile" ; "storage" ; contract "coase.ligo" ; "record [ cards = (map [] : cards) ; card_patterns = (map [] : card_patterns) ; next_id = 3n ]" ] ;
  [%expect{|
    (Pair (Pair {} {}) 3) |}] ;

  run_ligo_bad [ "compile" ; "storage" ; contract "coase.ligo" ; "Buy_single (record [ card_to_buy = 1n ])" ] ;
  [%expect{|
    Invalid command line argument.
    The provided storage does not have the correct type for the contract.
    File "../../test/contracts/coase.ligo", line 122, character 0 to line 127, character 3:
    121 |
    122 | function main (const action : parameter; const s : storage) : return is
    123 |   case action of [
    124 |     Buy_single (bs)      -> buy_single (bs, s)
    125 |   | Sell_single (as)     -> sell_single (as, s)
    126 |   | Transfer_single (at) -> transfer_single (at, s)
    127 |   ]

    Invalid type(s).
    Expected: "storage", but got: "parameter". |}] ;

  run_ligo_bad [ "compile" ; "parameter" ; contract "coase.ligo" ; "record [ cards = (map [] : cards) ; card_patterns = (map [] : card_patterns) ; next_id = 3n ]" ] ;
  [%expect{|
    Invalid command line argument.
    The provided parameter does not have the correct type for the given entrypoint.
    File "../../test/contracts/coase.ligo", line 122, character 0 to line 127, character 3:
    121 |
    122 | function main (const action : parameter; const s : storage) : return is
    123 |   case action of [
    124 |     Buy_single (bs)      -> buy_single (bs, s)
    125 |   | Sell_single (as)     -> sell_single (as, s)
    126 |   | Transfer_single (at) -> transfer_single (at, s)
    127 |   ]

    Invalid type(s).
    Expected: "parameter", but got: "storage". |}] ;

  ()

let%expect_test _  =
  run_ligo_good [ "compile" ; "storage" ; contract "timestamp.ligo" ; "(Tezos.get_now ())" ; "--now" ; "2042-01-01T00:00:00Z" ] ;
  [%expect {|
    File "../../test/contracts/timestamp.ligo", line 3, characters 21-22:
      2 |
      3 | function main (const p : unit; const s : storage_) :
      4 |   list (operation) * storage_ is ((nil: list (operation)), Tezos.get_now())
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/timestamp.ligo", line 3, characters 37-38:
      2 |
      3 | function main (const p : unit; const s : storage_) :
      4 |   list (operation) * storage_ is ((nil: list (operation)), Tezos.get_now())
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
                   { DUP 2 ;
                     CAR ;
                     CAR ;
                     DUP 2 ;
                     GET ;
                     IF_NONE { PUSH string "buy_single: No card pattern." ; FAILWITH } {} ;
                     PUSH nat 1 ;
                     DUP 2 ;
                     CDR ;
                     ADD ;
                     DUP 2 ;
                     CAR ;
                     MUL ;
                     AMOUNT ;
                     SWAP ;
                     COMPARE ;
                     GT ;
                     IF { PUSH string "Not enough money" ; FAILWITH } {} ;
                     PUSH nat 1 ;
                     DUP 2 ;
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
                     DUP 2 ;
                     CDR ;
                     SWAP ;
                     DIG 2 ;
                     CAR ;
                     CAR ;
                     PAIR ;
                     PAIR ;
                     PUSH nat 1 ;
                     DUP 2 ;
                     CDR ;
                     ADD ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     NIL operation }
                   { DUP 2 ;
                     CAR ;
                     CDR ;
                     DUP 2 ;
                     GET ;
                     IF_NONE { PUSH string "sell_single: No card." ; FAILWITH } {} ;
                     SENDER ;
                     DUP 2 ;
                     CAR ;
                     COMPARE ;
                     NEQ ;
                     IF { PUSH string "This card doesn't belong to you" ; FAILWITH } {} ;
                     DUP 3 ;
                     CAR ;
                     CAR ;
                     DUP 2 ;
                     CDR ;
                     GET ;
                     IF_NONE { PUSH string "sell_single: No card pattern." ; FAILWITH } {} ;
                     PUSH nat 1 ;
                     DUP 2 ;
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
                     DUP 2 ;
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
                     DUP 2 ;
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
                     CONS } }
               { DUP 2 ;
                 CAR ;
                 CDR ;
                 DUP ;
                 DUP 3 ;
                 CAR ;
                 GET ;
                 IF_NONE { PUSH string "transfer_single: No card." ; FAILWITH } {} ;
                 SENDER ;
                 DUP 2 ;
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
                 NIL operation } ;
             PAIR } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "multisig.ligo" ] ;
  [%expect{|
    { parameter
        (pair (pair (nat %counter) (lambda %message unit (list operation)))
              (list %signatures (pair key_hash signature))) ;
      storage (pair (pair (list %auth key) (nat %counter)) (string %id) (nat %threshold)) ;
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
                  PACK ;
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
                                  DIG 4 ;
                                  CDR ;
                                  DIG 2 ;
                                  CHECK_SIGNATURE ;
                                  IF { PUSH nat 1 ; DIG 2 ; ADD ; UNIT ; SWAP }
                                     { PUSH string "Invalid signature" ; FAILWITH } }
                                { DIG 3 ; DROP 2 ; UNIT ; DIG 2 } }
                           { DIG 2 ; DROP ; UNIT ; DIG 2 } ;
                         DIG 2 ;
                         PAIR ;
                         PAIR } ;
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
                     { DUP 2 ;
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
                       SWAP } } ;
             SWAP ;
             DROP ;
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
                    (nat %max_proposal)
                    (map %message_store bytes (set address)))
              (pair (map %proposal_counters address nat) (bytes %state_hash))
              (nat %threshold)) ;
      code { UNPAIR ;
             IF_LEFT
               { IF_LEFT
                   { DROP ; NIL operation }
                   { DUP 2 ;
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
                     DUP 2 ;
                     SIZE ;
                     COMPARE ;
                     GT ;
                     IF { PUSH string "Message size exceed maximum limit" ; FAILWITH } {} ;
                     DUP 3 ;
                     CAR ;
                     CDR ;
                     CDR ;
                     DUP 2 ;
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
                         SENDER ;
                         SWAP ;
                         SOME ;
                         SWAP ;
                         UPDATE ;
                         PAIR ;
                         PAIR ;
                         DIG 3 ;
                         CAR ;
                         PAIR ;
                         EMPTY_SET address ;
                         SENDER ;
                         PUSH bool True ;
                         SWAP ;
                         UPDATE ;
                         UNIT ;
                         DUG 2 }
                       { DUP ;
                         SENDER ;
                         MEM ;
                         IF { UNIT ; DIG 4 }
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
                              SENDER ;
                              SWAP ;
                              SOME ;
                              SWAP ;
                              UPDATE ;
                              PAIR ;
                              PAIR ;
                              DIG 4 ;
                              CAR ;
                              PAIR ;
                              UNIT ;
                              SWAP } ;
                         SWAP ;
                         DROP ;
                         SWAP ;
                         SENDER ;
                         PAIR ;
                         UNIT ;
                         DUG 2 ;
                         UNPAIR ;
                         PUSH bool True ;
                         SWAP ;
                         UPDATE } ;
                     PAIR ;
                     SWAP ;
                     DROP ;
                     UNPAIR ;
                     DUP 2 ;
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
                     DUP 2 ;
                     CDR ;
                     CDR ;
                     DUP 2 ;
                     SIZE ;
                     COMPARE ;
                     GE ;
                     IF { DUP 2 ;
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
                          DUP 2 ;
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
                          DUP 2 ;
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
                                 DUP 2 ;
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
                                      SWAP }
                                    { DROP 2 ; UNIT ; DIG 2 } ;
                                 DIG 2 ;
                                 PAIR ;
                                 PAIR } ;
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
                     CAR ;
                     UNPAIR } }
               { PACK ;
                 DUP 2 ;
                 CAR ;
                 CDR ;
                 CDR ;
                 DUP 2 ;
                 GET ;
                 IF_NONE
                   { DROP ; UNIT ; SWAP }
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
                          SENDER ;
                          SWAP ;
                          SOME ;
                          SWAP ;
                          UPDATE ;
                          PAIR ;
                          PAIR ;
                          DIG 3 ;
                          CAR ;
                          PAIR ;
                          UNIT ;
                          SWAP }
                        { UNIT ; DIG 3 } ;
                     SWAP ;
                     DROP ;
                     PUSH nat 0 ;
                     DUP 3 ;
                     SIZE ;
                     COMPARE ;
                     EQ ;
                     IF { SWAP ;
                          DROP ;
                          UNIT ;
                          DUP 2 ;
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
                          CAR }
                        { UNIT ;
                          DUP 2 ;
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
                          CAR } ;
                     PAIR ;
                     PAIR } ;
                 SWAP ;
                 DROP ;
                 NIL operation } ;
             PAIR } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "vote.mligo" ] ;
  [%expect {|
{ parameter
    (or (pair %reset (pair (timestamp %finish_time) (timestamp %start_time)) (string %title))
        (or %vote (unit %nay) (unit %yea))) ;
  storage
    (pair (pair (pair (timestamp %finish_time) (nat %nay))
                (timestamp %start_time)
                (string %title))
          (set %voters address)
          (nat %yea)) ;
  code { UNPAIR ;
         IF_LEFT
           { SWAP ;
             DROP ;
             PUSH nat 0 ;
             EMPTY_SET address ;
             PAIR ;
             DUP 2 ;
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
             PAIR }
           { SENDER ;
             SWAP ;
             IF_LEFT
               { DROP ;
                 DUP 2 ;
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
                 CAR } ;
             PAIR ;
             DUP ;
             CDR ;
             CDR ;
             DUP 2 ;
             CDR ;
             CAR ;
             DIG 3 ;
             PUSH bool True ;
             SWAP ;
             UPDATE ;
             PAIR ;
             SWAP ;
             CAR } ;
         PAIR ;
         NIL operation ;
         PAIR } } |}]

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
           { DUP 2 ;
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
 30 |         assert (ticketer = Tezos.get_self_address ());
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
             NIL operation }
           { DUP 2 ;
             SENDER ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             DUP ;
             CDR ;
             UNIT ;
             TICKET ;
             SWAP ;
             CAR ;
             PUSH mutez 0 ;
             DIG 2 ;
             TRANSFER_TOKENS ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CONS } ;
         PAIR } } |} ]

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
    File "../../test/contracts/amount_lambda.mligo", line 4, characters 7-8:
      3 |   let amt : tez = Tezos.get_amount () in
      4 |   fun (x : unit) -> amt
      5 |
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 2, characters 8-9:
      1 | (* should return a constant function *)
      2 | let f1 (x : unit) : unit -> tez =
      3 |   let amt : tez = Tezos.get_amount () in
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 8, characters 7-8:
      7 | let f2 (x : unit) : unit -> tez =
      8 |   fun (x : unit) -> Tezos.get_amount ()
      9 |
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 7, characters 8-9:
      6 | (* should return an impure function *)
      7 | let f2 (x : unit) : unit -> tez =
      8 |   fun (x : unit) -> Tezos.get_amount ()
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 10, characters 12-13:
      9 |
     10 | let main (b,s : bool * (unit -> tez)) : operation list * (unit -> tez) =
     11 |   (([] : operation list), (if b then f1 () else f2 ()))
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    { parameter bool ;
      storage (lambda unit mutez) ;
      code { CAR ;
             IF { AMOUNT ;
                  LAMBDA (pair mutez unit) mutez { CAR } ;
                  DUP 2 ;
                  APPLY ;
                  SWAP ;
                  DROP }
                { LAMBDA unit mutez { DROP ; AMOUNT } } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "sequence.mligo" ; ];
  [%expect {xxx|
    const y : unit -> nat =
      lambda (_#2unit)nat return let _x : nat = +1 in
                                 let ()#5 : unit = let _x : nat = +2 in unit in
                                 let ()#4 : unit = let _x : nat = +23 in unit in
                                 let ()#3 : unit = let _x : nat = +42 in unit in
                                 _x |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "bad_address_format.religo" ; "--werror" ] ;
  [%expect{|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Warning: Error(s) occurred while type checking the produced michelson contract:
    Ill typed contract:
      1: { parameter int ;
      2:   storage address ;
      3:   code { DROP /* [] */ ; PUSH address "KT1badaddr" ; NIL operation ; PAIR } }
    At line 3 characters 38 to 50, value "KT1badaddr"
    is invalid for type address.
    { "id": "proto.014-PtKathma.destination_repr.invalid_b58check",
      "description":
        "Failed to read a valid destination from a b58check_encoding data",
      "data": { "input": "KT1badaddr" } }
    Note: You compiled your contract with protocol jakarta although we internally use protocol kathmandu to typecheck the produced Michelson contract
    so you might want to ignore this error if related to a breaking change in protocol kathmandu

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Warning: Error(s) occurred while type checking the produced michelson contract:
    Ill typed contract:
      1: { parameter int ;
      2:   storage address ;
      3:   code { DROP /* [] */ ; PUSH address "KT1badaddr" ; NIL operation ; PAIR } }
    At line 3 characters 38 to 50, value "KT1badaddr"
    is invalid for type address.
    { "id": "proto.014-PtKathma.destination_repr.invalid_b58check",
      "description":
        "Failed to read a valid destination from a b58check_encoding data",
      "data": { "input": "KT1badaddr" } }
    Note: You compiled your contract with protocol jakarta although we internally use protocol kathmandu to typecheck the produced Michelson contract
    so you might want to ignore this error if related to a breaking change in protocol kathmandu |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "bad_timestamp.ligo" ] ;
  [%expect {|
    File "../../test/contracts/bad_timestamp.ligo", line 7, characters 30-44:
      6 |   {
      7 |     var stamp : timestamp := ("badtimestamp" : timestamp)
      8 |   }

    Ill-formed timestamp "badtimestamp".
    At this point, a string with a RFC3339 notation or the number of seconds since Epoch is expected. |}]

let%expect_test _ =
    run_ligo_good [ "run" ; "dry-run" ; contract "redeclaration.ligo" ; "unit" ; "0" ] ;
    [%expect {|
      File "../../test/contracts/redeclaration.ligo", line 1, characters 20-21:
        1 | function foo (const p : unit) : int is 0
        2 |
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

      File "../../test/contracts/redeclaration.ligo", line 6, characters 20-21:
        5 |
        6 | function foo (const p : unit) : int is 1
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

    failed with: "This contract always fails" |}]

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

File "../../test/contracts/negative/create_contract_toplevel.mligo", line 4, character 2 to line 10, character 19:
  3 | let main (action, store : string * string) : return =
  4 |   let toto : operation * address = Tezos.create_contract
  5 |     (fun (p, s : nat * string) -> (([] : operation list), store))
  6 |     (None: key_hash option)
  7 |     300tz
  8 |     "un"
  9 |   in
 10 |   ([toto.0], store)

Not all free variables could be inlined in Tezos.create_contract usage: gen#148. |}] ;

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

    File "../../test/contracts/negative/create_contract_modfv.mligo", line 7, character 2 to line 13, character 19:
      6 |   end in
      7 |   let toto : operation * address = Tezos.create_contract
      8 |     (fun (p, s : nat * string) -> (([] : operation list), Foo.store))
      9 |     (None: key_hash option)
     10 |     300tz
     11 |     "un"
     12 |   in
     13 |   ([toto.0], store)

    Not all free variables could be inlined in Tezos.create_contract usage: gen#151. |}] ;

  run_ligo_bad [ "compile" ; "contract" ; bad_contract "create_contract_no_inline.mligo" ] ;
  [%expect{|
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

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 9, characters 19-89:
      8 | let main (action, store : int * int) : return =
      9 |   let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
     10 |   let toto : operation list = [ op ] in

    Not all free variables could be inlined in Tezos.create_contract usage: foo#167. |}] ;

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
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

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
      storage (pair (pair int string) nat bool) ;
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
  run_ligo_good [ "compile" ; "contract" ; contract "self_type_annotation_warn.ligo" ] ;
  [%expect {|
    File "../../test/contracts/self_type_annotation_warn.ligo", line 8, characters 10-23:
      7 |   {
      8 |     const self_contract: contract(int) = Tezos.self ("%default");
      9 |   }
    :
    Warning: unused variable "self_contract".
    Hint: replace it by "_self_contract" to prevent this warning.

    File "../../test/contracts/self_type_annotation_warn.ligo", line 6, characters 21-22:
      5 |
      6 | function main (const p : parameter; const s : storage) : return is
      7 |   {
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/self_type_annotation_warn.ligo", line 8, characters 4-64:
      7 |   {
      8 |     const self_contract: contract(int) = Tezos.self ("%default");
      9 |   }

    Warning: Tezos.self type annotation.
    Annotation "contract (int)" was given, but contract being compiled would expect "contract (nat)".
    Note that "Tezos.self" refers to the current contract, so the parameters should be generally the same.
    { parameter nat ; storage int ; code { CDR ; NIL operation ; PAIR } } |}] ;

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

File "../../test/contracts/negative/bad_contract.mligo", line 4, characters 4-8:
  3 |
  4 | let main (action, store : parameter * storage) : storage =
  5 |   store + 1

Invalid type for entrypoint "main".
An entrypoint must of type "parameter * storage -> operation list * storage". |}] ;

  run_ligo_bad [ "compile" ; "contract" ; bad_contract "bad_contract2.mligo" ] ;
  [%expect {|
File "../../test/contracts/negative/bad_contract2.mligo", line 5, characters 10-16:
  4 |
  5 | let main (action, store : parameter * storage) : return =
  6 |   ("bad",store + 1)
:
Warning: unused variable "action".
Hint: replace it by "_action" to prevent this warning.

File "../../test/contracts/negative/bad_contract2.mligo", line 5, character 0 to line 6, character 19:
  4 |
  5 | let main (action, store : parameter * storage) : return =
  6 |   ("bad",store + 1)

Invalid type for entrypoint "main".
An entrypoint must of type "parameter * storage -> operation list * storage".
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

File "../../test/contracts/negative/bad_contract3.mligo", line 5, character 0 to line 6, character 30:
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
      7 |   {
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
      7 |   {
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
  7 |   {
:
Warning: unused variable "p".
Hint: replace it by "_p" to prevent this warning.

File "../../test/contracts/negative/self_bad_entrypoint_format.ligo", line 8, characters 52-58:
  7 |   {
  8 |     const self_contract: contract(int) = Tezos.self("Toto") ;
  9 |     const op : operation = Tezos.transaction (2, 300tz, self_contract) ;

Invalid entrypoint "Toto". One of the following patterns is expected:
* "%bar" is expected for entrypoint "Bar"
* "%default" when no entrypoint is used. |}];

  run_ligo_bad ["compile" ; "contract"; bad_contract "nested_bigmap_1.religo"];
  [%expect {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    File "../../test/contracts/negative/nested_bigmap_1.religo", line 1, characters 11-29:
      1 | type bar = big_map (nat, int);
      2 |

    Invalid big map nesting.
    A big map cannot be nested inside another big map. |}];

  run_ligo_bad ["compile" ; "contract"; bad_contract "nested_bigmap_2.religo"];
  [%expect {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    File "../../test/contracts/negative/nested_bigmap_2.religo", line 2, characters 29-50:
      1 | /* this should result in an error as nested big_maps are not supported: */
      2 | type storage = big_map (nat, big_map (int, string));
      3 |

    Invalid big map nesting.
    A big map cannot be nested inside another big map. |}];

  run_ligo_bad ["compile" ; "contract"; bad_contract "nested_bigmap_3.religo"];
  [%expect {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    File "../../test/contracts/negative/nested_bigmap_3.religo", line 1, characters 11-29:
      1 | type bar = big_map (nat, int);
      2 |

    Invalid big map nesting.
    A big map cannot be nested inside another big map. |}];

  run_ligo_bad ["compile" ; "contract"; bad_contract "nested_bigmap_4.religo"];
  [%expect {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

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
  (set[] : list (operation) , x)const f0 = lambda (_a : string) return true
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
  (set[] : list (operation) , x)const f0 = lambda (_a : string) return true
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
    File "../../test/contracts/existential.mligo", line 1, characters 8-10:
      1 | let a : 'a = 2
      2 | let b : _ ->'b = fun _ -> 2

    Type "'a" not found. |}];
  run_ligo_bad ["print" ; "ast-typed"; bad_contract "missing_funarg_annotation.mligo"];
  [%expect {|
    File "../../test/contracts/negative/missing_funarg_annotation.mligo", line 3, characters 7-10:
      2 | let a b = b
      3 | let a (b,c) = b
      4 | let a ((b)) = b

    Pattern not of the expected type ^gen#449 |}];
  run_ligo_bad ["print" ; "ast-typed"; bad_contract "missing_funarg_annotation.religo"];
  [%expect {|
Pattern (b,c) not of the expected type ^gen#449 |}];
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
      code { LAMBDA (pair unit unit unit unit) unit { DROP ; UNIT } ;
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


let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "warning_duplicate3.mligo" ] ;
  [%expect{|
    { parameter (pair (chest %c) (chest_key %ck)) ;
      storage int ;
      code { DROP ; PUSH int 1 ; NIL operation ; PAIR } } |}]

(* warning layout attribute on constructor *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "B 42n" ; "--init-file" ; contract "warning_layout.mligo" ] ;
  [%expect {|
    File "../../test/contracts/warning_layout.mligo", line 3, character 4 to line 6, character 13:
      2 |   [@layout:comb]
      3 |     B of nat
      4 |   | C of int
      5 |   | D of string
      6 |   | A of unit
      7 |

    Warning: layout attribute only applying to B, probably ignored.


    Warning: The type of this value is ambiguous: Inferred type is parameter_ok but could be of type parameter_warns.
    Hint: You might want to add a type annotation.

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
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

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
    File "../../test/contracts/never.jsligo", line 8, characters 4-8:
      7 |
      8 | let main = ([action, store] : [parameter, storage]) : [list<operation>, storage] => {
      9 |   return [

    Toplevel let declaration are silently change to const declaration.

    { parameter (or (never %extend) (int %increment)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { SWAP ; DROP ; NEVER } { ADD } ;
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
    File "../../test/contracts/negative/compile_test.mligo", line 21, characters 14-37:
     20 |   let (taddr, _, _) = Test.originate main  initial_storage 0tez in
     21 |   let contr = Test.to_contract(taddr) in
     22 |   let _r = Test.transfer_to_contract_exn contr (Increment (32)) 1tez  in

    Underspecified type ^gen#453.
    Please add additional annotations. |}]

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
    File "../../test/contracts/negative/bad_annotation_unpack.mligo", line 1, characters 9-42:
      1 | let x = (Bytes.unpack (Bytes.pack "hello") : string)

    Invalid type(s)
    Cannot unify option (^gen#447) with string. |}]

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
  [%expect {xxx|
    const f : int -> int =
      lambda (nint)int return let f : int -> int =
                                rec (fint -> int => lambda (nint)int return
                              let match_#111[@var] : bool = EQ(n , 0) in
                               match match_#111 with
                                | False unit_proj#112 ->
                                  (f)@(C_POLYMORPHIC_SUB(n , 1))
                                | True unit_proj#113 ->
                                  1) in
                              (f)@(4)
    const g : int -> int -> int -> int =
      rec (gint -> int -> int -> int => lambda (fint -> int)int -> int return
      (g)@(let h : int -> int =
             rec (hint -> int => lambda (nint)int return let match_#114[@var] : bool =
                                                           EQ(n , 0) in
                                                          match match_#114 with
                                                           | False unit_proj#115 ->
                                                             (h)@(C_POLYMORPHIC_SUB
                                                                  (n ,
                                                                   1))
                                                           | True unit_proj#116 ->
                                                             1) in
           h)) |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "reuse_variable_name_top.jsligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/reuse_variable_name_top.jsligo", line 2, characters 10-14:
      1 | let dog = 1;
      2 | let dog = true;

    Cannot redeclare block-scoped variable. |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "reuse_variable_name_block.jsligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/reuse_variable_name_block.jsligo", line 3, characters 8-13:
      2 |     let x = 2;
      3 |     let x = 2;
      4 |     return x;

    Cannot redeclare block-scoped variable. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "evaluate-call"; contract "assert.mligo"; "(false, ())"; "-e"; "with_error"];
  [%expect {| failed with: "my custom error" |}]

let%expect_test _ =
  run_ligo_good [ "run"; "evaluate-call"; contract "assert.mligo"; "(None: unit option)"; "-e"; "some_with_error"];
  [%expect {| failed with: "my custom error" |}]

let%expect_test _ =
  run_ligo_good [ "run"; "evaluate-call"; contract "assert.mligo"; "(Some (): unit option)"; "-e"; "none_with_error"];
  [%expect {| failed with: "my custom error" |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "attributes.jsligo" ] ;
  [%expect {xxx|
    const x[@var] : int = 1[@inline][@private]
    const foo[@var] : int -> int =
      lambda (aint)int return let test[@var] : int =
                                C_POLYMORPHIC_ADD(2 , a)[@inline][@private] in
                              test[@inline][@private]
    const y[@var] : int = 1[@private]
    const bar[@var] : int -> int =
      lambda (bint)int return let test[@var] : int -> int =
                                lambda (zint)int return C_POLYMORPHIC_ADD
                                                        (C_POLYMORPHIC_ADD(2 , b) ,
                                                         z)[@inline][@private] in
                              (test)@(b)[@private]
    const check[@var] : int = 4[@private] |xxx}]

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
      Internal error: Entrypoint main does not exist |}];
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "modules_export_const.jsligo" ] ;
    [%expect {|
      Internal error: Entrypoint main does not exist |}];
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "modules_export_namespace.jsligo" ] ;
    [%expect {|
      Internal error: Entrypoint main does not exist |}]

(* Test compile contract with Big_map.get_and_update for Hangzhou *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "ticket_wallet.mligo" ] ;
  [%expect{|
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
               { DUP 2 ;
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
             LAMBDA
               unit
               unit
               { { /* x#127 */ } }
             /* File "../../test/contracts/noop.mligo", line 2, characters 9-10 */ ;
             { /* f#126, _ */ } ;
             SWAP ;
             DUP 2 ;
             SWAP ;
             EXEC ;
             { /* s2#128, f#126 */ } ;
             DUP 2 ;
             SWAP ;
             EXEC ;
             { /* s3#129, f#126 */ } ;
             EXEC ;
             { /* s#130 */ } ;
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
              [ "T_constant",
                { "language": "Michelson", "injection": [ "Unit" ],
                  "parameters": [] } ], "type_meta": null, "orig_var": null,
            "location":
              [ "File",
                { "start":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "3", "pos_bol": "90", "pos_cnum": "108" },
                      "point_num": "108", "point_bol": "90" },
                  "stop":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "3", "pos_bol": "90", "pos_cnum": "111" },
                      "point_num": "111", "point_bol": "90" } } ] },
          { "type_content":
              [ "T_constant",
                { "language": "Michelson", "injection": [ "Unit" ],
                  "parameters": [] } ], "type_meta": null, "orig_var": null,
            "location":
              [ "File",
                { "start":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "4", "pos_bol": "115", "pos_cnum": "133" },
                      "point_num": "133", "point_bol": "115" },
                  "stop":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "4", "pos_bol": "115", "pos_cnum": "137" },
                      "point_num": "137", "point_bol": "115" } } ] },
          { "type_content":
              [ "T_constant",
                { "language": "Michelson", "injection": [ "Unit" ],
                  "parameters": [] } ], "type_meta": null, "orig_var": null,
            "location":
              [ "File",
                { "start":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "5", "pos_bol": "141", "pos_cnum": "151" },
                      "point_num": "151", "point_bol": "141" },
                  "stop":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "5", "pos_bol": "141", "pos_cnum": "155" },
                      "point_num": "155", "point_bol": "141" } } ] },
          { "type_content":
              [ "T_constant",
                { "language": "Michelson", "injection": [ "Unit" ],
                  "parameters": [] } ], "type_meta": null, "orig_var": null,
            "location": [ "Virtual", "generated" ] },
          { "type_content":
              [ "T_arrow",
                { "type1":
                    { "type_content":
                        [ "T_constant",
                          { "language": "Michelson", "injection": [ "Unit" ],
                            "parameters": [] } ], "type_meta": null,
                      "orig_var": null, "location": [ "Virtual", "generated" ] },
                  "type2":
                    { "type_content":
                        [ "T_constant",
                          { "language": "Michelson", "injection": [ "Unit" ],
                            "parameters": [] } ], "type_meta": null,
                      "orig_var": null, "location": [ "Virtual", "generated" ] } } ],
            "type_meta":
              { "type_content":
                  [ "T_arrow",
                    { "type1":
                        { "type_content":
                            [ "T_variable",
                              { "name": "unit", "counter": "0",
                                "generated": false,
                                "location": [ "Virtual", "dummy" ] } ],
                          "sugar":
                            { "type_content":
                                [ "T_variable",
                                  { "name": "unit", "counter": "0",
                                    "generated": false,
                                    "location": [ "Virtual", "dummy" ] } ],
                              "location":
                                [ "File",
                                  { "start":
                                      { "byte":
                                          { "pos_fname":
                                              "../../test/contracts/noop.mligo",
                                            "pos_lnum": "2", "pos_bol": "57",
                                            "pos_cnum": "70" },
                                        "point_num": "70", "point_bol": "57" },
                                    "stop":
                                      { "byte":
                                          { "pos_fname":
                                              "../../test/contracts/noop.mligo",
                                            "pos_lnum": "2", "pos_bol": "57",
                                            "pos_cnum": "74" },
                                        "point_num": "74", "point_bol": "57" } } ] },
                          "location":
                            [ "File",
                              { "start":
                                  { "byte":
                                      { "pos_fname":
                                          "../../test/contracts/noop.mligo",
                                        "pos_lnum": "2", "pos_bol": "57",
                                        "pos_cnum": "70" }, "point_num": "70",
                                    "point_bol": "57" },
                                "stop":
                                  { "byte":
                                      { "pos_fname":
                                          "../../test/contracts/noop.mligo",
                                        "pos_lnum": "2", "pos_bol": "57",
                                        "pos_cnum": "74" }, "point_num": "74",
                                    "point_bol": "57" } } ] },
                      "type2":
                        { "type_content":
                            [ "T_variable",
                              { "name": "unit", "counter": "0",
                                "generated": false,
                                "location": [ "Virtual", "dummy" ] } ],
                          "sugar":
                            { "type_content":
                                [ "T_variable",
                                  { "name": "unit", "counter": "0",
                                    "generated": false,
                                    "location": [ "Virtual", "dummy" ] } ],
                              "location":
                                [ "File",
                                  { "start":
                                      { "byte":
                                          { "pos_fname":
                                              "../../test/contracts/noop.mligo",
                                            "pos_lnum": "2", "pos_bol": "57",
                                            "pos_cnum": "78" },
                                        "point_num": "78", "point_bol": "57" },
                                    "stop":
                                      { "byte":
                                          { "pos_fname":
                                              "../../test/contracts/noop.mligo",
                                            "pos_lnum": "2", "pos_bol": "57",
                                            "pos_cnum": "82" },
                                        "point_num": "82", "point_bol": "57" } } ] },
                          "location":
                            [ "File",
                              { "start":
                                  { "byte":
                                      { "pos_fname":
                                          "../../test/contracts/noop.mligo",
                                        "pos_lnum": "2", "pos_bol": "57",
                                        "pos_cnum": "78" }, "point_num": "78",
                                    "point_bol": "57" },
                                "stop":
                                  { "byte":
                                      { "pos_fname":
                                          "../../test/contracts/noop.mligo",
                                        "pos_lnum": "2", "pos_bol": "57",
                                        "pos_cnum": "82" }, "point_num": "82",
                                    "point_bol": "57" } } ] } } ],
                "sugar":
                  { "type_content":
                      [ "T_arrow",
                        { "type1":
                            { "type_content":
                                [ "T_variable",
                                  { "name": "unit", "counter": "0",
                                    "generated": false,
                                    "location": [ "Virtual", "dummy" ] } ],
                              "location":
                                [ "File",
                                  { "start":
                                      { "byte":
                                          { "pos_fname":
                                              "../../test/contracts/noop.mligo",
                                            "pos_lnum": "2", "pos_bol": "57",
                                            "pos_cnum": "70" },
                                        "point_num": "70", "point_bol": "57" },
                                    "stop":
                                      { "byte":
                                          { "pos_fname":
                                              "../../test/contracts/noop.mligo",
                                            "pos_lnum": "2", "pos_bol": "57",
                                            "pos_cnum": "74" },
                                        "point_num": "74", "point_bol": "57" } } ] },
                          "type2":
                            { "type_content":
                                [ "T_variable",
                                  { "name": "unit", "counter": "0",
                                    "generated": false,
                                    "location": [ "Virtual", "dummy" ] } ],
                              "location":
                                [ "File",
                                  { "start":
                                      { "byte":
                                          { "pos_fname":
                                              "../../test/contracts/noop.mligo",
                                            "pos_lnum": "2", "pos_bol": "57",
                                            "pos_cnum": "78" },
                                        "point_num": "78", "point_bol": "57" },
                                    "stop":
                                      { "byte":
                                          { "pos_fname":
                                              "../../test/contracts/noop.mligo",
                                            "pos_lnum": "2", "pos_bol": "57",
                                            "pos_cnum": "82" },
                                        "point_num": "82", "point_bol": "57" } } ] } } ],
                    "location": [ "Virtual", "generated" ] },
                "location": [ "Virtual", "generated" ] }, "orig_var": null,
            "location": [ "Virtual", "generated" ] } ],
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
                      [], { "prim": "SWAP" },
                      { "prim": "DUP", "args": [ { "int": "2" } ] },
                      { "prim": "SWAP" }, { "prim": "EXEC" }, [],
                      { "prim": "DUP", "args": [ { "int": "2" } ] },
                      { "prim": "SWAP" }, { "prim": "EXEC" }, [],
                      { "prim": "EXEC" }, [],
                      { "prim": "NIL", "args": [ { "prim": "operation" } ] },
                      { "prim": "PAIR" } ] ] } ],
          "locations":
            [ {}, {}, {}, {}, {}, {}, {}, { "environment": [ null ] }, {},
              { "environment": [ { "source_type": "3" } ] },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "2",
                        "col": "9" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "2",
                        "col": "10" } } }, {}, {}, {},
              { "environment": [ { "name": "x#127", "source_type": "3" } ] },
              { "environment":
                  [ { "name": "f#126", "source_type": "4" },
                    { "source_type": "3" } ] }, {}, {}, {}, {}, {},
              { "environment":
                  [ { "name": "s2#128", "source_type": "0" },
                    { "name": "f#126", "source_type": "4" } ] }, {}, {}, {}, {},
              { "environment":
                  [ { "name": "s3#129", "source_type": "1" },
                    { "name": "f#126", "source_type": "4" } ] }, {},
              { "environment": [ { "name": "s#130", "source_type": "2" } ] },
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
  [%expect {xxx|
const c : unit -> ( operation * address ) =
  lambda (gen#5unit)( operation * address ) return ((((Tezos.create_contract@{unit}@{unit})@(lambda (gen#2
                                                      ( unit * unit ))
                                                      ( list (operation) * unit ) return
                                                       match gen#2 with
                                                        | ( _#4 : unit , _#3 : unit ) ->
                                                        ( LIST_EMPTY() ,
                                                          unit )))@(None(unit)))@(0mutez))@(unit)
const foo : unit =
  let match_#117[@var] : ( operation * address ) = (c)@(unit) in
   match match_#117 with
    | ( _a : operation , _b : address ) ->
    unit
const c : unit -> ( int * string * nat * int * string * nat * int * string * nat * int * string ) =
  lambda (gen#6unit)( int * string * nat * int * string * nat * int * string * nat * int * string ) return
  ( 1 ,
    "1" ,
    +1 ,
    2 ,
    "2" ,
    +2 ,
    3 ,
    "3" ,
    +3 ,
    4 ,
    "4" )
const foo : unit =
  let match_#119[@var] : ( int * string * nat * int * string * nat * int * string * nat * int * string ) =
    (c)@(unit) in
   match match_#119 with
    | ( _i1 : int , _s1 : string , _n1 : nat , _i2 : int , _s2 : string , _n2 : nat , _i3 : int , _s3 : string , _n3 : nat , _i4 : int , _s4 : string ) ->
    unit |xxx} ]

(* Module being defined does not type with its own type *)
let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "modules_env.mligo" ] ;
  [%expect {|
    let get_balance#111 =
      fun _u#1590 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
    let get_amount#112 =
      fun _u#1592 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
    let get_now#113 = fun _u#1594 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
    let get_sender#114 =
      fun _u#1596 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
    let get_source#115 =
      fun _u#1598 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
    let get_level#116 = fun _u#1600 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
    let get_self_address#117 = fun _u#1602 -> (SELF_ADDRESS())[@inline] in
    let get_chain_id#118 =
      fun _u#1604 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
    let get_total_voting_power#119 =
      fun _u#1606 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
    let get_min_block_time#120 =
      fun _u#1608 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
    let voting_power#131 =
      fun kh#1620 -> (({ VOTING_POWER })@(kh#1620))[@inline] in
    let implicit_account#133 =
      fun kh#1624 -> (IMPLICIT_ACCOUNT(kh#1624))[@inline] in
    let pairing_check#137 =
      fun l#1632 -> (({ PAIRING_CHECK })@(l#1632))[@inline] in
    let set_delegate#139 = fun o#1636 -> (SET_DELEGATE(o#1636))[@inline] in
    let open_chest#147 =
      fun ck#1657 ->
      (fun c#1658 -> (fun n#1659 -> (OPEN_CHEST(ck#1657 , c#1658 , n#1659))))[@inline] in
    let xor#156 =
      fun l#1693 -> (fun r#1694 -> (XOR(l#1693 , r#1694)))[@inline] in
    let or#157 = fun l#1696 -> (fun r#1697 -> (OR(l#1696 , r#1697)))[@inline] in
    let shift_left#158 =
      fun l#1699 -> (fun r#1700 -> (LSL(l#1699 , r#1700)))[@inline] in
    let shift_right#159 =
      fun l#1702 -> (fun r#1703 -> (LSR(l#1702 , r#1703)))[@inline] in
    let length#204 = fun b#1848 -> (({ SIZE })@(b#1848))[@inline] in
    let concat#205 =
      fun b1#1850 ->
      (fun b2#1851 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1850 , b2#1851))))[@inline] in
    let sub#206 =
      fun s#1853 ->
      (fun l#1854 ->
       (fun b#1855 ->
        (({ UNPAIR ;
           UNPAIR ;
           SLICE ;
           IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1853 ,
                                                                       l#1854) ,
                                                                  b#1855)))))[@inline] in
    let length#212 = fun b#1870 -> (({ SIZE })@(b#1870))[@inline] in
    let concat#213 =
      fun b1#1872 ->
      (fun b2#1873 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1872 , b2#1873))))[@inline] in
    let sub#214 =
      fun s#1875 ->
      (fun l#1876 ->
       (fun b#1877 ->
        (({ UNPAIR ;
           UNPAIR ;
           SLICE ;
           IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1875 ,
                                                                       l#1876) ,
                                                                  b#1877)))))[@inline] in
    let blake2b#215 = fun b#1879 -> (({ BLAKE2B })@(b#1879))[@inline] in
    let sha256#216 = fun b#1881 -> (({ SHA256 })@(b#1881))[@inline] in
    let sha512#217 = fun b#1883 -> (({ SHA512 })@(b#1883))[@inline] in
    let sha3#218 = fun b#1885 -> (({ SHA3 })@(b#1885))[@inline] in
    let keccak#219 = fun b#1887 -> (({ KECCAK })@(b#1887))[@inline] in
    let hash_key#220 = fun k#1889 -> (({ HASH_KEY })@(k#1889))[@inline] in
    let check#221 =
      fun k#1891 ->
      (fun s#1892 ->
       (fun b#1893 ->
        (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#1891 , s#1892) ,
                                                       b#1893)))))[@inline] in
    let assert#222 =
      fun b#1895 ->
      (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#1895))[@inline] in
    let abs#225 = fun i#1901 -> (({ ABS })@(i#1901))[@inline] in
    let is_nat#226 = fun i#1903 -> (({ ISNAT })@(i#1903))[@inline] in
    let true#227 = TRUE()[@inline] in
    let false#228 = FALSE()[@inline] in
    let unit#229 = UNIT()[@inline] in
    let assert_with_error#233 =
      fun b#1913 ->
      (fun s#1914 ->
       (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#1913 , s#1914))))[@inline] in
    let poly_stub_78 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_77 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_76 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_75 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_74 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_73 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_72 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_71 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_70 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_69 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_68 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_67 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_66 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_65 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_64 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_63 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_62 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_61 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_60 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_59 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_58 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_57 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_56 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_55 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_54 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_53 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_52 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_51 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_50 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_49 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_48 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_47 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_46 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_45 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_44 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_43 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_42 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_41 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let poly_stub_40 = fun x#1925 -> (({ FAILWITH })@(x#1925))[@inline] in
    let get_total_voting_power#241 =
      fun _u#1934 -> ((poly_stub_78)@(L(unit)))[@inline] in
    let set_source#244 = fun _a#1940 -> ((poly_stub_77)@(L(unit)))[@inline] in
    let get_storage_of_address#245 =
      fun _a#1942 -> ((poly_stub_76)@(L(unit)))[@inline] in
    let get_balance#246 = fun _a#1944 -> ((poly_stub_75)@(L(unit)))[@inline] in
    let print#247 = fun _v#1946 -> ((poly_stub_74)@(L(unit)))[@inline] in
    let eprint#248 = fun _v#1948 -> ((poly_stub_73)@(L(unit)))[@inline] in
    let get_voting_power#249 =
      fun _kh#1950 -> ((poly_stub_72)@(L(unit)))[@inline] in
    let nth_bootstrap_contract#250 =
      fun _i#1952 -> ((poly_stub_71)@(L(unit)))[@inline] in
    let nth_bootstrap_account#251 =
      fun _i#1954 -> ((poly_stub_70)@(L(unit)))[@inline] in
    let get_bootstrap_account#252 =
      fun _n#1956 -> ((poly_stub_69)@(L(unit)))[@inline] in
    let last_originations#254 =
      fun _u#1960 -> ((poly_stub_68)@(L(unit)))[@inline] in
    let new_account#256 = fun _u#1964 -> ((poly_stub_67)@(L(unit)))[@inline] in
    let bake_until_n_cycle_end#258 =
      fun _n#1968 -> ((poly_stub_66)@(L(unit)))[@inline] in
    let register_delegate#260 =
      fun _kh#1972 -> ((poly_stub_65)@(L(unit)))[@inline] in
    let register_constant#261 =
      fun _m#1974 -> ((poly_stub_64)@(L(unit)))[@inline] in
    let constant_to_michelson_program#263 =
      fun _s#1978 -> ((poly_stub_63)@(L(unit)))[@inline] in
    let restore_context#264 =
      fun _u#1980 -> ((poly_stub_62)@(L(unit)))[@inline] in
    let save_context#265 = fun _u#1982 -> ((poly_stub_61)@(L(unit)))[@inline] in
    let drop_context#266 = fun _u#1984 -> ((poly_stub_60)@(L(unit)))[@inline] in
    let set_baker_policy#269 =
      fun _bp#1990 -> ((poly_stub_59)@(L(unit)))[@inline] in
    let set_baker#270 = fun _a#1992 -> ((poly_stub_58)@(L(unit)))[@inline] in
    let size#271 = fun _c#1994 -> ((poly_stub_57)@(L(unit)))[@inline] in
    let read_contract_from_file#273 =
      fun _fn#1998 -> ((poly_stub_56)@(L(unit)))[@inline] in
    let chr#274 = fun _n#2000 -> ((poly_stub_55)@(L(unit)))[@inline] in
    let nl#275 = L("NEWLINE")[@inline] in
    let println#276 = fun _v#2003 -> ((poly_stub_54)@(L(unit)))[@inline] in
    let transfer#277 =
      fun _a#2005 -> (fun _s#2006 -> (fun _t#2007 -> ((poly_stub_53)@(L(unit)))))[@inline] in
    let transfer_exn#278 =
      fun _a#2009 -> (fun _s#2010 -> (fun _t#2011 -> ((poly_stub_52)@(L(unit)))))[@inline] in
    let reset_state#280 =
      fun _n#2015 -> (fun _l#2016 -> ((poly_stub_51)@(L(unit))))[@inline] in
    let reset_state_at#281 =
      fun _t#2018 -> (fun _n#2019 -> (fun _l#2020 -> ((poly_stub_50)@(L(unit)))))[@inline] in
    let save_mutation#284 =
      fun _s#2029 -> (fun _m#2030 -> ((poly_stub_49)@(L(unit))))[@inline] in
    let sign#287 =
      fun _sk#2038 -> (fun _d#2039 -> ((poly_stub_48)@(L(unit))))[@inline] in
    let add_account#288 =
      fun _s#2041 -> (fun _k#2042 -> ((poly_stub_47)@(L(unit))))[@inline] in
    let baker_account#289 =
      fun _p#2044 -> (fun _o#2045 -> ((poly_stub_46)@(L(unit))))[@inline] in
    let create_chest#291 =
      fun _b#2050 -> (fun _n#2051 -> ((poly_stub_45)@(L(unit))))[@inline] in
    let create_chest_key#292 =
      fun _c#2053 -> (fun _n#2054 -> ((poly_stub_44)@(L(unit))))[@inline] in
    let michelson_equal#295 =
      fun _m1#2064 -> (fun _m2#2065 -> ((poly_stub_43)@(L(unit))))[@inline] in
    let originate_contract#297 =
      fun _c#2070 -> (fun _s#2071 -> (fun _t#2072 -> ((poly_stub_42)@(L(unit)))))[@inline] in
    let compile_contract_from_file#299 =
      fun _fn#2078 ->
      (fun _e#2079 -> (fun _v#2080 -> ((poly_stub_41)@(L(unit)))))[@inline] in
    let originate_from_file#300 =
      fun _fn#2082 ->
      (fun _e#2083 ->
       (fun _v#2084 ->
        (fun _s#2085 -> (fun _t#2086 -> ((poly_stub_40)@(L(unit)))))))[@inline] in
    let get_balance#301 =
      fun _u#2088 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
    let get_amount#302 =
      fun _u#2090 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
    let get_now#303 = fun _u#2092 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
    let get_sender#304 =
      fun _u#2094 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
    let get_source#305 =
      fun _u#2096 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
    let get_level#306 = fun _u#2098 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
    let get_self_address#307 = fun _u#2100 -> (SELF_ADDRESS())[@inline] in
    let get_chain_id#308 =
      fun _u#2102 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
    let get_total_voting_power#309 =
      fun _u#2104 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
    let get_min_block_time#310 =
      fun _u#2106 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
    let voting_power#321 =
      fun kh#2118 -> (({ VOTING_POWER })@(kh#2118))[@inline] in
    let implicit_account#323 =
      fun kh#2122 -> (IMPLICIT_ACCOUNT(kh#2122))[@inline] in
    let pairing_check#327 =
      fun l#2130 -> (({ PAIRING_CHECK })@(l#2130))[@inline] in
    let set_delegate#329 = fun o#2134 -> (SET_DELEGATE(o#2134))[@inline] in
    let open_chest#337 =
      fun gen#2158 ->
      (let (gen#2685, gen#2686) = gen#2158 in
       let (gen#2687, gen#2688) = gen#2685 in
       let ck#2159 = gen#2687 in
       let c#2160 = gen#2688 in
       let n#2161 = gen#2686 in OPEN_CHEST(ck#2159 , c#2160 , n#2161))[@inline] in
    let xor#346 =
      fun gen#2204 ->
      (let (gen#2689, gen#2690) = gen#2204 in
       let l#2205 = gen#2689 in let r#2206 = gen#2690 in XOR(l#2205 , r#2206))[@inline] in
    let or#347 =
      fun gen#2208 ->
      (let (gen#2691, gen#2692) = gen#2208 in
       let l#2209 = gen#2691 in let r#2210 = gen#2692 in OR(l#2209 , r#2210))[@inline] in
    let shift_left#348 =
      fun gen#2212 ->
      (let (gen#2693, gen#2694) = gen#2212 in
       let l#2213 = gen#2693 in let r#2214 = gen#2694 in LSL(l#2213 , r#2214))[@inline] in
    let shift_right#349 =
      fun gen#2216 ->
      (let (gen#2695, gen#2696) = gen#2216 in
       let l#2217 = gen#2695 in let r#2218 = gen#2696 in LSR(l#2217 , r#2218))[@inline] in
    let length#394 = fun b#2394 -> (({ SIZE })@(b#2394))[@inline] in
    let concat#395 =
      fun gen#2396 ->
      (let (gen#2697, gen#2698) = gen#2396 in
       let b1#2397 = gen#2697 in
       let b2#2398 = gen#2698 in ({ UNPAIR ; CONCAT })@(PAIR(b1#2397 , b2#2398)))[@inline] in
    let sub#396 =
      fun gen#2400 ->
      (let (gen#2699, gen#2700) = gen#2400 in
       let (gen#2701, gen#2702) = gen#2699 in
       let s#2401 = gen#2701 in
       let l#2402 = gen#2702 in
       let b#2403 = gen#2700 in
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2401 ,
                                                                     l#2402) ,
                                                                b#2403)))[@inline] in
    let length#402 = fun b#2420 -> (({ SIZE })@(b#2420))[@inline] in
    let concat#403 =
      fun gen#2422 ->
      (let (gen#2703, gen#2704) = gen#2422 in
       let b1#2423 = gen#2703 in
       let b2#2424 = gen#2704 in ({ UNPAIR ; CONCAT })@(PAIR(b1#2423 , b2#2424)))[@inline] in
    let sub#404 =
      fun gen#2426 ->
      (let (gen#2705, gen#2706) = gen#2426 in
       let (gen#2707, gen#2708) = gen#2705 in
       let s#2427 = gen#2707 in
       let l#2428 = gen#2708 in
       let b#2429 = gen#2706 in
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2427 ,
                                                                     l#2428) ,
                                                                b#2429)))[@inline] in
    let blake2b#405 = fun b#2431 -> (({ BLAKE2B })@(b#2431))[@inline] in
    let sha256#406 = fun b#2433 -> (({ SHA256 })@(b#2433))[@inline] in
    let sha512#407 = fun b#2435 -> (({ SHA512 })@(b#2435))[@inline] in
    let sha3#408 = fun b#2437 -> (({ SHA3 })@(b#2437))[@inline] in
    let keccak#409 = fun b#2439 -> (({ KECCAK })@(b#2439))[@inline] in
    let hash_key#410 = fun k#2441 -> (({ HASH_KEY })@(k#2441))[@inline] in
    let check#411 =
      fun gen#2443 ->
      (let (gen#2709, gen#2710) = gen#2443 in
       let (gen#2711, gen#2712) = gen#2709 in
       let k#2444 = gen#2711 in
       let s#2445 = gen#2712 in
       let b#2446 = gen#2710 in
       ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#2444 , s#2445) ,
                                                     b#2446)))[@inline] in
    let assert#412 =
      fun b#2448 ->
      (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#2448))[@inline] in
    let abs#415 = fun i#2454 -> (({ ABS })@(i#2454))[@inline] in
    let is_nat#416 = fun i#2456 -> (({ ISNAT })@(i#2456))[@inline] in
    let true#417 = TRUE()[@inline] in
    let false#418 = FALSE()[@inline] in
    let unit#419 = UNIT()[@inline] in
    let assert_with_error#423 =
      fun gen#2466 ->
      (let (gen#2713, gen#2714) = gen#2466 in
       let b#2467 = gen#2713 in
       let s#2468 = gen#2714 in
       ({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#2467 , s#2468)))[@inline] in
    let poly_stub_39 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_38 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_37 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_36 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_35 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_34 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_33 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_32 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_31 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_30 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_29 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_28 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_27 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_26 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_25 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_24 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_23 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_22 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_21 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_20 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_19 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_18 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_17 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_16 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_15 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_14 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_13 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_12 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_11 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_10 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_9 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_8 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_7 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_6 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_5 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_4 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_3 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_2 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let poly_stub_1 = fun x#2482 -> (({ FAILWITH })@(x#2482))[@inline] in
    let get_total_voting_power#431 =
      fun _u#2492 -> ((poly_stub_39)@(L(unit)))[@inline] in
    let set_source#434 = fun _a#2498 -> ((poly_stub_38)@(L(unit)))[@inline] in
    let get_storage_of_address#435 =
      fun _a#2500 -> ((poly_stub_37)@(L(unit)))[@inline] in
    let get_balance#436 = fun _a#2502 -> ((poly_stub_36)@(L(unit)))[@inline] in
    let print#437 = fun _v#2504 -> ((poly_stub_35)@(L(unit)))[@inline] in
    let eprint#438 = fun _v#2506 -> ((poly_stub_34)@(L(unit)))[@inline] in
    let get_voting_power#439 =
      fun _kh#2508 -> ((poly_stub_33)@(L(unit)))[@inline] in
    let nth_bootstrap_contract#440 =
      fun _i#2510 -> ((poly_stub_32)@(L(unit)))[@inline] in
    let nth_bootstrap_account#441 =
      fun _i#2512 -> ((poly_stub_31)@(L(unit)))[@inline] in
    let get_bootstrap_account#442 =
      fun _n#2514 -> ((poly_stub_30)@(L(unit)))[@inline] in
    let last_originations#444 =
      fun _u#2518 -> ((poly_stub_29)@(L(unit)))[@inline] in
    let new_account#446 = fun _u#2522 -> ((poly_stub_28)@(L(unit)))[@inline] in
    let bake_until_n_cycle_end#448 =
      fun _n#2526 -> ((poly_stub_27)@(L(unit)))[@inline] in
    let register_delegate#450 =
      fun _kh#2530 -> ((poly_stub_26)@(L(unit)))[@inline] in
    let register_constant#451 =
      fun _m#2532 -> ((poly_stub_25)@(L(unit)))[@inline] in
    let constant_to_michelson_program#453 =
      fun _s#2536 -> ((poly_stub_24)@(L(unit)))[@inline] in
    let restore_context#454 =
      fun _u#2538 -> ((poly_stub_23)@(L(unit)))[@inline] in
    let save_context#455 = fun _u#2540 -> ((poly_stub_22)@(L(unit)))[@inline] in
    let drop_context#456 = fun _u#2542 -> ((poly_stub_21)@(L(unit)))[@inline] in
    let set_baker_policy#459 =
      fun _bp#2548 -> ((poly_stub_20)@(L(unit)))[@inline] in
    let set_baker#460 = fun _a#2550 -> ((poly_stub_19)@(L(unit)))[@inline] in
    let size#461 = fun _c#2552 -> ((poly_stub_18)@(L(unit)))[@inline] in
    let read_contract_from_file#463 =
      fun _fn#2556 -> ((poly_stub_17)@(L(unit)))[@inline] in
    let chr#464 = fun _n#2558 -> ((poly_stub_16)@(L(unit)))[@inline] in
    let nl#465 = L("NEWLINE")[@inline] in
    let println#466 = fun _v#2561 -> ((poly_stub_15)@(L(unit)))[@inline] in
    let transfer#467 =
      fun gen#2563 ->
      (let (gen#2715, gen#2716) = gen#2563 in
       let (gen#2717, gen#2718) = gen#2715 in
       let _a#2564 = gen#2717 in
       let _s#2565 = gen#2718 in
       let _t#2566 = gen#2716 in (poly_stub_14)@(L(unit)))[@inline] in
    let transfer_exn#468 =
      fun gen#2568 ->
      (let (gen#2719, gen#2720) = gen#2568 in
       let (gen#2721, gen#2722) = gen#2719 in
       let _a#2569 = gen#2721 in
       let _s#2570 = gen#2722 in
       let _t#2571 = gen#2720 in (poly_stub_13)@(L(unit)))[@inline] in
    let reset_state#470 =
      fun gen#2575 ->
      (let (gen#2723, gen#2724) = gen#2575 in
       let _n#2576 = gen#2723 in
       let _l#2577 = gen#2724 in (poly_stub_12)@(L(unit)))[@inline] in
    let reset_state_at#471 =
      fun gen#2579 ->
      (let (gen#2725, gen#2726) = gen#2579 in
       let (gen#2727, gen#2728) = gen#2725 in
       let _t#2580 = gen#2727 in
       let _n#2581 = gen#2728 in
       let _l#2582 = gen#2726 in (poly_stub_11)@(L(unit)))[@inline] in
    let save_mutation#474 =
      fun gen#2593 ->
      (let (gen#2729, gen#2730) = gen#2593 in
       let _s#2594 = gen#2729 in
       let _m#2595 = gen#2730 in (poly_stub_10)@(L(unit)))[@inline] in
    let sign#477 =
      fun gen#2605 ->
      (let (gen#2731, gen#2732) = gen#2605 in
       let _sk#2606 = gen#2731 in
       let _d#2607 = gen#2732 in (poly_stub_9)@(L(unit)))[@inline] in
    let add_account#478 =
      fun gen#2609 ->
      (let (gen#2733, gen#2734) = gen#2609 in
       let _s#2610 = gen#2733 in
       let _k#2611 = gen#2734 in (poly_stub_8)@(L(unit)))[@inline] in
    let baker_account#479 =
      fun gen#2613 ->
      (let (gen#2735, gen#2736) = gen#2613 in
       let _p#2614 = gen#2735 in
       let _o#2615 = gen#2736 in (poly_stub_7)@(L(unit)))[@inline] in
    let create_chest#481 =
      fun gen#2621 ->
      (let (gen#2737, gen#2738) = gen#2621 in
       let _b#2622 = gen#2737 in
       let _n#2623 = gen#2738 in (poly_stub_6)@(L(unit)))[@inline] in
    let create_chest_key#482 =
      fun gen#2625 ->
      (let (gen#2739, gen#2740) = gen#2625 in
       let _c#2626 = gen#2739 in
       let _n#2627 = gen#2740 in (poly_stub_5)@(L(unit)))[@inline] in
    let michelson_equal#485 =
      fun gen#2639 ->
      (let (gen#2741, gen#2742) = gen#2639 in
       let _m1#2640 = gen#2741 in
       let _m2#2641 = gen#2742 in (poly_stub_4)@(L(unit)))[@inline] in
    let originate_contract#487 =
      fun gen#2647 ->
      (let (gen#2743, gen#2744) = gen#2647 in
       let (gen#2745, gen#2746) = gen#2743 in
       let _c#2648 = gen#2745 in
       let _s#2649 = gen#2746 in
       let _t#2650 = gen#2744 in (poly_stub_3)@(L(unit)))[@inline] in
    let compile_contract_from_file#489 =
      fun gen#2657 ->
      (let (gen#2747, gen#2748) = gen#2657 in
       let (gen#2749, gen#2750) = gen#2747 in
       let _fn#2658 = gen#2749 in
       let _e#2659 = gen#2750 in
       let _v#2660 = gen#2748 in (poly_stub_2)@(L(unit)))[@inline] in
    let originate_from_file#490 =
      fun gen#2662 ->
      (let (gen#2751, gen#2752) = gen#2662 in
       let (gen#2753, gen#2754) = gen#2751 in
       let (gen#2757, gen#2758) = gen#2753 in
       let _fn#2663 = gen#2757 in
       let _e#2664 = gen#2758 in
       let (gen#2755, gen#2756) = gen#2754 in
       let _v#2665 = gen#2755 in
       let _s#2666 = gen#2756 in
       let _t#2667 = gen#2752 in (poly_stub_1)@(L(unit)))[@inline] in
    let assert = assert#222[@inline] in
    let abs = abs#225[@inline] in
    let is_nat = is_nat#226[@inline] in
    let true = true#227[@inline] in
    let false = false#228[@inline] in
    let unit = unit#229[@inline] in
    let assert_with_error = assert_with_error#233[@inline] in
    let x#491 = L(54) in let y#492 = x#491 in L(unit) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "storage" ; contract "module_contract_simple.mligo" ; "999" ] ;
  [%expect{| 999 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "parameter" ; contract "module_contract_simple.mligo" ; "Add 999" ] ;
  [%expect{| (Left (Left 999)) |}]

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
  run_ligo_good [ "compile" ; "contract" ; contract "global_constant.mligo" ; "--constants" ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }" ] ;
  [%expect{|
    { parameter unit ;
      storage int ;
      code { CDR ;
             constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2" ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "parameter" ; contract "global_constant.mligo" ; "()" ; "--constants" ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }" ] ;
  [%expect{| Unit |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "storage" ; contract "global_constant.mligo" ; "v" ; "--constants" ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }" ] ;
  [%expect{| 128 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "storage" ; contract "global_constant.mligo" ; "42" ; "--constants" ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "storage" ; contract "global_constant.mligo" ; "42" ; "--file-constants" ; contract_resource "const.json" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "v" ; "--init-file" ; contract "global_constant.mligo" ; "--constants" ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }" ] ;
  [%expect{| 128 |}]

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
  run_ligo_good [ "compile" ; "constant" ; "cameligo" ; "fun (x : int) -> if x > 3 then x * 2 else x * String.length \"fja\" + 1" ] ;
  [%expect{|
    Michelson constant as JSON string:
    "{ PUSH int 3 ;\n  DUP 2 ;\n  COMPARE ;\n  GT ;\n  IF { PUSH int 2 ; SWAP ; MUL }\n     { PUSH int 1 ; PUSH string \"fja\" ; SIZE ; DIG 2 ; MUL ; ADD } }"
    This string can be passed in `--constants` argument when compiling a contract.

    Remember to register it in the network, e.g.:
    > tezos-client register global constant "{ PUSH int 3 ;
      DUP 2 ;
      COMPARE ;
      GT ;
      IF { PUSH int 2 ; SWAP ; MUL }
         { PUSH int 1 ; PUSH string \"fja\" ; SIZE ; DIG 2 ; MUL ; ADD } }" from bootstrap1

    Constant hash:
    exprtr7GE1A1cR39zNGRGF44aGfAX23tC7szWrnLzs9fkUhasLEcQT |}]

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
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

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

(* Test compiling a contract with a get_entrypoint_opt to a capitalized entrypoint *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "get_capitalized_entrypoint.mligo" ] ;
  [%expect{|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             SENDER ;
             CONTRACT %Upper unit ;
             IF_NONE
               { PUSH string "lol" ; FAILWITH }
               { PUSH mutez 0 ;
                 UNIT ;
                 TRANSFER_TOKENS ;
                 UNIT ;
                 NIL operation ;
                 DIG 2 ;
                 CONS ;
                 PAIR } } } |}]

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
                 DUP 2 ;
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

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_typed_opt.mligo" ; "-e" ; "main2" ; "--enable-michelson-typed-opt" ] ;
  [%expect{|
    { parameter (or (pair %one (nat %x) (int %y)) (pair %two (nat %x) (int %y))) ;
      storage nat ;
      code { CAR ; IF_LEFT {} {} ; CAR ; NIL operation ; PAIR } }  |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_typed_opt.mligo" ; "-e" ; "main3" ; "--enable-michelson-typed-opt" ] ;
  [%expect{|
    { parameter (or (pair %onee (nat %x) (int %y)) (pair %three (nat %x) (int %z))) ;
      storage nat ;
      code { CAR ; IF_LEFT {} {} ; CAR ; NIL operation ; PAIR } }
           |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_typed_opt.mligo" ; "-e" ; "main4" ; "--enable-michelson-typed-opt" ] ;
  [%expect{|
    { parameter (or (pair %four (nat %x) (timestamp %y)) (pair %oneee (nat %x) (int %y))) ;
      storage nat ;
      code { CAR ; IF_LEFT { CAR } { CAR } ; NIL operation ; PAIR } }
           |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_typed_opt.mligo" ; "-e" ; "main2" ] ;
  [%expect{|
    { parameter (or (pair %one (nat %x) (int %y)) (pair %two (nat %x) (int %y))) ;
      storage nat ;
      code { CAR ; IF_LEFT { CAR } { CAR } ; NIL operation ; PAIR } }
           |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_typed_opt.mligo" ; "-e" ; "main3" ] ;
  [%expect{|
    { parameter (or (pair %onee (nat %x) (int %y)) (pair %three (nat %x) (int %z))) ;
      storage nat ;
      code { CAR ; IF_LEFT { CAR } { CAR } ; NIL operation ; PAIR } }
           |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "michelson_typed_opt.mligo" ; "-e" ; "main4" ] ;
  [%expect{|
    { parameter (or (pair %four (nat %x) (timestamp %y)) (pair %oneee (nat %x) (int %y))) ;
      storage nat ;
      code { CAR ; IF_LEFT { CAR } { CAR } ; NIL operation ; PAIR } }
           |}]

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
File "../../test/contracts/extend_builtin.jsligo", line 2, characters 13-14:
  1 | namespace Tezos {
  2 |   export let x = 42;
  3 |   export let f = (x  : int) : int => x + 2;

Toplevel let declaration are silently change to const declaration.

File "../../test/contracts/extend_builtin.jsligo", line 3, characters 13-14:
  2 |   export let x = 42;
  3 |   export let f = (x  : int) : int => x + 2;
  4 | }

Toplevel let declaration are silently change to const declaration.

File "../../test/contracts/extend_builtin.jsligo", line 6, characters 4-5:
  5 |
  6 | let y = Tezos.f(Tezos.x);

Toplevel let declaration are silently change to const declaration.

44 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "thunk.mligo" ] ;
  [%expect{|
{ parameter string ;
  storage string ;
  code { CDR ;
         SENDER ;
         PUSH mutez 1000000 ;
         NONE key_hash ;
         CREATE_CONTRACT
           { parameter nat ;
             storage address ;
             code { DROP ; SENDER ; NIL operation ; PAIR } } ;
         PAIR ;
         SWAP ;
         NIL operation ;
         DIG 2 ;
         CAR ;
         CONS ;
         PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "reasonligo" ; "y" ; "--init-file" ; contract "extend_builtin.religo" ] ;
  [%expect{|
Reasonligo is depreacted, support will be dropped in a few versions.

Reasonligo is depreacted, support will be dropped in a few versions.

Reasonligo is depreacted, support will be dropped in a few versions.

44 |}]

(* check compiling many (more than 10) views *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "views_many.ligo" ] ;
  [%expect{|
{ parameter unit ;
  storage nat ;
  code { CDR ; NIL operation ; PAIR } ;
  view "view_1" unit int { CDR ; PUSH int 1 ; ADD } ;
  view "view_2" unit int { CDR ; PUSH int 2 ; ADD } ;
  view "view_3" unit int { CDR ; PUSH int 3 ; ADD } ;
  view "view_4" unit int { CDR ; PUSH int 4 ; ADD } ;
  view "view_5" unit int { CDR ; PUSH int 5 ; ADD } ;
  view "view_6" unit int { CDR ; PUSH int 6 ; ADD } ;
  view "view_7" unit int { CDR ; PUSH int 7 ; ADD } ;
  view "view_8" unit int { CDR ; PUSH int 8 ; ADD } ;
  view "view_9" unit int { CDR ; PUSH int 9 ; ADD } ;
  view "view_10" unit int { CDR ; PUSH int 10 ; ADD } ;
  view "view_11" unit int { CDR ; PUSH int 11 ; ADD } } |}]

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

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "shadowed_sum_type.mligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/shadowed_sum_type.mligo", line 13, characters 8-12:
     12 |
     13 | let x = A 42
     14 |

    Constructor "A" not found.
  |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "bad_contract_return_type.mligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/bad_contract_return_type.mligo", line 5, characters 4-8:
      4 |
      5 | let main (_,s : paramater * storage) : _return =
      6 |     [], s, 1tez

    Invalid type for entrypoint "main".
    An entrypoint must of type "parameter * storage -> operation list * storage".
  |}]

(* ignore in JsLIGO *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ;  "jsligo" ; "test" ; "--init-file" ; contract "ignore.jsligo" ] ;
  [%expect{| 1 |}]
