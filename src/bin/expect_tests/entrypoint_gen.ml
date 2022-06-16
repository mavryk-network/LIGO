open Cli_expect

let contract name = "../../test/contracts/entrypoint_gen/" ^ name

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "FA1.2.mligo" ] ;
  [%expect{|
    { parameter
        (or (pair %transfer (address %from) (address %to) (nat %value))
            (or (pair %approve (address %spender) (nat %value))
                (or (pair %getAllowance
                       (pair %request (address %owner) (address %spender))
                       (contract %callback nat))
                    (or (pair %getBalance (address %owner) (contract %callback nat))
                        (pair %getTotalSupply (unit %request) (contract %callback nat)))))) ;
      storage
        (pair (pair (big_map %allowances (pair (address %owner) (address %spender)) nat)
                    (big_map %tokens address nat))
              (nat %total_supply)) ;
      code { UNPAIR ;
             IF_LEFT
               { DUP 2 ;
                 CAR ;
                 CAR ;
                 DUP 3 ;
                 CAR ;
                 CDR ;
                 DUP 3 ;
                 CAR ;
                 SENDER ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP }
                    { SENDER ;
                      DUP 4 ;
                      CAR ;
                      PAIR ;
                      DUP 4 ;
                      GET 4 ;
                      DUP 4 ;
                      DUP 3 ;
                      GET ;
                      IF_NONE { PUSH nat 0 } {} ;
                      SUB ;
                      ISNAT ;
                      IF_NONE { PUSH string "NotEnoughAllowance" ; FAILWITH } {} ;
                      DIG 3 ;
                      PUSH nat 0 ;
                      DUP 3 ;
                      COMPARE ;
                      EQ ;
                      IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                      DIG 2 ;
                      UPDATE } ;
                 DUP 3 ;
                 GET 4 ;
                 DUP 3 ;
                 DUP 5 ;
                 CAR ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 SUB ;
                 ISNAT ;
                 IF_NONE { PUSH string "NotEnoughBalance" ; FAILWITH } {} ;
                 DIG 2 ;
                 PUSH nat 0 ;
                 DUP 3 ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                 DUP 4 ;
                 CAR ;
                 UPDATE ;
                 DUP 3 ;
                 GET 4 ;
                 DUP 2 ;
                 DUP 5 ;
                 GET 3 ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 ADD ;
                 DUP 5 ;
                 CDR ;
                 DIG 2 ;
                 PUSH nat 0 ;
                 DUP 4 ;
                 COMPARE ;
                 EQ ;
                 IF { DIG 2 ; DROP ; NONE nat } { DIG 2 ; SOME } ;
                 DIG 4 ;
                 GET 3 ;
                 UPDATE ;
                 DIG 3 ;
                 DROP ;
                 DIG 2 ;
                 PAIR ;
                 PAIR ;
                 NIL operation }
               { IF_LEFT
                   { DUP 2 ;
                     CAR ;
                     CAR ;
                     DUP 2 ;
                     CAR ;
                     SENDER ;
                     PAIR ;
                     PUSH nat 0 ;
                     DUP 4 ;
                     CDR ;
                     COMPARE ;
                     GT ;
                     PUSH nat 0 ;
                     DUP 4 ;
                     DUP 4 ;
                     GET ;
                     IF_NONE { PUSH nat 0 } {} ;
                     COMPARE ;
                     GT ;
                     AND ;
                     IF { PUSH string "UnsafeAllowanceChange" ; FAILWITH } {} ;
                     DUP 4 ;
                     CDR ;
                     DIG 4 ;
                     CAR ;
                     CDR ;
                     DIG 4 ;
                     CDR ;
                     DIG 4 ;
                     PUSH nat 0 ;
                     DUP 3 ;
                     COMPARE ;
                     EQ ;
                     IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                     DIG 4 ;
                     UPDATE ;
                     PAIR ;
                     PAIR ;
                     NIL operation }
                   { IF_LEFT
                       { DUP 2 ;
                         NIL operation ;
                         DUP 3 ;
                         CDR ;
                         PUSH mutez 0 ;
                         DIG 5 ;
                         CAR ;
                         CAR ;
                         DIG 5 ;
                         CAR ;
                         GET ;
                         IF_NONE { PUSH nat 0 } {} ;
                         TRANSFER_TOKENS }
                       { IF_LEFT
                           { DUP 2 ;
                             NIL operation ;
                             DUP 3 ;
                             CDR ;
                             PUSH mutez 0 ;
                             DIG 5 ;
                             CAR ;
                             CDR ;
                             DIG 5 ;
                             CAR ;
                             GET ;
                             IF_NONE { PUSH nat 0 } {} ;
                             TRANSFER_TOKENS }
                           { DUP 2 ;
                             NIL operation ;
                             DIG 2 ;
                             CDR ;
                             PUSH mutez 0 ;
                             DIG 4 ;
                             CDR ;
                             TRANSFER_TOKENS } } ;
                     CONS } } ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "FA1.2.mligo" ; "-e" ; "transfer" ] ;
  [%expect{|
    { parameter (pair (address %from) (address %to) (nat %value)) ;
      storage
        (pair (pair (big_map %allowances (pair (address %owner) (address %spender)) nat)
                    (big_map %tokens address nat))
              (nat %total_supply)) ;
      code { UNPAIR ;
             DUP 2 ;
             CAR ;
             CAR ;
             DUP 3 ;
             CAR ;
             CDR ;
             DUP 3 ;
             CAR ;
             SENDER ;
             COMPARE ;
             EQ ;
             IF { SWAP }
                { SENDER ;
                  DUP 4 ;
                  CAR ;
                  PAIR ;
                  DUP 4 ;
                  GET 4 ;
                  DUP 4 ;
                  DUP 3 ;
                  GET ;
                  IF_NONE { PUSH nat 0 } {} ;
                  SUB ;
                  ISNAT ;
                  IF_NONE { PUSH string "NotEnoughAllowance" ; FAILWITH } {} ;
                  DIG 3 ;
                  PUSH nat 0 ;
                  DUP 3 ;
                  COMPARE ;
                  EQ ;
                  IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                  DIG 2 ;
                  UPDATE } ;
             DUP 3 ;
             GET 4 ;
             DUP 3 ;
             DUP 5 ;
             CAR ;
             GET ;
             IF_NONE { PUSH nat 0 } {} ;
             SUB ;
             ISNAT ;
             IF_NONE { PUSH string "NotEnoughBalance" ; FAILWITH } {} ;
             DIG 2 ;
             PUSH nat 0 ;
             DUP 3 ;
             COMPARE ;
             EQ ;
             IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
             DUP 4 ;
             CAR ;
             UPDATE ;
             DUP 3 ;
             GET 4 ;
             DUP 2 ;
             DUP 5 ;
             GET 3 ;
             GET ;
             IF_NONE { PUSH nat 0 } {} ;
             ADD ;
             DUP 5 ;
             CDR ;
             DIG 2 ;
             PUSH nat 0 ;
             DUP 4 ;
             COMPARE ;
             EQ ;
             IF { DIG 2 ; DROP ; NONE nat } { DIG 2 ; SOME } ;
             DIG 4 ;
             GET 3 ;
             UPDATE ;
             DIG 3 ;
             DROP ;
             DIG 2 ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "FA1.2.mligo" ; "-e" ; "transfer,approve" ] ;
  [%expect{|
    { parameter
        (or (pair %approve (address %spender) (nat %value))
            (pair %transfer (address %from) (address %to) (nat %value))) ;
      storage
        (pair (pair (big_map %allowances (pair (address %owner) (address %spender)) nat)
                    (big_map %tokens address nat))
              (nat %total_supply)) ;
      code { UNPAIR ;
             IF_LEFT
               { DUP 2 ;
                 CAR ;
                 CAR ;
                 DUP 2 ;
                 CAR ;
                 SENDER ;
                 PAIR ;
                 PUSH nat 0 ;
                 DUP 4 ;
                 CDR ;
                 COMPARE ;
                 GT ;
                 PUSH nat 0 ;
                 DUP 4 ;
                 DUP 4 ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 COMPARE ;
                 GT ;
                 AND ;
                 IF { PUSH string "UnsafeAllowanceChange" ; FAILWITH } {} ;
                 DUP 4 ;
                 CDR ;
                 DIG 4 ;
                 CAR ;
                 CDR ;
                 DIG 4 ;
                 CDR ;
                 DIG 4 ;
                 PUSH nat 0 ;
                 DUP 3 ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                 DIG 4 ;
                 UPDATE }
               { DUP 2 ;
                 CAR ;
                 CAR ;
                 DUP 3 ;
                 CAR ;
                 CDR ;
                 DUP 3 ;
                 CAR ;
                 SENDER ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP }
                    { SENDER ;
                      DUP 4 ;
                      CAR ;
                      PAIR ;
                      DUP 4 ;
                      GET 4 ;
                      DUP 4 ;
                      DUP 3 ;
                      GET ;
                      IF_NONE { PUSH nat 0 } {} ;
                      SUB ;
                      ISNAT ;
                      IF_NONE { PUSH string "NotEnoughAllowance" ; FAILWITH } {} ;
                      DIG 3 ;
                      PUSH nat 0 ;
                      DUP 3 ;
                      COMPARE ;
                      EQ ;
                      IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                      DIG 2 ;
                      UPDATE } ;
                 DUP 3 ;
                 GET 4 ;
                 DUP 3 ;
                 DUP 5 ;
                 CAR ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 SUB ;
                 ISNAT ;
                 IF_NONE { PUSH string "NotEnoughBalance" ; FAILWITH } {} ;
                 DIG 2 ;
                 PUSH nat 0 ;
                 DUP 3 ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                 DUP 4 ;
                 CAR ;
                 UPDATE ;
                 DUP 3 ;
                 GET 4 ;
                 DUP 2 ;
                 DUP 5 ;
                 GET 3 ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 ADD ;
                 DUP 5 ;
                 CDR ;
                 DIG 2 ;
                 PUSH nat 0 ;
                 DUP 4 ;
                 COMPARE ;
                 EQ ;
                 IF { DIG 2 ; DROP ; NONE nat } { DIG 2 ; SOME } ;
                 DIG 4 ;
                 GET 3 ;
                 UPDATE ;
                 DIG 3 ;
                 DROP ;
                 DIG 2 } ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

(* entrypoint doesn't exist *)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "FA1.2.mligo" ; "-e" ; "transfert" ] ;
  [%expect {|
    Internal error: Entrypoint transfert does not exist |}]
