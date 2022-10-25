open Cli_expect

let%expect_test _ =
  run_ligo_good ["compile" ; "contract" ; test "timelock.mligo" ] ;
  [%expect {|
    Warning: Error(s) occurred while type checking the produced michelson contract:
    { "id": "proto.015-PtLimaPt.michelson_v1.ill_formed_type",
      "description":
        "The toplevel error thrown when trying to parse a type expression (always followed by more precise errors).",
      "data":
        { "identifier": "parameter",
          "ill_formed_expression":
            [ { "prim": "parameter",
                "args":
                  [ { "prim": "pair",
                      "args": [ { "prim": "chest_key" }, { "prim": "chest" } ] } ] },
              { "prim": "storage", "args": [ { "prim": "bytes" } ] },
              { "prim": "code",
                "args":
                  [ [ { "prim": "CAR" }, { "prim": "UNPAIR" },
                      { "prim": "PUSH",
                        "args": [ { "prim": "nat" }, { "int": "1000" } ] },
                      { "prim": "DUG", "args": [ { "int": "2" } ] },
                      { "prim": "OPEN_CHEST" },
                      { "prim": "IF_LEFT",
                        "args":
                          [ [ { "prim": "RIGHT",
                                "args":
                                  [ { "prim": "or",
                                      "args":
                                        [ { "prim": "unit" },
                                          { "prim": "unit" } ] } ] } ],
                            [ { "prim": "IF",
                                "args":
                                  [ [ { "prim": "UNIT" },
                                      { "prim": "LEFT",
                                        "args": [ { "prim": "unit" } ] } ],
                                    [ { "prim": "UNIT" },
                                      { "prim": "RIGHT",
                                        "args": [ { "prim": "unit" } ] } ] ] },
                              { "prim": "LEFT", "args": [ { "prim": "bytes" } ] } ] ] },
                      { "prim": "IF_LEFT",
                        "args":
                          [ [ { "prim": "IF_LEFT",
                                "args":
                                  [ [ { "prim": "DROP" },
                                      { "prim": "PUSH",
                                        "args":
                                          [ { "prim": "bytes" },
                                            { "bytes": "01" } ] } ],
                                    [ { "prim": "DROP" },
                                      { "prim": "PUSH",
                                        "args":
                                          [ { "prim": "bytes" },
                                            { "bytes": "00" } ] } ] ] } ], [] ] },
                      { "prim": "NIL", "args": [ { "prim": "operation" } ] },
                      { "prim": "PAIR" } ] ] } ], "location": 2 } }
    { "id": "proto.015-PtLimaPt.michelson_v1.deprecated_instruction",
      "description":
        "A deprecated instruction usage is disallowed in newly created contracts",
      "data": { "prim": "chest_key" } }
    Note: You compiled your contract with protocol jakarta although we internally use protocol kathmandu to typecheck the produced Michelson contract
    so you might want to ignore this error if related to a breaking change in protocol kathmandu

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