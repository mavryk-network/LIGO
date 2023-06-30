open Cli_expect

let test basename = "../../test/contracts/contract_metadata/" ^ basename

(* ========================================================================== *)
(* Check of storage type *)

(* -------------------------------------------------------------------------- *)
(* Contracts without a 'metadata' field should not be checked. *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "no_metadata.mligo" ];
  [%expect {| { parameter int ; storage int ; code { CDR ; NIL operation ; PAIR } } |}];
  let storage = "42" in
  run_ligo_good [ "compile"; "storage"; test "no_metadata.mligo"; storage ];
  [%expect {| 42 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "no_metadata.jsligo" ];
  [%expect {| { parameter int ; storage int ; code { CDR ; NIL operation ; PAIR } } |}];
  let storage = "42" in
  run_ligo_good [ "compile"; "storage"; test "no_metadata.jsligo"; storage ];
  [%expect {| 42 |}]

(* -------------------------------------------------------------------------- *)
(* Contracts with 'metadata' of invalid type should raise warning *)
let%expect_test _ =
  let entrypoint = "entry_invalid_metadata_1" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.mligo"; "-e"; entrypoint ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 16, characters 15-18:
     15 |   { data     : int
     16 |   ; metadata : nat
                         ^^^
     17 |   }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.

    { parameter int ;
      storage (pair (int %data) (nat %metadata)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage = "{ data  = 42; metadata = 33n}" in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.mligo"; "-e"; entrypoint; storage ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 16, characters 15-18:
     15 |   { data     : int
     16 |   ; metadata : nat
                         ^^^
     17 |   }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.

    (Pair 42 33) |}]

let%expect_test _ =
  let entrypoint = "entry_invalid_metadata_1" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.jsligo"; "-e"; entrypoint ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.jsligo", line 17, characters 12-15:
     16 |   data: int,
     17 |   metadata: nat,
                      ^^^
     18 | };
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      big_map<string, bytes>
    You can disable this warning with the '--no-metadata-check' flag.

    { parameter int ;
      storage (pair (int %data) (nat %metadata)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage = "{ data  : 42, metadata : (33 as nat)}" in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.jsligo"; "-e"; entrypoint; storage ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.jsligo", line 17, characters 12-15:
     16 |   data: int,
     17 |   metadata: nat,
                      ^^^
     18 | };
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      big_map<string, bytes>
    You can disable this warning with the '--no-metadata-check' flag.

    (Pair 42 33) |}]

(* -------------------------------------------------------------------------- *)
(* Contracts with 'metadata' of invalid type should raise warning *)
let%expect_test _ =
  let entrypoint = "entry_invalid_metadata_2" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.mligo"; "-e"; entrypoint ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 25, characters 15-30:
     24 |   { data     : int
     25 |   ; metadata : (bytes, string) big_map
                         ^^^^^^^^^^^^^^^
     26 |   }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.

    { parameter int ;
      storage (pair (int %data) (big_map %metadata bytes string)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage =
    "{ data  = 42 ; metadata = Big_map.literal [ (0x42, \"toto\") ; (0x24, \"titi\") ] }"
  in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.mligo"; "-e"; entrypoint; storage ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 25, characters 15-30:
     24 |   { data     : int
     25 |   ; metadata : (bytes, string) big_map
                         ^^^^^^^^^^^^^^^
     26 |   }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.

    (Pair 42 { Elt 0x24 "titi" ; Elt 0x42 "toto" }) |}]

let%expect_test _ =
  let entrypoint = "entry_invalid_metadata_2" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.jsligo"; "-e"; entrypoint ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.jsligo", line 29, characters 12-34:
     28 |   data: int,
     29 |   metadata: big_map<bytes, string>;
                      ^^^^^^^^^^^^^^^^^^^^^^
     30 | };
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      big_map<string, bytes>
    You can disable this warning with the '--no-metadata-check' flag.

    { parameter int ;
      storage (pair (int %data) (big_map %metadata bytes string)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage =
    "{ data : 42, metadata : Big_map.literal (list([[0x42, \"toto\"],[0x24, \"titi\"]]))}"
  in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.jsligo"; "-e"; entrypoint; storage ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.jsligo", line 29, characters 12-34:
     28 |   data: int,
     29 |   metadata: big_map<bytes, string>;
                      ^^^^^^^^^^^^^^^^^^^^^^
     30 | };
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      big_map<string, bytes>
    You can disable this warning with the '--no-metadata-check' flag.

    (Pair 42 { Elt 0x24 "titi" ; Elt 0x42 "toto" }) |}]

(* -------------------------------------------------------------------------- *)
(* Contracts with 'metadata' annotation of invalid type should raise warning *)
let%expect_test _ =
  let entrypoint = "entry_invalid_metadata_3" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.mligo"; "-e"; entrypoint ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 34, characters 32-47:
     33 |   { data     : int
     34 |   ; [@annot metadata] notdata : (bytes, string) big_map
                                          ^^^^^^^^^^^^^^^
     35 |   }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.

    { parameter int ;
      storage (pair (int %data) (big_map %metadata bytes string)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage =
    "{ data  = 42 ; notdata = Big_map.literal [ (0x42, \"toto\") ; (0x24, \"titi\") ] }"
  in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.mligo"; "-e"; entrypoint; storage ];
  [%expect
    {|
    File "../../test/contracts/contract_metadata/metadata_tzip16.mligo", line 34, characters 32-47:
     33 |   { data     : int
     34 |   ; [@annot metadata] notdata : (bytes, string) big_map
                                          ^^^^^^^^^^^^^^^
     35 |   }
    :
    Warning: If the following metadata is meant to be TZIP-16 compliant,
    then it should be a 'big_map' from 'string' to 'bytes'.
    Hint: The corresponding type should be :
      (string, bytes) big_map
    You can disable this warning with the '--no-metadata-check' flag.

    (Pair 42 { Elt 0x24 "titi" ; Elt 0x42 "toto" }) |}]

(* -------------------------------------------------------------------------- *)
(* Contracts with valid 'metadata' type should pass *)
let%expect_test _ =
  let entrypoint = "entry_valid_metadata" in
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-e"
    ; entrypoint
    ; "good_storage"
    ];
  [%expect
    {|
    (Pair 42
          { Elt "" 0x74657a6f732d73746f726167653a68656c6c6f253246776f726c64 ;
            Elt "hello/world"
                0x207b0a2020226e616d65223a22464132204e4654204d61726b6574706c616365222c0a2020226465736372697074696f6e223a224578616d706c65206f662046413220696d706c656d656e746174696f6e222c0a20202276657273696f6e223a22302e302e31222c0a2020226c6963656e7365223a7b226e616d65223a224d4954227d2c0a202022617574686f7273223a5b224d617269676f6c643c636f6e74616374406d617269676f6c642e6465763e225d2c0a202022686f6d6570616765223a2268747470733a2f2f6d617269676f6c642e646576222c0a202022736f75726365223a7b0a202022746f6f6c73223a5b224c69676f225d2c0a2020226c6f636174696f6e223a2268747470733a2f2f6769746875622e636f6d2f6c69676f6c616e672f636f6e74726163742d636174616c6f6775652f747265652f6d61696e2f6c69622f666132227d2c0a202022696e7465726661636573223a5b22545a49502d303132225d2c0a2020226572726f7273223a205b5d2c0a2020227669657773223a205b5d0a20207d0a2020 }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-e"
    ; entrypoint
    ; "bad_storage0"
    ];
  [%expect
    {|
    Warning: Slash ('/') not in a valid position in URI: "hello/invalid_not_http", use instead "%2F".
    (Pair 42
          { Elt "" 0x74657a6f732d73746f726167653a68656c6c6f2f696e76616c69645f6e6f745f68747470 ;
            Elt "hello/world" 0x4a534f4e3f ;
            Elt "invalid_not_http" 0x68747470733a2f2f7777772e6578616d706c652e636f6d ;
            Elt "invalid_trailing_slash"
                0x697066733a2f2f516d577169337542684251354b55367352314c704c714a5472344778755066454b3755447976364763633366484c2f ;
            Elt "invalid_wrong_hash" 0x697066733a2f2f696e76616c69642d68617368 }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-e"
    ; entrypoint
    ; "bad_storage1"
    ];
  [%expect
    {|
     Warning: Could not find a valid URI haha in storage's metadata empty key.
    (Pair 42
          { Elt "" 0x68616861 ;
            Elt "hello/world" 0x687474703a2f2f7777772e6578616d706c652e636f6d }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-e"
    ; entrypoint
    ; "bad_storage2"
    ];
  [%expect
    {|
     Warning: Could not find key haha in storage's metadata.
    (Pair 42
          { Elt "" 0x74657a6f732d73746f726167653a68616861 ;
            Elt "hello/world" 0x687474703a2f2f7777772e6578616d706c652e636f6d }) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "-e"
    ; entrypoint
    ; "bad_storage3"
    ];
  [%expect
    {|
    Warning: Could not parse JSON in storage's metadata: "Line 1, bytes 0-7:
    Invalid token 'nojson!'".
    (Pair 42
          { Elt "" 0x74657a6f732d73746f726167653a68616861 ; Elt "haha" 0x6e6f6a736f6e21 }) |}]

let%expect_test _ =
  let entrypoint = "entry_valid_metadata" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.mligo"; "-e"; entrypoint ];
  [%expect
    {|
    { parameter int ;
      storage (pair (int %data) (big_map %metadata string bytes)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage =
    "{ data  = 42 ; metadata = Big_map.literal [ (\"toto\", 0x42) ; (\"titi\", 0x24) ] }"
  in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.mligo"; "-e"; entrypoint; storage ];
  [%expect
    {|
     Warning: Empty key in metadata big-map is mandatory.
    (Pair 42 { Elt "titi" 0x24 ; Elt "toto" 0x42 }) |}]

let%expect_test _ =
  let entrypoint = "entry_valid_metadata" in
  run_ligo_good [ "compile"; "contract"; test "metadata_tzip16.jsligo"; "-e"; entrypoint ];
  [%expect
    {|
    { parameter int ;
      storage (pair (int %data) (big_map %metadata string bytes)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage =
    "{ data : 42, metadata : Big_map.literal (list([[\"toto\", 0x42],[\"titi\", 0x24]]))}"
  in
  run_ligo_good
    [ "compile"; "storage"; test "metadata_tzip16.jsligo"; "-e"; entrypoint; storage ];
  [%expect
    {|
     Warning: Empty key in metadata big-map is mandatory.
    (Pair 42 { Elt "titi" 0x24 ; Elt "toto" 0x42 }) |}]

(* -------------------------------------------------------------------------- *)
(* Contracts with invalid 'metadata' should pass when the waiver flag is enabled *)

let%expect_test _ =
  let entrypoint = "entry_invalid_metadata_1" in
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "metadata_tzip16.mligo"
    ; "--no-metadata-check"
    ; "-e"
    ; entrypoint
    ];
  [%expect
    {|
    { parameter int ;
      storage (pair (int %data) (nat %metadata)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage = "{ data  = 42; metadata = 33n}" in
  run_ligo_good
    [ "compile"
    ; "storage"
    ; test "metadata_tzip16.mligo"
    ; "--no-metadata-check"
    ; "-e"
    ; entrypoint
    ; storage
    ];
  [%expect {| (Pair 42 33) |}]

let%expect_test _ =
  let entrypoint = "entry_invalid_metadata_1" in
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "metadata_tzip16.jsligo"
    ; "--no-metadata-check"
    ; "-e"
    ; entrypoint
    ];
  [%expect
    {|
    { parameter int ;
      storage (pair (int %data) (nat %metadata)) ;
      code { CDR ; NIL operation ; PAIR } } |}];
  let storage = "{ data  : 42, metadata : (33 as nat)}" in
  run_ligo_good
    [ "compile"
    ; "storage"
    ; "--no-metadata-check"
    ; test "metadata_tzip16.jsligo"
    ; "-e"
    ; entrypoint
    ; storage
    ];
  [%expect {| (Pair 42 33) |}]
