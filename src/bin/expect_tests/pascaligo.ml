(* MAVRYK: PascaLIGO. CLI snapshot tests for the restored PascaLIGO syntax:
   contract compilation (incl. multi-entry [@entry] and [@view]), expression
   compilation across the core data types, and a syntax-error message. *)

open Cli_expect

(* Multi-entry [@entry] dispatch compiles to an [or]-parameter. *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "increment.ligo" ];
  [%expect
    {|
    { parameter (or (unit %reset) (or (int %decrement) (int %increment))) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { DROP 2 ; PUSH int 0 } { IF_LEFT { SWAP ; SUB } { ADD } } ;
             NIL operation ;
             PAIR } } |}]

(* Records, maps, options, currency and [@view] declarations. *)
let%expect_test _ =
  run_ligo_good [ "info"; "list-declarations"; test "registry.ligo" ];
  [%expect
    {|
    ../../test/contracts/registry.ligo declarations:
    $contract
    $views
    $main
    total_supply
    balance_of
    deposit |}]

(* Currency: 1 mav = 1_000_000 mumav. *)
let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "pascaligo"; "10mumav + 1mav" ];
  [%expect {| 1000010 |}]

(* The [=/=] not-equal operator (regression test for the shared-lexer fix). *)
let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "pascaligo"; "3 =/= 4" ];
  [%expect {| True |}]

(* List / set / map literals. *)
let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "pascaligo"; "list [1; 2; 3]" ];
  [%expect {| { 1 ; 2 ; 3 } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "pascaligo"; "map [1 -> 2; 3 -> 4]" ];
  [%expect {| { Elt 1 2 ; Elt 3 4 } |}]

(* Functional record update. *)
let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "pascaligo"; "(record [a = 1; b = 2]) with record [a = 5]" ];
  [%expect {| (Pair 5 2) |}]

(* Constructors. *)
let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "pascaligo"; "Some (5)" ];
  [%expect {| (Some 5) |}]

(* A syntax error produces a PascaLIGO parser message. *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "pascaligo"; "record [a = ]" ];
  [%expect
    {|
    Ill-formed record expression or record patch.
    At this point, the right-hand side of the field assignment is expected
    as an expression. |}]

(* --- M3: full-parity syntax extensions (see PASCALIGO_SYNTAX_RFC.md) --- *)

(* Module signatures + module-type annotation compile through (the signature is a
   compile-time constraint, erased in Michelson). *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "pascaligo_m3.ligo" ];
  [%expect
    {|
    { parameter (or (unit %reset) (int %add)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { DROP 2 ; PUSH int 0 } { ADD } ;
             NIL operation ;
             PAIR } } |}]

(* Dynamic entrypoints: the punned `dynamic_entrypoints` field adds the dispatch big_map. *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "pascaligo_dyn_entry.ligo"; "-m"; "C" ];
  [%expect
    {|
    { parameter unit ;
      storage (pair (int %storage) (big_map %dynamic_entrypoints nat bytes)) ;
      code { CDR ; PUSH int -1 ; UPDATE 1 ; NIL operation ; PAIR } } |}]

(* contract_of + parameter_of: originate a PascaLIGO module as a contract inside a test. *)
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "pascaligo_contract_of.ligo" ];
  [%expect {|
    File "../../test/contracts/pascaligo_contract_of.ligo", line 15, characters 17-31:
     14 |   block {
     15 |     const orig = Test.originate(contract_of C, 0, 0mumav);
                           ^^^^^^^^^^^^^^
     16 |     const _r = Test.transfer_exn(orig.addr, Add(5), 0mumav);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/pascaligo_contract_of.ligo", line 16, characters 15-32:
     15 |     const orig = Test.originate(contract_of C, 0, 0mumav);
     16 |     const _r = Test.transfer_exn(orig.addr, Add(5), 0mumav);
                         ^^^^^^^^^^^^^^^^^
     17 |     const s = Test.get_storage(orig.addr);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/pascaligo_contract_of.ligo", line 17, characters 14-30:
     16 |     const _r = Test.transfer_exn(orig.addr, Add(5), 0mumav);
     17 |     const s = Test.get_storage(orig.addr);
                        ^^^^^^^^^^^^^^^^
     18 |   } with assert (s = 5)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/pascaligo_contract_of.ligo", line 18, characters 9-15:
     17 |     const s = Test.get_storage(orig.addr);
     18 |   } with assert (s = 5)
                   ^^^^^^
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_add exited with value (). |}]

(* Cross-syntax interop: a CameLIGO test imports a PascaLIGO module and `contract_of`-s it. *)
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "pascaligo_interop.mligo" ];
  [%expect {|
    File "../../test/contracts/pascaligo_interop.mligo", line 6, characters 13-27:
      5 | let test =
      6 |   let orig = Test.originate (contract_of P.C) 0 0mav in
                       ^^^^^^^^^^^^^^
      7 |   let _ = Test.transfer_exn orig.addr (Add 5) 0mav in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/pascaligo_interop.mligo", line 7, characters 10-27:
      6 |   let orig = Test.originate (contract_of P.C) 0 0mav in
      7 |   let _ = Test.transfer_exn orig.addr (Add 5) 0mav in
                    ^^^^^^^^^^^^^^^^^
      8 |   assert (Test.get_storage orig.addr = 5)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/pascaligo_interop.mligo", line 8, characters 2-8:
      7 |   let _ = Test.transfer_exn orig.addr (Add 5) 0mav in
      8 |   assert (Test.get_storage orig.addr = 5)
            ^^^^^^
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/pascaligo_interop.mligo", line 8, characters 10-26:
      7 |   let _ = Test.transfer_exn orig.addr (Add 5) 0mav in
      8 |   assert (Test.get_storage orig.addr = 5)
                    ^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

(* --- Union types "t1 | t2 | ..." (the last CameLIGO/JsLIGO grammar-parity gap). An anonymous
   union lowers to a sum (Union.Injection), so a coercion injects the value into an [or]. The
   uppercase-ctor / lowercase-type lexical split keeps "A | B" a variant (see the constructor test
   above), so these never collide. --- *)

(* First member injects Left. *)
let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "pascaligo"; "(42 : int | string)" ];
  [%expect {| (Left 42) |}]

(* Second member injects Right. *)
let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "pascaligo"; {|("hi" : int | string)|} ];
  [%expect {| (Right "hi") |}]

(* Three members nest the [or]. *)
let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "pascaligo"; "(1n : int | nat | string)" ];
  [%expect {| (Right (Left 1)) |}]

(* A malformed union (missing member after "|") produces the tailored parser message. *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "pascaligo"; "(42 : int | )" ];
  [%expect
    {|
    Ill-formed union type.
    At this point, a type expression is expected, as a member to the right of the vertical bar "|". |}]
