open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_function_annotation_1.mligo"];
  [%expect {|
    Invalid type(s)
    Cannot unify int with unit. |}];

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_function_annotation_2.mligo"; "--entry-point"; "f"];
  [%expect {|
    Invalid type(s)
    Cannot unify ( int * int ) -> int with int. |}];

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_function_annotation_3.mligo"; "--entry-point"; "f"];
  [%expect {|
    File "../../test/contracts/negative/error_function_annotation_3.mligo", line 2, character 4 to line 3, character 14:
      1 | type op =
      2 |   | Add of int
      3 |   | Sub of int
      4 |

    Invalid type(s)
    Cannot unify sum[Add -> int , Sub -> int] with ( list (operation) * sum[Add -> int , Sub -> int] ). |}];

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_no_tail_recursive_function.mligo"; "--entry-point"; "unvalid"];
  [%expect {|
    File "../../test/contracts/negative/error_no_tail_recursive_function.mligo", line 2, characters 14-21:
      1 | let rec unvalid (n:int):int =
      2 |     let res = unvalid (n) in
      3 |     res + 1

    Recursive call not in tail position.
    The value of a recursive call must be immediately returned by the defined function. |}];

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_type.ligo" ] ;
  [%expect {|
    Invalid type(s)
    Cannot unify bls12_381_g1 with int. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_type_record_access.mligo" ] ;
  [%expect {|
    Invalid type(s)
    Cannot unify int with bool. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_type_record_update.mligo" ] ;
  [%expect {|
    Invalid type(s)
    Cannot unify bool with int. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_1.mligo" ] ;
  [%expect {|
    Invalid type(s)
    Cannot unify int with string. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_2.mligo" ] ;
  [%expect {|
    Invalid type(s)
    Cannot unify option (int) with list (string). |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_3.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_3.mligo", line 1, characters 13-25:
      1 | type toto = (int * string)
      2 |

    Invalid type(s)
    Cannot unify ( int * string ) with ( int * string * bool ). |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_4.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_4.mligo", line 1, characters 12-47:
      1 | type toto = { a : int ; b : string ; c : bool }
      2 | type tata = { a : int ; d : string ; c : bool }

    Invalid type(s)
    Cannot unify record[a -> int , b -> string , c -> bool] with record[a -> int , c -> bool , d -> string]. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_5.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_5.mligo", line 1, characters 10-17:
      1 | let foo : boolean = 3
      2 |

    Type "boolean" not found. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_6.mligo" ] ;
  [%expect {|
    Invalid type(s)
    Cannot unify bool with string. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_7.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_7.mligo", line 4, characters 18-48:
      3 |
      4 | let foo : tata = ({a = 1 ; b = "foo" ; c = true} : toto)
      5 |

    Mismatching record labels. Expected record of type record[a -> int , b -> string]. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_1.jsligo" ] ;
  [%expect {|
    Invalid type(s)
    Cannot unify ( nat * nat ) with nat. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/id.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/id.mligo", line 3, character 18 to line 7, character 1:
      2 |
      3 | type id_details = {
      4 |   owner: address;
      5 |   controller: address;
      6 |   profile: bytes;
      7 | }
      8 |

    Invalid type(s)
    Cannot unify record[controller -> address , owner -> address , profile -> bytes] with option (^gen#238). |}]

(*
  This test is here to ensure compatibility with comparable pairs introduced in carthage
  note that only "comb pairs" are allowed to be compared (would be better if any pair would be comparable ?)
  EDIT: With EDO, all kind of pairs are comparable
*)
let%expect_test _ =
  run_ligo_good [ "run"; "interpret" ; "Set.literal [ (1,(2,3)) ; (2,(3,4)) ]" ; "--syntax"; "cameligo" ] ;
  [%expect {|
    SET_ADD(( 2 , ( 3 , 4 ) ) , SET_ADD(( 1 , ( 2 , 3 ) ) , SET_EMPTY())) |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/invalid_field_record_update.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/invalid_field_record_update.mligo", line 4, characters 29-36:
      3 | let main (p:int) (storage : abc) =
      4 |   (([] : operation list) , { storage with nofield=2048} )

    Invalid record field "nofield" in record. |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/override_option.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/override_option.mligo", line 3, characters 53-57:
      2 |
      3 | let main (x,y:bool * bool) = ([] : operation list), (None : option)

    Constructor "None" not found. |} ]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/will_be_ignored.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/will_be_ignored.mligo", line 7, characters 47-55:
      6 |      let receiver : contract =
      7 |       match (Tezos.get_contract_opt(s.owner) : contract option) with
      8 |         Some (contract) -> contract

    Type is applied to a wrong number of arguments, expected: 1 got: 0 |}]

(* Compiles due to inference ;) *)
(* let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_contract_type_inference.mligo" ] ;
  [%expect {|
      File "../../test/contracts/negative/error_contract_type_inference.mligo", line 8, characters 12-69:
        7 |     Some contract -> contract
        8 |   | None -> (failwith "The entrypoint does not exist" : int contract)
        9 |
  
      Invalid type(s).
      Expected: "contract ('a)", but got: "contract (int)". |}] *)