open Cli_expect

let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

(* avoid pretty printing *)
let () = Unix.putenv ~key:"TERM" ~data:"dumb"

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "bad_michelson_insertion_1.ligo" ] ;
  [%expect{xxx|
    Raw Michelson must be seq (with curly braces {}), got: ADD. |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_contract "bad_michelson_insertion_2.ligo" ] ;
  [%expect{xxx|
Invalid type(s)
Cannot unify ( nat * nat ) with nat. |xxx}]