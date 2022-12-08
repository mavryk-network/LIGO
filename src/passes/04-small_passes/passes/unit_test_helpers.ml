
open Simple_utils.Trace

let expected_failure f =
  try_with
    (fun ~raise ~catch:_ ->
      let _v = f ~raise in
      print_endline "This test should have failed")
    (fun ~catch:_ e ->
      Format.fprintf
        Format.std_formatter
        "Err : %a"
        Errors.(error_ppformat ~display_format:Dev)
        e)

