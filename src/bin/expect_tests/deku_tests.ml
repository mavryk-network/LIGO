open Cli_expect

let%expect_test _ =
  run_ligo_good ["compile"; "contract"; "--backend"; "wasm"; "../../test/contracts/deku/simple_invocation.mligo"];

  [%expect{|
    sh: ./vendors/wasi/wasi-sdk-14.0/bin/wasm-ld: No such file or directory
    {} |}]