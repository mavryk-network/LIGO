open Cli_expect

let%expect_test _ =
  run_ligo_good ["compile"; "contract"; "--backend"; "wasm"; "../../test/contracts/deku/simple_invocation.mligo"];

  [%expect{|
    before:
    let main =
      fun gen#13 ->
      (let (gen#18, gen#19) = gen#13 in
       let _#14 = gen#18 in
       let _#15 = gen#19 in
       let a#16 = L(42) in
       let result#17 = ADD(a#16 , L(1)) in PAIR(LIST_EMPTY() , result#17)) in
    main : ((int * int)) -> ((list(operation) * int))

    after:
    wasm-ld: error: unable to find library -lc
    wasm-ld: error: cannot open /Users/Sander/Projects/ligo/_build/default/src/bin/expect_tests/vendors/wasi/wasi-sdk-14.0/lib/clang/13.0.0/lib/wasi/libclang_rt.builtins-wasm32.a: No such file or directory
    wasm-ld: error: unable to find library -lc
    wasm-ld: error: cannot open /Users/Sander/Projects/ligo/_build/default/src/bin/expect_tests/vendors/ligo-runtime/libligo_runtime.a: No such file or directory
    let main =
      fun gen#13 ->
      (let (gen#18, gen#19) = gen#13 in
       let _#14 = gen#18 in
       let _#15 = gen#19 in
       let a#16 = L(42) in
       let result#17 = ADD(a#16 , L(1)) in PAIR(LIST_EMPTY() , result#17)) in
    main : ((int * int)) -> ((list(operation) * int)){} |}]