open Test_helpers

let mfile_FA1  = "./contracts/FA1.mligo"

let compile_main ~raise f _s () =
  Test_helpers.compile_main ~raise f ()

let main = test_suite "tzip-5" [
  test_w "compile"                           (compile_main             mfile_FA1 "cameligo");
  ]
