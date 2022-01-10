open Cli_expect

let pwd = Sys.getcwd ()
let () = Sys.chdir "../../test/contracts/include/test1"

let%expect_test _ =
  run_ligo_good [ "print" ; "preprocessed" ;  "root.ligo" ; "--lib" ; "includes" ] ;
  [%expect{|
    # 1 "root.ligo"

    # 1 "includes/b1.ligo" 1

    # 1 "includes/b2/b2.ligo" 1

    # 1 "includes/b2/../b3.ligo" 1
    const b3 = 3


    # 2 "includes/b2/b2.ligo" 2

    const b2 = b3 * 3


    # 2 "includes/b1.ligo" 2

    const b1 = b2 * 2 + b3


    # 2 "root.ligo" 2 |}]

let () = Sys.chdir pwd ;
         Sys.chdir "../../test/contracts/include/test2"

let%expect_test _ =
  run_ligo_good [ "print" ; "preprocessed" ;  "Root.mligo" ; "--lib" ; "bug" ] ;
  [%expect{|
    # 1 "Root.mligo"

    # 1 "bug/A.mligo" 1

    # 1 "bug/dir/B.mligo" 1
    let x : int = 42

    # 2 "bug/A.mligo" 2

    # 2 "Root.mligo" 2 |}]

let () = Sys.chdir pwd ;
         Sys.chdir "../../test/contracts/include/test3"        

let%expect_test _ =
  run_ligo_good [ "print" ; "preprocessed" ;  "B1.ligo" ; "--lib" ; "B2" ] ;
  [%expect{|
    # 1 "B1.ligo"

    # 1 "B2/B2.ligo" 1

    # 1 "B2/../B3.ligo" 1
    const b3 = 3

    # 2 "B2/B2.ligo" 2

    const b2 = b3 * 3

    # 2 "B1.ligo" 2

    const b1 = b2 * 2 + b3 |}]

let () = Sys.chdir pwd ;
         Sys.chdir "../../test/contracts/include/test4/current"

let%expect_test _ =
  run_ligo_good [ "print" ; "preprocessed" ;  "../Root.ligo" ; "--lib" ; "../bug" ] ;
  [%expect{|
    # 1 "../Root.ligo"

    # 1 "../bug/nested/A.ligo" 1

    # 1 "../bug/nested/../B.ligo" 1
    const x = 42

    # 2 "../bug/nested/A.ligo" 2

    # 2 "../Root.ligo" 2 |}]

let () = Sys.chdir pwd