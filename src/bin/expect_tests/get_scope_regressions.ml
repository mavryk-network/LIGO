open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/regressions/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "missing_stdlib_and_let_mut_in.jsligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [ _useless#2:20-28 s#1:17-18  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 44-48
    [ do_nothing#2:6-16 s#1:17-18  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 3, characters 2-27
    [ iter_op#1:6-13  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 13-20

    Variable definitions:
    (_useless#2:20-28 -> _useless)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 20-28
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 44-48
    Content: |core: int|
    references: []
    (do_nothing#2:6-16 -> do_nothing)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 6-16
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 19-48
    Content: |resolved: int -> unit|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 3, characters 13-23
    (iter_op#1:6-13 -> iter_op)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 1, characters 6-13
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 1, character 16 to line 4, character 1
    Content: |resolved: list (int) -> unit|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 13-20
    (s#1:17-18 -> s)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 1, characters 17-18
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, character 6 to line 3, character 27
    Content: |core: list (int)|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 3, characters 25-26
    (test#6:6-10 -> test)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 6-10
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 13-20
    Content: |resolved: list (int) -> unit|
    references: []
    Type definitions:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "missing_stdlib.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [ p#1:10-11  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 2, characters 10-23
    [ p#1:10-11  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 2, characters 26-52
    [ c#2:6-7 p#1:10-11  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 3, characters 2-17

    Variable definitions:
    (c#2:6-7 -> c)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 2, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 2, characters 26-52
    Content: |core: contract (unit)|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 3, characters 16-17
    (main#1:4-8 -> main)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 1, characters 4-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 1, character 0 to line 3, character 17
    Content: |core: key_hash -> address|
    references: []
    (p#1:10-11 -> p)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 1, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 2, character 2 to line 3, character 17
    Content: |core: key_hash|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 2, characters 50-51
    Type definitions:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "buggy_file_with_core_types.jsligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 1, character 12 to line 5, character 1
    [ user#1:5-9  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 7, characters 14-18
    [ user#1:5-9  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 8, characters 13-21
    [ user#1:5-9  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 9, characters 13-17
    [ user#1:5-9  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 10, characters 13-20
    [ alice#7:6-11 user#1:5-9  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 18-22
    [ alice#7:6-11 user#1:5-9  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 25-30

    Variable definitions:
    (alice#7:6-11 -> alice)
    Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 7, characters 6-11
    Body Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 7, character 21 to line 11, character 1
    Content: |core: user|
    references:
      File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 25-30
    (alice_admin#13:4-15 -> alice_admin)
    Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 4-15
    Body Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 25-32
    Content: |core: bool|
    references: []
    Type definitions:
    (user#1:5-9 -> user)
    Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 1, characters 5-9
    Body Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 1, character 12 to line 5, character 1
    Content: : |record[id -> nat , is_admin -> bool , name -> string]|
    references:
      File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 7, characters 14-18
    Module definitions:
    Errors:
    File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 25-32:
     12 |
     13 | let alice_admin : bool = alice.i

    Invalid record field "i" in record.
    Warnings:
    File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 0-32:
     12 |
     13 | let alice_admin : bool = alice.i

    Toplevel let declaration are silently change to const declaration. |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "wrong_reference1.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 1, characters 8-10
    [ x#2:6-7  ] File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 10-11
    [ f#2:4-5 x#1:4-5  ] File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 3, characters 8-9

    Variable definitions:
    (f#2:4-5 -> f)
    Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 0-11
    Content: |resolved: ∀ gen#7 : * . gen#7 -> int|
    references: []
    (g#3:4-5 -> g)
    Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 3, characters 8-9
    Content: |resolved: int|
    references: []
    (x#1:4-5 -> x)
    Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 1, characters 8-10
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 3, characters 8-9
    (x#2:6-7 -> x)
    Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 10-11
    Content: |resolved: gen#7|
    references: []
    Type definitions:
    Module definitions:
    Warnings:
    File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 6-7:
      1 | let x = 42
      2 | let f x = 0
      3 | let g = x
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning. |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module_alias_def_reference.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 4, characters 19-20
    [ A#2:7-8 B#3:11-12 D#9:11-12 toto#4:12-16  ] File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 15-21

    Variable definitions:
    Type definitions:
    Module definitions:
    (A#2:7-8 -> A)
    Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 2, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 3, character 4 to line 5, character 7
    Content: Members: Variable definitions:
                      Type definitions:
                      Module definitions:
                      (B#3:11-12 -> B)
                      Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 3, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 4, characters 8-20
                      Content: Members: Variable definitions:
                                        (toto#4:12-16 -> toto)
                                        Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 4, characters 12-16
                                        Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 4, characters 19-20
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 17-21
                                        Type definitions:
                                        Module definitions:

                      references:
                        File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 9, characters 17-18


    references:
      File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 9, characters 15-16

    (C#8:7-8 -> C)
    Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 8, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 9, character 4 to line 10, character 21
    Content: Members: Variable definitions:
                      (tata#10:8-12 -> tata)
                      Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 8-12
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 15-21
                      Content: |resolved: int|
                      references: []
                      Type definitions:
                      Module definitions:
                      (D#9:11-12 -> D)
                      Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 9, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 9, characters 15-18
                      Content: Alias: A#2:7-8.B#3:11-12
                      references:
                        File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 15-16


    references: [] |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "local_module_alias_def_reference.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 2, characters 16-19
    [ titi#2:9-13  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 18-22
    [ titi#2:9-13  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 25-27
    [ A#1:7-8 C#3:11-12 D#8:7-8 titi#2:9-13 toto#4:12-16  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 10, characters 11-17
    [ A#1:7-8 C#3:11-12 D#8:7-8 E#11:11-12 titi#2:9-13 toto#4:12-16  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 12, characters 4-10

    Variable definitions:
    (toto#10:4-8 -> toto)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 10, characters 4-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, character 4 to line 12, character 10
    Content: |core: D.titi|
    references: []
    Type definitions:
    Module definitions:
    (A#1:7-8 -> A)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 1, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 2, character 4 to line 5, character 7
    Content: Members: Variable definitions:
                      Type definitions:
                      (titi#2:9-13 -> titi)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 2, characters 9-13
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 2, characters 16-19
                      Content: : |int|
                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 18-22 ,
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 10, characters 13-17
                      Module definitions:
                      (C#3:11-12 -> C)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 3, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 8-27
                      Content: Members: Variable definitions:
                                        (toto#4:12-16 -> toto)
                                        Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 12-16
                                        Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 25-27
                                        Content: |core: titi|
                                        references:
                                          File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 12, characters 6-10
                                        Type definitions:
                                        Module definitions:

                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, characters 17-18


    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 8, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, characters 15-16

    (D#8:7-8 -> D)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 8, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 8, characters 11-12
    Content: Alias: A#1:7-8
    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 10, characters 11-12

    (E#11:11-12 -> E)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, characters 15-18
    Content: Alias: A#1:7-8.C#3:11-12
    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 12, characters 4-5 |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "local_module_alias_def_reference2.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 1, characters 11-14
    [  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 4, characters 23-24
    [ A#2:11-12 C#3:15-16 F#8:15-16 toto#4:16-20  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 19-25
    [ A#2:11-12 C#3:15-16 E#7:11-12 F#8:15-16 toto#4:16-20 toto#9:12-16  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 11, characters 4-10

    Variable definitions:
    (toto#1:4-8 -> toto)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 1, characters 4-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 2, character 4 to line 11, character 10
    Content: |core: int|
    references: []
    Type definitions:
    Module definitions:
    (A#2:11-12 -> A)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 2, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 3, character 8 to line 5, character 11
    Content: Members: Variable definitions:
                      Type definitions:
                      Module definitions:
                      (C#3:15-16 -> C)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 3, characters 15-16
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 4, characters 12-24
                      Content: Members: Variable definitions:
                                        (toto#4:16-20 -> toto)
                                        Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 4, characters 16-20
                                        Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 4, characters 23-24
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 21-25
                                        Type definitions:
                                        Module definitions:

                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 8, characters 21-22


    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 8, characters 19-20

    (E#7:11-12 -> E)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 7, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 8, character 8 to line 9, character 25
    Content: Members: Variable definitions:
                      (toto#9:12-16 -> toto)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 12-16
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 19-25
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 11, characters 6-10
                      Type definitions:
                      Module definitions:
                      (F#8:15-16 -> F)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 8, characters 15-16
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 8, characters 19-22
                      Content: Alias: A#2:11-12.C#3:15-16
                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 19-20


    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 11, characters 4-5 |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "duplicate_unused_warnings.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 1, characters 20-21
    [ s_x#2:9-12  ] File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 2, characters 16-17
    [  ] File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 3, characters 12-13
    [ m#1:4-5  ] File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 6, characters 21-22
    [ m#1:4-5  ] File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 7, characters 14-15
    [ m#1:4-5  ] File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 8, characters 12-13

    Variable definitions:
    (m#1:4-5 -> m)
    Range: File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 1, character 8 to line 3, character 13
    Content: |resolved: int|
    references: []
    (m2#6:4-6 -> m2)
    Range: File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 6, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 6, character 9 to line 8, character 13
    Content: |resolved: int|
    references: []
    (s_x#2:9-12 -> s_x)
    Range: File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 2, characters 9-12
    Body Range: File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 2, characters 16-17
    Content: |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    Warnings:
    File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 2, characters 9-12:
      1 | let m = match (Some 4) with
      2 |   | Some s_x -> 1
      3 |   | None -> 0
    :
    Warning: unused variable "s_x".
    Hint: replace it by "_s_x" to prevent this warning. |}]
