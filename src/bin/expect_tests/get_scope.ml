open Cli_expect

let gs = fun s -> ("../../test/contracts/get_scope_tests/"^s)

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda_letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes List a#26 f#31 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-7
[ Bytes List a#26 g#29 i#27 j#28 k#30 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-21
[ Bytes List a#26 g#29 i#27 j#28 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-25
[ Bytes List a#26 i#27 j#28 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-21
[ Bytes List ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 0-9
[ a#21 generated#24 head_opt#20 length#12 size#15 xs#23 ] File "", line 18, characters 23-25
[ a#21 head_opt#20 length#12 size#15 xs#22 ] File "", line 16, characters 11-13
[ a#16 generated#18 length#12 size#15 x#19 xs#17 ] File "", line 14, characters 22-23
[ a#16 length#12 size#15 xs#17 ] File "", line 12, characters 11-13
[ a#13 length#12 xs#14 ] File "", line 10, characters 43-52
[ a#10 xs#11 ] File "", line 9, characters 45-93

Variable definitions:
(a#26 -> a) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 6-7
(b#32 -> b) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#31 -> f) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-3
(g#29 -> g) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 24-25 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 16-17
(i#27 -> i) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 37-38 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 8-9
(j#28 -> j) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 47-48 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-5
(k#30 -> k) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 20-21
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 2, character 0 to line 7, character 3
(List -> List) File "", line 8, character 0 to line 19, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes List a#26 c#27 d#30 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11
[ Bytes List a#26 c#27 e#28 f#29 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
[ Bytes List a#26 c#27 e#28 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
[ Bytes List a#26 c#27 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
[ Bytes List a#26 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
[ Bytes List ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
[ a#21 generated#24 head_opt#20 length#12 size#15 xs#23 ] File "", line 18, characters 23-25
[ a#21 head_opt#20 length#12 size#15 xs#22 ] File "", line 16, characters 11-13
[ a#16 generated#18 length#12 size#15 x#19 xs#17 ] File "", line 14, characters 22-23
[ a#16 length#12 size#15 xs#17 ] File "", line 12, characters 11-13
[ a#13 length#12 xs#14 ] File "", line 10, characters 43-52
[ a#10 xs#11 ] File "", line 9, characters 45-93

Variable definitions:
(a#26 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
(b#31 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(c#27 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
(d#30 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
(e#28 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
(f#29 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 2, character 0 to line 7, character 3
(List -> List) File "", line 8, character 0 to line 19, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes List a#26 f#29 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-7
[ Bytes List a#26 i#27 j#28 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-63
[ Bytes List ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 0-9
[ a#21 generated#24 head_opt#20 length#12 size#15 xs#23 ] File "", line 18, characters 23-25
[ a#21 head_opt#20 length#12 size#15 xs#22 ] File "", line 16, characters 11-13
[ a#16 generated#18 length#12 size#15 x#19 xs#17 ] File "", line 14, characters 22-23
[ a#16 length#12 size#15 xs#17 ] File "", line 12, characters 11-13
[ a#13 length#12 xs#14 ] File "", line 10, characters 43-52
[ a#10 xs#11 ] File "", line 9, characters 45-93

Variable definitions:
(a#26 -> a) File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 4-5 |resolved: int|
references: []
(b#30 -> b) File "../../test/contracts/get_scope_tests/lambda.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#29 -> f) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-3
(i#27 -> i) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 36-37 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 62-63
(j#28 -> j) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 46-47 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-59
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 2, character 0 to line 7, character 3
(List -> List) File "", line 8, character 0 to line 19, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "match.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#27 b#31 c#35 mytype#26 s#37 ] File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-21
    [ Bytes List a#27 b#31 c#35 d#36 mytype#26 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-31
    [ Bytes List a#27 b#31 c#35 mytype#26 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, character 9 to line 20, character 13
    [ Bytes List a#27 b#31 hd#34 mytype#26 tl#33 ] File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    [ Bytes List a#27 b#31 c#32 mytype#26 ] File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5
    [ Bytes List a#27 b#31 mytype#26 ] File "../../test/contracts/get_scope_tests/match.mligo", line 11, character 11 to line 14, character 5
    [ Bytes List a#27 mytype#26 y#30 ] File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-18
    [ Bytes List a#27 mytype#26 x#29 ] File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-18
    [ Bytes List a#27 c#28 mytype#26 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    [ Bytes List a#27 mytype#26 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 9-27
    [ Bytes List mytype#26 ] File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 0-9
    [ a#21 generated#24 head_opt#20 length#12 size#15 xs#23 ] File "", line 18, characters 23-25
    [ a#21 head_opt#20 length#12 size#15 xs#22 ] File "", line 16, characters 11-13
    [ a#16 generated#18 length#12 size#15 x#19 xs#17 ] File "", line 14, characters 22-23
    [ a#16 length#12 size#15 xs#17 ] File "", line 12, characters 11-13
    [ a#13 length#12 xs#14 ] File "", line 10, characters 43-52
    [ a#10 xs#11 ] File "", line 9, characters 45-93

    Variable definitions:
    (a#27 -> a) File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-29 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13
    (b#31 -> b) File "../../test/contracts/get_scope_tests/match.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (c#28 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    (c#32 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 8-9 |resolved: int|
    references: []
    (c#35 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 10, characters 4-5 |resolved: int|
    references: []
    (d#36 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 30-31
    (d#38 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 17, characters 4-5 |resolved: int|
    references: []
    (hd#34 -> hd) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6 |resolved: int|
    references: []
    (s#37 -> s) File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 10-11 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-17
    (tl#33 -> tl) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 8-10 |resolved: list (int)|
    references: []
    (x#29 -> x) File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-14
    (y#30 -> y) File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 8-9 |resolved: string|
    references: []
    Type definitions:
    (mytype#26 -> mytype) File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 0-40 :
    sum[Bar -> string , Foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 7, character 3
    (List -> List) File "", line 8, character 0 to line 19, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "rec.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#26 b#32 c#31 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-9
    [ Bytes List a#26 c#31 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 8, character 2 to line 9, character 10
    [ Bytes List a#26 c#27 i#28 j#29 k#30 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-10
    [ Bytes List a#26 c#27 i#28 j#29 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-21
    [ Bytes List a#26 c#27 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-40
    [ Bytes List ] File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 0-9
    [ a#21 generated#24 head_opt#20 length#12 size#15 xs#23 ] File "", line 18, characters 23-25
    [ a#21 head_opt#20 length#12 size#15 xs#22 ] File "", line 16, characters 11-13
    [ a#16 generated#18 length#12 size#15 x#19 xs#17 ] File "", line 14, characters 22-23
    [ a#16 length#12 size#15 xs#17 ] File "", line 12, characters 11-13
    [ a#13 length#12 xs#14 ] File "", line 10, characters 43-52
    [ a#10 xs#11 ] File "", line 9, characters 45-93

    Variable definitions:
    (a#26 -> a) File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 5-6
    (b#32 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 8-9
    (b#33 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#27 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-5
    (c#31 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-3
    (i#28 -> i) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-38 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-13
    (j#29 -> j) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 39-40 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 16-17
    (k#30 -> k) File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 7-8
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 7, character 3
    (List -> List) File "", line 8, character 0 to line 19, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "shadowing.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#26 c#27 d#30 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-11
    [ Bytes List a#29 c#27 e#28 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-13
    [ Bytes List a#26 c#27 e#28 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-21
    [ Bytes List a#26 c#27 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-17
    [ Bytes List a#26 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-15
    [ Bytes List ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 0-9
    [ a#21 generated#24 head_opt#20 length#12 size#15 xs#23 ] File "", line 18, characters 23-25
    [ a#21 head_opt#20 length#12 size#15 xs#22 ] File "", line 16, characters 11-13
    [ a#16 generated#18 length#12 size#15 x#19 xs#17 ] File "", line 14, characters 22-23
    [ a#16 length#12 size#15 xs#17 ] File "", line 12, characters 11-13
    [ a#13 length#12 xs#14 ] File "", line 10, characters 43-52
    [ a#10 xs#11 ] File "", line 9, characters 45-93

    Variable definitions:
    (a#26 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-3
    (a#29 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-5
    (b#31 -> b) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#27 -> c) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 6-7
    (d#30 -> d) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 10-11
    (e#28 -> e) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 12-13
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 7, character 3
    (List -> List) File "", line 8, character 0 to line 19, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "records.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#27 b#30 g#31 myrec#26 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-41
    [ Bytes List a#27 b#30 myrec#26 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-41
    [ Bytes List a#27 i#28 j#29 myrec#26 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-45
    [ Bytes List a#27 i#28 myrec#26 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 27-45
    [ Bytes List a#27 myrec#26 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 14-55
    [ Bytes List myrec#26 ] File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 0-9
    [ a#21 generated#24 head_opt#20 length#12 size#15 xs#23 ] File "", line 18, characters 23-25
    [ a#21 head_opt#20 length#12 size#15 xs#22 ] File "", line 16, characters 11-13
    [ a#16 generated#18 length#12 size#15 x#19 xs#17 ] File "", line 14, characters 22-23
    [ a#16 length#12 size#15 xs#17 ] File "", line 12, characters 11-13
    [ a#13 length#12 xs#14 ] File "", line 10, characters 43-52
    [ a#10 xs#11 ] File "", line 9, characters 45-93

    Variable definitions:
    (a#27 -> a) File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-41 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 40-41
    (b#30 -> b) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 4-5 |resolved: myrec|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 32-33
    (e#32 -> e) File "../../test/contracts/get_scope_tests/records.mligo", line 15, characters 4-5 |resolved: myrec|
    references: []
    (g#31 -> g) File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 19-20 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-29
    (i#28 -> i) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 18-19 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 42-43
    (j#29 -> j) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 31-32 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 44-45
    Type definitions:
    (myrec#26 -> myrec) File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 0-36 :
    record[bar -> int , foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 7, character 3
    (List -> List) File "", line 8, character 0 to line 19, character 3 |} ] ;

  run_ligo_good [ "info" ; "get-scope" ; gs "constant.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#26 e#29 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-30
    [ Bytes List a#26 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 5-32
    [ Bytes List a#26 c#27 d#28 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ Bytes List a#26 c#27 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    [ Bytes List ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 0-9
    [ a#21 generated#24 head_opt#20 length#12 size#15 xs#23 ] File "", line 18, characters 23-25
    [ a#21 head_opt#20 length#12 size#15 xs#22 ] File "", line 16, characters 11-13
    [ a#16 generated#18 length#12 size#15 x#19 xs#17 ] File "", line 14, characters 22-23
    [ a#16 length#12 size#15 xs#17 ] File "", line 12, characters 11-13
    [ a#13 length#12 xs#14 ] File "", line 10, characters 43-52
    [ a#10 xs#11 ] File "", line 9, characters 45-93

    Variable definitions:
    (a#26 -> a) File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (b#30 -> b) File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5 |resolved: list (int)|
    references: []
    (c#27 -> c) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (d#28 -> d) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#29 -> e) File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 7, character 3
    (List -> List) File "", line 8, character 0 to line 19, character 3 |} ] 

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "application.mligo" ; "--format";"dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List c#30 f#28 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    [ Bytes List b#29 f#28 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-19
    [ Bytes List f#28 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 3-36
    [ Bytes List i#26 j#27 ] File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-63
    [ a#21 generated#24 head_opt#20 length#12 size#15 xs#23 ] File "", line 18, characters 23-25
    [ a#21 head_opt#20 length#12 size#15 xs#22 ] File "", line 16, characters 11-13
    [ a#16 generated#18 length#12 size#15 x#19 xs#17 ] File "", line 14, characters 22-23
    [ a#16 length#12 size#15 xs#17 ] File "", line 12, characters 11-13
    [ a#13 length#12 xs#14 ] File "", line 10, characters 43-52
    [ a#10 xs#11 ] File "", line 9, characters 45-93

    Variable definitions:
    (a#31 -> a) File "../../test/contracts/get_scope_tests/application.mligo", line 1, characters 4-5 |resolved: int|
    references: []
    (b#29 -> b) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 7-8 |resolved: int|
    references: []
    (c#30 -> c) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    (f#28 -> f) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 6-7 |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-17
    (i#26 -> i) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 36-37 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 62-63
    (j#27 -> j) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 46-47 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-59
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 7, character 3
    (List -> List) File "", line 8, character 0 to line 19, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "include.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#26 b#31 x#32 ] File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-13
    [ Bytes List a#26 b#31 ] File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 0-9
    [ Bytes List a#26 c#27 d#30 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11
    [ Bytes List a#26 c#27 e#28 f#29 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
    [ Bytes List a#26 c#27 e#28 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    [ Bytes List a#26 c#27 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    [ Bytes List a#26 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    [ Bytes List ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
    [ a#21 generated#24 head_opt#20 length#12 size#15 xs#23 ] File "", line 18, characters 23-25
    [ a#21 head_opt#20 length#12 size#15 xs#22 ] File "", line 16, characters 11-13
    [ a#16 generated#18 length#12 size#15 x#19 xs#17 ] File "", line 14, characters 22-23
    [ a#16 length#12 size#15 xs#17 ] File "", line 12, characters 11-13
    [ a#13 length#12 xs#14 ] File "", line 10, characters 43-52
    [ a#10 xs#11 ] File "", line 9, characters 45-93

    Variable definitions:
    (a#26 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    (b#31 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#27 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    (d#30 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    (e#28 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    (f#29 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    (x#32 -> x) File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-9
    (y#33 -> y) File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 7, character 3
    (List -> List) File "", line 8, character 0 to line 19, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "bad_field_record.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#29 c#27 foo_record#26 j#30 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    [ Bytes List a#29 c#27 foo_record#26 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    [ Bytes List c#27 foo_record#26 i#28 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    [ Bytes List c#27 foo_record#26 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11
    [ Bytes List foo_record#26 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 4, character 8 to line 5, character 9
    [ a#21 generated#24 head_opt#20 length#12 size#15 xs#23 ] File "", line 18, characters 23-25
    [ a#21 head_opt#20 length#12 size#15 xs#22 ] File "", line 16, characters 11-13
    [ a#16 generated#18 length#12 size#15 x#19 xs#17 ] File "", line 14, characters 22-23
    [ a#16 length#12 size#15 xs#17 ] File "", line 12, characters 11-13
    [ a#13 length#12 xs#14 ] File "", line 10, characters 43-52
    [ a#10 xs#11 ] File "", line 9, characters 45-93

    Variable definitions:
    (a#29 -> a) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 8, characters 4-5 |resolved: int|
    references: []
    (b#31 -> b) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 12, characters 4-5 |unresolved|
    references: []
    (c#27 -> c) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 3, characters 4-5 |resolved: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11 ,
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    (i#28 -> i) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    (j#30 -> j) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 6-7 |unresolved|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    Type definitions:
    (foo_record#26 -> foo_record) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 1, characters 0-43 :
    record[bar -> int , foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 7, character 3
    (List -> List) File "", line 8, character 0 to line 19, character 3 |} ];

  run_ligo_good [ "info"; "get-scope" ; gs "nominal_types.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#28 b#29 c#30 foo_record#27 foo_variant#26 p#31 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    [ Bytes List a#28 b#29 foo_record#27 foo_variant#26 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, character 8 to line 10, character 9
    [ Bytes List a#28 foo_record#27 foo_variant#26 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 12-14
    [ Bytes List foo_record#27 foo_variant#26 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 12-13
    [ a#21 generated#24 head_opt#20 length#12 size#15 xs#23 ] File "", line 18, characters 23-25
    [ a#21 head_opt#20 length#12 size#15 xs#22 ] File "", line 16, characters 11-13
    [ a#16 generated#18 length#12 size#15 x#19 xs#17 ] File "", line 14, characters 22-23
    [ a#16 length#12 size#15 xs#17 ] File "", line 12, characters 11-13
    [ a#13 length#12 xs#14 ] File "", line 10, characters 43-52
    [ a#10 xs#11 ] File "", line 9, characters 45-93

    Variable definitions:
    (a#28 -> a) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    (b#29 -> b) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    (c#30 -> c) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 8, characters 4-5 |resolved: foo_record|
    references: []
    (main#32 -> main) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 4-8 |core: foo_record -> foo_variant|
    references: []
    (p#31 -> p) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 10-11 |core: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    Type definitions:
    (foo_record#27 -> foo_record) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 0-58 :
    record[bar -> foo_variant , foo -> foo_variant]
    (foo_variant#26 -> foo_variant) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 0-45 :
    sum[Bar -> string , Foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 7, character 3
    (List -> List) File "", line 8, character 0 to line 19, character 3 |} ] ;
    
  run_ligo_good [ "info"; "get-scope" ; gs "module.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ A B Bytes C D List a#27 b#28 ] File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 4-7
    [ ] File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 8-17

    Variable definitions:
    (a#27 -> a) File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (b#28 -> b) File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 4-5 |resolved: int|
    references: []
    (titi#30 -> titi) File "../../test/contracts/get_scope_tests/module.mligo", line 11, characters 4-8 |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A -> A) File "../../test/contracts/get_scope_tests/module.mligo", line 1, character 0 to line 3, character 3
    (B -> B) File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 0-12
    (Bytes -> Bytes) File "", line 2, character 0 to line 7, character 3
    (C -> C) File "../../test/contracts/get_scope_tests/module.mligo", line 12, character 4 to line 16, character 7
    (D -> D) File "../../test/contracts/get_scope_tests/module.mligo", line 15, character 4 to line 16, character 7
    (List -> List) File "", line 8, character 0 to line 19, character 3 |} ]