open Cli_expect

let gs = fun s -> ("../../test/contracts/get_scope_tests/"^s)

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda_letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes List a#19 f#24 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-7
[ Bytes List a#19 g#22 i#20 j#21 k#23 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-21
[ Bytes List a#19 g#22 i#20 j#21 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-25
[ Bytes List a#19 i#20 j#21 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-21
[ Bytes List ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 0-9
[ a#14 generated#17 head_opt#13 length#5 size#8 xs#16 ] File "", line 15, characters 23-25
[ a#14 head_opt#13 length#5 size#8 xs#15 ] File "", line 13, characters 11-13
[ a#9 generated#11 length#5 size#8 x#12 xs#10 ] File "", line 11, characters 22-23
[ a#9 length#5 size#8 xs#10 ] File "", line 9, characters 11-13
[ a#6 length#5 xs#7 ] File "", line 7, characters 43-52
[ a#3 xs#4 ] File "", line 6, characters 45-93

Variable definitions:
(a#19 -> a) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 6-7
(b#25 -> b) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#24 -> f) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-3
(g#22 -> g) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 24-25 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 16-17
(i#20 -> i) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 37-38 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 8-9
(j#21 -> j) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 47-48 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-5
(k#23 -> k) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 20-21
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 2, character 0 to line 4, character 3
(List -> List) File "", line 5, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes List a#19 c#20 d#23 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11
[ Bytes List a#19 c#20 e#21 f#22 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
[ Bytes List a#19 c#20 e#21 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
[ Bytes List a#19 c#20 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
[ Bytes List a#19 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
[ Bytes List ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
[ a#14 generated#17 head_opt#13 length#5 size#8 xs#16 ] File "", line 15, characters 23-25
[ a#14 head_opt#13 length#5 size#8 xs#15 ] File "", line 13, characters 11-13
[ a#9 generated#11 length#5 size#8 x#12 xs#10 ] File "", line 11, characters 22-23
[ a#9 length#5 size#8 xs#10 ] File "", line 9, characters 11-13
[ a#6 length#5 xs#7 ] File "", line 7, characters 43-52
[ a#3 xs#4 ] File "", line 6, characters 45-93

Variable definitions:
(a#19 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
(b#24 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(c#20 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
(d#23 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
(e#21 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
(f#22 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 2, character 0 to line 4, character 3
(List -> List) File "", line 5, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes List a#19 f#22 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-7
[ Bytes List a#19 i#20 j#21 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-63
[ Bytes List ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 0-9
[ a#14 generated#17 head_opt#13 length#5 size#8 xs#16 ] File "", line 15, characters 23-25
[ a#14 head_opt#13 length#5 size#8 xs#15 ] File "", line 13, characters 11-13
[ a#9 generated#11 length#5 size#8 x#12 xs#10 ] File "", line 11, characters 22-23
[ a#9 length#5 size#8 xs#10 ] File "", line 9, characters 11-13
[ a#6 length#5 xs#7 ] File "", line 7, characters 43-52
[ a#3 xs#4 ] File "", line 6, characters 45-93

Variable definitions:
(a#19 -> a) File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 4-5 |resolved: int|
references: []
(b#23 -> b) File "../../test/contracts/get_scope_tests/lambda.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#22 -> f) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-3
(i#20 -> i) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 36-37 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 62-63
(j#21 -> j) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 46-47 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-59
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 2, character 0 to line 4, character 3
(List -> List) File "", line 5, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "match.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#20 b#24 c#28 mytype#19 s#30 ] File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-21
    [ Bytes List a#20 b#24 c#28 d#29 mytype#19 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-31
    [ Bytes List a#20 b#24 c#28 mytype#19 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, character 9 to line 20, character 13
    [ Bytes List a#20 b#24 hd#27 mytype#19 tl#26 ] File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    [ Bytes List a#20 b#24 c#25 mytype#19 ] File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5
    [ Bytes List a#20 b#24 mytype#19 ] File "../../test/contracts/get_scope_tests/match.mligo", line 11, character 11 to line 14, character 5
    [ Bytes List a#20 mytype#19 y#23 ] File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-18
    [ Bytes List a#20 mytype#19 x#22 ] File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-18
    [ Bytes List a#20 c#21 mytype#19 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    [ Bytes List a#20 mytype#19 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 9-27
    [ Bytes List mytype#19 ] File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 0-9
    [ a#14 generated#17 head_opt#13 length#5 size#8 xs#16 ] File "", line 15, characters 23-25
    [ a#14 head_opt#13 length#5 size#8 xs#15 ] File "", line 13, characters 11-13
    [ a#9 generated#11 length#5 size#8 x#12 xs#10 ] File "", line 11, characters 22-23
    [ a#9 length#5 size#8 xs#10 ] File "", line 9, characters 11-13
    [ a#6 length#5 xs#7 ] File "", line 7, characters 43-52
    [ a#3 xs#4 ] File "", line 6, characters 45-93

    Variable definitions:
    (a#20 -> a) File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-29 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13
    (b#24 -> b) File "../../test/contracts/get_scope_tests/match.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (c#21 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    (c#25 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 8-9 |resolved: int|
    references: []
    (c#28 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 10, characters 4-5 |resolved: int|
    references: []
    (d#29 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 30-31
    (d#31 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 17, characters 4-5 |resolved: int|
    references: []
    (hd#27 -> hd) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6 |resolved: int|
    references: []
    (s#30 -> s) File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 10-11 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-17
    (tl#26 -> tl) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 8-10 |resolved: list (int)|
    references: []
    (x#22 -> x) File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-14
    (y#23 -> y) File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 8-9 |resolved: string|
    references: []
    Type definitions:
    (mytype#19 -> mytype) File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 0-40 :
    sum[Bar -> string , Foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 4, character 3
    (List -> List) File "", line 5, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "rec.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#19 b#25 c#24 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-9
    [ Bytes List a#19 c#24 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 8, character 2 to line 9, character 10
    [ Bytes List a#19 c#20 i#21 j#22 k#23 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-10
    [ Bytes List a#19 c#20 i#21 j#22 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-21
    [ Bytes List a#19 c#20 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-40
    [ Bytes List ] File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 0-9
    [ a#14 generated#17 head_opt#13 length#5 size#8 xs#16 ] File "", line 15, characters 23-25
    [ a#14 head_opt#13 length#5 size#8 xs#15 ] File "", line 13, characters 11-13
    [ a#9 generated#11 length#5 size#8 x#12 xs#10 ] File "", line 11, characters 22-23
    [ a#9 length#5 size#8 xs#10 ] File "", line 9, characters 11-13
    [ a#6 length#5 xs#7 ] File "", line 7, characters 43-52
    [ a#3 xs#4 ] File "", line 6, characters 45-93

    Variable definitions:
    (a#19 -> a) File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 5-6
    (b#25 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 8-9
    (b#26 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#20 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-5
    (c#24 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-3
    (i#21 -> i) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-38 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-13
    (j#22 -> j) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 39-40 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 16-17
    (k#23 -> k) File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 7-8
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 4, character 3
    (List -> List) File "", line 5, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "shadowing.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#19 c#20 d#23 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-11
    [ Bytes List a#22 c#20 e#21 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-13
    [ Bytes List a#19 c#20 e#21 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-21
    [ Bytes List a#19 c#20 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-17
    [ Bytes List a#19 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-15
    [ Bytes List ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 0-9
    [ a#14 generated#17 head_opt#13 length#5 size#8 xs#16 ] File "", line 15, characters 23-25
    [ a#14 head_opt#13 length#5 size#8 xs#15 ] File "", line 13, characters 11-13
    [ a#9 generated#11 length#5 size#8 x#12 xs#10 ] File "", line 11, characters 22-23
    [ a#9 length#5 size#8 xs#10 ] File "", line 9, characters 11-13
    [ a#6 length#5 xs#7 ] File "", line 7, characters 43-52
    [ a#3 xs#4 ] File "", line 6, characters 45-93

    Variable definitions:
    (a#19 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-3
    (a#22 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-5
    (b#24 -> b) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#20 -> c) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 6-7
    (d#23 -> d) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 10-11
    (e#21 -> e) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 12-13
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 4, character 3
    (List -> List) File "", line 5, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "records.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#20 b#23 g#24 myrec#19 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-41
    [ Bytes List a#20 b#23 myrec#19 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-41
    [ Bytes List a#20 i#21 j#22 myrec#19 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-45
    [ Bytes List a#20 i#21 myrec#19 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 27-45
    [ Bytes List a#20 myrec#19 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 14-55
    [ Bytes List myrec#19 ] File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 0-9
    [ a#14 generated#17 head_opt#13 length#5 size#8 xs#16 ] File "", line 15, characters 23-25
    [ a#14 head_opt#13 length#5 size#8 xs#15 ] File "", line 13, characters 11-13
    [ a#9 generated#11 length#5 size#8 x#12 xs#10 ] File "", line 11, characters 22-23
    [ a#9 length#5 size#8 xs#10 ] File "", line 9, characters 11-13
    [ a#6 length#5 xs#7 ] File "", line 7, characters 43-52
    [ a#3 xs#4 ] File "", line 6, characters 45-93

    Variable definitions:
    (a#20 -> a) File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-41 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 40-41
    (b#23 -> b) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 4-5 |resolved: myrec|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 32-33
    (e#25 -> e) File "../../test/contracts/get_scope_tests/records.mligo", line 15, characters 4-5 |resolved: myrec|
    references: []
    (g#24 -> g) File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 19-20 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-29
    (i#21 -> i) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 18-19 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 42-43
    (j#22 -> j) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 31-32 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 44-45
    Type definitions:
    (myrec#19 -> myrec) File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 0-36 :
    record[bar -> int , foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 4, character 3
    (List -> List) File "", line 5, character 0 to line 16, character 3 |} ] ;

  run_ligo_good [ "info" ; "get-scope" ; gs "constant.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#19 e#22 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-30
    [ Bytes List a#19 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 5-32
    [ Bytes List a#19 c#20 d#21 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ Bytes List a#19 c#20 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    [ Bytes List ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 0-9
    [ a#14 generated#17 head_opt#13 length#5 size#8 xs#16 ] File "", line 15, characters 23-25
    [ a#14 head_opt#13 length#5 size#8 xs#15 ] File "", line 13, characters 11-13
    [ a#9 generated#11 length#5 size#8 x#12 xs#10 ] File "", line 11, characters 22-23
    [ a#9 length#5 size#8 xs#10 ] File "", line 9, characters 11-13
    [ a#6 length#5 xs#7 ] File "", line 7, characters 43-52
    [ a#3 xs#4 ] File "", line 6, characters 45-93

    Variable definitions:
    (a#19 -> a) File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (b#23 -> b) File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5 |resolved: list (int)|
    references: []
    (c#20 -> c) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (d#21 -> d) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#22 -> e) File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 4, character 3
    (List -> List) File "", line 5, character 0 to line 16, character 3 |} ] 

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "application.mligo" ; "--format";"dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List c#23 f#21 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    [ Bytes List b#22 f#21 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-19
    [ Bytes List f#21 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 3-36
    [ Bytes List i#19 j#20 ] File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-63
    [ a#14 generated#17 head_opt#13 length#5 size#8 xs#16 ] File "", line 15, characters 23-25
    [ a#14 head_opt#13 length#5 size#8 xs#15 ] File "", line 13, characters 11-13
    [ a#9 generated#11 length#5 size#8 x#12 xs#10 ] File "", line 11, characters 22-23
    [ a#9 length#5 size#8 xs#10 ] File "", line 9, characters 11-13
    [ a#6 length#5 xs#7 ] File "", line 7, characters 43-52
    [ a#3 xs#4 ] File "", line 6, characters 45-93

    Variable definitions:
    (a#24 -> a) File "../../test/contracts/get_scope_tests/application.mligo", line 1, characters 4-5 |resolved: int|
    references: []
    (b#22 -> b) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 7-8 |resolved: int|
    references: []
    (c#23 -> c) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    (f#21 -> f) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 6-7 |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-17
    (i#19 -> i) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 36-37 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 62-63
    (j#20 -> j) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 46-47 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-59
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 4, character 3
    (List -> List) File "", line 5, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "include.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#19 b#24 x#25 ] File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-13
    [ Bytes List a#19 b#24 ] File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 0-9
    [ Bytes List a#19 c#20 d#23 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11
    [ Bytes List a#19 c#20 e#21 f#22 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
    [ Bytes List a#19 c#20 e#21 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    [ Bytes List a#19 c#20 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    [ Bytes List a#19 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    [ Bytes List ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
    [ a#14 generated#17 head_opt#13 length#5 size#8 xs#16 ] File "", line 15, characters 23-25
    [ a#14 head_opt#13 length#5 size#8 xs#15 ] File "", line 13, characters 11-13
    [ a#9 generated#11 length#5 size#8 x#12 xs#10 ] File "", line 11, characters 22-23
    [ a#9 length#5 size#8 xs#10 ] File "", line 9, characters 11-13
    [ a#6 length#5 xs#7 ] File "", line 7, characters 43-52
    [ a#3 xs#4 ] File "", line 6, characters 45-93

    Variable definitions:
    (a#19 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    (b#24 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#20 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    (d#23 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    (e#21 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    (f#22 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    (x#25 -> x) File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-9
    (y#26 -> y) File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 4, character 3
    (List -> List) File "", line 5, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "bad_field_record.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#22 c#20 foo_record#19 j#23 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    [ Bytes List a#22 c#20 foo_record#19 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    [ Bytes List c#20 foo_record#19 i#21 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    [ Bytes List c#20 foo_record#19 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11
    [ Bytes List foo_record#19 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 4, character 8 to line 5, character 9
    [ a#14 generated#17 head_opt#13 length#5 size#8 xs#16 ] File "", line 15, characters 23-25
    [ a#14 head_opt#13 length#5 size#8 xs#15 ] File "", line 13, characters 11-13
    [ a#9 generated#11 length#5 size#8 x#12 xs#10 ] File "", line 11, characters 22-23
    [ a#9 length#5 size#8 xs#10 ] File "", line 9, characters 11-13
    [ a#6 length#5 xs#7 ] File "", line 7, characters 43-52
    [ a#3 xs#4 ] File "", line 6, characters 45-93

    Variable definitions:
    (a#22 -> a) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 8, characters 4-5 |resolved: int|
    references: []
    (b#24 -> b) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 12, characters 4-5 |unresolved|
    references: []
    (c#20 -> c) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 3, characters 4-5 |resolved: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11 ,
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    (i#21 -> i) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    (j#23 -> j) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 6-7 |unresolved|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    Type definitions:
    (foo_record#19 -> foo_record) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 1, characters 0-43 :
    record[bar -> int , foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 4, character 3
    (List -> List) File "", line 5, character 0 to line 16, character 3 |} ];

  run_ligo_good [ "info"; "get-scope" ; gs "nominal_types.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#21 b#22 c#23 foo_record#20 foo_variant#19 p#24 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    [ Bytes List a#21 b#22 foo_record#20 foo_variant#19 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, character 8 to line 10, character 9
    [ Bytes List a#21 foo_record#20 foo_variant#19 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 12-14
    [ Bytes List foo_record#20 foo_variant#19 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 12-13
    [ a#14 generated#17 head_opt#13 length#5 size#8 xs#16 ] File "", line 15, characters 23-25
    [ a#14 head_opt#13 length#5 size#8 xs#15 ] File "", line 13, characters 11-13
    [ a#9 generated#11 length#5 size#8 x#12 xs#10 ] File "", line 11, characters 22-23
    [ a#9 length#5 size#8 xs#10 ] File "", line 9, characters 11-13
    [ a#6 length#5 xs#7 ] File "", line 7, characters 43-52
    [ a#3 xs#4 ] File "", line 6, characters 45-93

    Variable definitions:
    (a#21 -> a) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    (b#22 -> b) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    (c#23 -> c) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 8, characters 4-5 |resolved: foo_record|
    references: []
    (main#25 -> main) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 4-8 |core: foo_record -> foo_variant|
    references: []
    (p#24 -> p) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 10-11 |core: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    Type definitions:
    (foo_record#20 -> foo_record) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 0-58 :
    record[bar -> foo_variant , foo -> foo_variant]
    (foo_variant#19 -> foo_variant) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 0-45 :
    sum[Bar -> string , Foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 4, character 3
    (List -> List) File "", line 5, character 0 to line 16, character 3 |} ] ;
    
  run_ligo_good [ "info"; "get-scope" ; gs "module.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ A B Bytes C D List a#20 b#21 ] File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 4-7
    [ ] File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 8-17

    Variable definitions:
    (a#20 -> a) File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (b#21 -> b) File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 4-5 |resolved: int|
    references: []
    (titi#23 -> titi) File "../../test/contracts/get_scope_tests/module.mligo", line 11, characters 4-8 |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A -> A) File "../../test/contracts/get_scope_tests/module.mligo", line 1, character 0 to line 3, character 3
    (B -> B) File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 0-12
    (Bytes -> Bytes) File "", line 2, character 0 to line 4, character 3
    (C -> C) File "../../test/contracts/get_scope_tests/module.mligo", line 12, character 4 to line 16, character 7
    (D -> D) File "../../test/contracts/get_scope_tests/module.mligo", line 15, character 4 to line 16, character 7
    (List -> List) File "", line 5, character 0 to line 16, character 3 |} ]