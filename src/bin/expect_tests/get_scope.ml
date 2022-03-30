open Cli_expect

let gs = fun s -> ("../../test/contracts/get_scope_tests/"^s)

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda_letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes List a#28 f#33 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-7
[ Bytes List a#28 g#31 i#29 j#30 k#32 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-21
[ Bytes List a#28 g#31 i#29 j#30 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-25
[ Bytes List a#28 i#29 j#30 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-21
[ Bytes List ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 0-9
[ a#23 generated#26 head_opt#22 length#14 size#17 xs#25 ] File "", line 19, characters 23-25
[ a#23 head_opt#22 length#14 size#17 xs#24 ] File "", line 17, characters 11-13
[ a#18 generated#20 length#14 size#17 x#21 xs#19 ] File "", line 15, characters 22-23
[ a#18 length#14 size#17 xs#19 ] File "", line 13, characters 11-13
[ a#15 length#14 xs#16 ] File "", line 11, characters 53-62
[ a#12 xs#13 ] File "", line 10, characters 55-103

Variable definitions:
(a#28 -> a) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 6-7
(b#34 -> b) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#33 -> f) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-3
(g#31 -> g) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 24-25 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 16-17
(i#29 -> i) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 37-38 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 8-9
(j#30 -> j) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 47-48 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-5
(k#32 -> k) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 20-21
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 2, character 0 to line 8, character 3
(List -> List) File "", line 9, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes List a#28 c#29 d#32 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11
[ Bytes List a#28 c#29 e#30 f#31 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
[ Bytes List a#28 c#29 e#30 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
[ Bytes List a#28 c#29 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
[ Bytes List a#28 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
[ Bytes List ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
[ a#23 generated#26 head_opt#22 length#14 size#17 xs#25 ] File "", line 19, characters 23-25
[ a#23 head_opt#22 length#14 size#17 xs#24 ] File "", line 17, characters 11-13
[ a#18 generated#20 length#14 size#17 x#21 xs#19 ] File "", line 15, characters 22-23
[ a#18 length#14 size#17 xs#19 ] File "", line 13, characters 11-13
[ a#15 length#14 xs#16 ] File "", line 11, characters 53-62
[ a#12 xs#13 ] File "", line 10, characters 55-103

Variable definitions:
(a#28 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
(b#33 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(c#29 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
(d#32 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
(e#30 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
(f#31 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 2, character 0 to line 8, character 3
(List -> List) File "", line 9, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes List a#28 f#31 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-7
[ Bytes List a#28 i#29 j#30 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-63
[ Bytes List ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 0-9
[ a#23 generated#26 head_opt#22 length#14 size#17 xs#25 ] File "", line 19, characters 23-25
[ a#23 head_opt#22 length#14 size#17 xs#24 ] File "", line 17, characters 11-13
[ a#18 generated#20 length#14 size#17 x#21 xs#19 ] File "", line 15, characters 22-23
[ a#18 length#14 size#17 xs#19 ] File "", line 13, characters 11-13
[ a#15 length#14 xs#16 ] File "", line 11, characters 53-62
[ a#12 xs#13 ] File "", line 10, characters 55-103

Variable definitions:
(a#28 -> a) File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 4-5 |resolved: int|
references: []
(b#32 -> b) File "../../test/contracts/get_scope_tests/lambda.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#31 -> f) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-3
(i#29 -> i) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 36-37 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 62-63
(j#30 -> j) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 46-47 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-59
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 2, character 0 to line 8, character 3
(List -> List) File "", line 9, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "match.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#29 b#33 c#37 mytype#28 s#39 ] File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-21
    [ Bytes List a#29 b#33 c#37 d#38 mytype#28 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-31
    [ Bytes List a#29 b#33 c#37 mytype#28 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, character 9 to line 20, character 13
    [ Bytes List a#29 b#33 hd#36 mytype#28 tl#35 ] File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    [ Bytes List a#29 b#33 c#34 mytype#28 ] File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5
    [ Bytes List a#29 b#33 mytype#28 ] File "../../test/contracts/get_scope_tests/match.mligo", line 11, character 11 to line 14, character 5
    [ Bytes List a#29 mytype#28 y#32 ] File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-18
    [ Bytes List a#29 mytype#28 x#31 ] File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-18
    [ Bytes List a#29 c#30 mytype#28 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    [ Bytes List a#29 mytype#28 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 9-27
    [ Bytes List mytype#28 ] File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 0-9
    [ a#23 generated#26 head_opt#22 length#14 size#17 xs#25 ] File "", line 19, characters 23-25
    [ a#23 head_opt#22 length#14 size#17 xs#24 ] File "", line 17, characters 11-13
    [ a#18 generated#20 length#14 size#17 x#21 xs#19 ] File "", line 15, characters 22-23
    [ a#18 length#14 size#17 xs#19 ] File "", line 13, characters 11-13
    [ a#15 length#14 xs#16 ] File "", line 11, characters 53-62
    [ a#12 xs#13 ] File "", line 10, characters 55-103

    Variable definitions:
    (a#29 -> a) File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-29 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13
    (b#33 -> b) File "../../test/contracts/get_scope_tests/match.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (c#30 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    (c#34 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 8-9 |resolved: int|
    references: []
    (c#37 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 10, characters 4-5 |resolved: int|
    references: []
    (d#38 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 30-31
    (d#40 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 17, characters 4-5 |resolved: int|
    references: []
    (hd#36 -> hd) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6 |resolved: int|
    references: []
    (s#39 -> s) File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 10-11 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-17
    (tl#35 -> tl) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 8-10 |resolved: list (int)|
    references: []
    (x#31 -> x) File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-14
    (y#32 -> y) File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 8-9 |resolved: string|
    references: []
    Type definitions:
    (mytype#28 -> mytype) File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 0-40 :
    sum[Bar -> string , Foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 8, character 3
    (List -> List) File "", line 9, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "rec.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#28 b#34 c#33 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-9
    [ Bytes List a#28 c#33 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 8, character 2 to line 9, character 10
    [ Bytes List a#28 c#29 i#30 j#31 k#32 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-10
    [ Bytes List a#28 c#29 i#30 j#31 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-21
    [ Bytes List a#28 c#29 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-40
    [ Bytes List ] File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 0-9
    [ a#23 generated#26 head_opt#22 length#14 size#17 xs#25 ] File "", line 19, characters 23-25
    [ a#23 head_opt#22 length#14 size#17 xs#24 ] File "", line 17, characters 11-13
    [ a#18 generated#20 length#14 size#17 x#21 xs#19 ] File "", line 15, characters 22-23
    [ a#18 length#14 size#17 xs#19 ] File "", line 13, characters 11-13
    [ a#15 length#14 xs#16 ] File "", line 11, characters 53-62
    [ a#12 xs#13 ] File "", line 10, characters 55-103

    Variable definitions:
    (a#28 -> a) File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 5-6
    (b#34 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 8-9
    (b#35 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#29 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-5
    (c#33 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-3
    (i#30 -> i) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-38 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-13
    (j#31 -> j) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 39-40 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 16-17
    (k#32 -> k) File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 7-8
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 8, character 3
    (List -> List) File "", line 9, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "shadowing.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#28 c#29 d#32 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-11
    [ Bytes List a#31 c#29 e#30 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-13
    [ Bytes List a#28 c#29 e#30 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-21
    [ Bytes List a#28 c#29 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-17
    [ Bytes List a#28 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-15
    [ Bytes List ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 0-9
    [ a#23 generated#26 head_opt#22 length#14 size#17 xs#25 ] File "", line 19, characters 23-25
    [ a#23 head_opt#22 length#14 size#17 xs#24 ] File "", line 17, characters 11-13
    [ a#18 generated#20 length#14 size#17 x#21 xs#19 ] File "", line 15, characters 22-23
    [ a#18 length#14 size#17 xs#19 ] File "", line 13, characters 11-13
    [ a#15 length#14 xs#16 ] File "", line 11, characters 53-62
    [ a#12 xs#13 ] File "", line 10, characters 55-103

    Variable definitions:
    (a#28 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-3
    (a#31 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-5
    (b#33 -> b) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#29 -> c) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 6-7
    (d#32 -> d) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 10-11
    (e#30 -> e) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 12-13
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 8, character 3
    (List -> List) File "", line 9, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "records.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#29 b#32 g#33 myrec#28 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-41
    [ Bytes List a#29 b#32 myrec#28 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-41
    [ Bytes List a#29 i#30 j#31 myrec#28 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-45
    [ Bytes List a#29 i#30 myrec#28 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 27-45
    [ Bytes List a#29 myrec#28 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 14-55
    [ Bytes List myrec#28 ] File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 0-9
    [ a#23 generated#26 head_opt#22 length#14 size#17 xs#25 ] File "", line 19, characters 23-25
    [ a#23 head_opt#22 length#14 size#17 xs#24 ] File "", line 17, characters 11-13
    [ a#18 generated#20 length#14 size#17 x#21 xs#19 ] File "", line 15, characters 22-23
    [ a#18 length#14 size#17 xs#19 ] File "", line 13, characters 11-13
    [ a#15 length#14 xs#16 ] File "", line 11, characters 53-62
    [ a#12 xs#13 ] File "", line 10, characters 55-103

    Variable definitions:
    (a#29 -> a) File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-41 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 40-41
    (b#32 -> b) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 4-5 |resolved: myrec|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 32-33
    (e#34 -> e) File "../../test/contracts/get_scope_tests/records.mligo", line 15, characters 4-5 |resolved: myrec|
    references: []
    (g#33 -> g) File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 19-20 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-29
    (i#30 -> i) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 18-19 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 42-43
    (j#31 -> j) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 31-32 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 44-45
    Type definitions:
    (myrec#28 -> myrec) File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 0-36 :
    record[bar -> int , foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 8, character 3
    (List -> List) File "", line 9, character 0 to line 20, character 3 |} ] ;

  run_ligo_good [ "info" ; "get-scope" ; gs "constant.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#28 e#31 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-30
    [ Bytes List a#28 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 5-32
    [ Bytes List a#28 c#29 d#30 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ Bytes List a#28 c#29 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    [ Bytes List ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 0-9
    [ a#23 generated#26 head_opt#22 length#14 size#17 xs#25 ] File "", line 19, characters 23-25
    [ a#23 head_opt#22 length#14 size#17 xs#24 ] File "", line 17, characters 11-13
    [ a#18 generated#20 length#14 size#17 x#21 xs#19 ] File "", line 15, characters 22-23
    [ a#18 length#14 size#17 xs#19 ] File "", line 13, characters 11-13
    [ a#15 length#14 xs#16 ] File "", line 11, characters 53-62
    [ a#12 xs#13 ] File "", line 10, characters 55-103

    Variable definitions:
    (a#28 -> a) File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (b#32 -> b) File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5 |resolved: list (int)|
    references: []
    (c#29 -> c) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (d#30 -> d) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#31 -> e) File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 8, character 3
    (List -> List) File "", line 9, character 0 to line 20, character 3 |} ] 

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "application.mligo" ; "--format";"dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List c#32 f#30 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    [ Bytes List b#31 f#30 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-19
    [ Bytes List f#30 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 3-36
    [ Bytes List i#28 j#29 ] File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-63
    [ a#23 generated#26 head_opt#22 length#14 size#17 xs#25 ] File "", line 19, characters 23-25
    [ a#23 head_opt#22 length#14 size#17 xs#24 ] File "", line 17, characters 11-13
    [ a#18 generated#20 length#14 size#17 x#21 xs#19 ] File "", line 15, characters 22-23
    [ a#18 length#14 size#17 xs#19 ] File "", line 13, characters 11-13
    [ a#15 length#14 xs#16 ] File "", line 11, characters 53-62
    [ a#12 xs#13 ] File "", line 10, characters 55-103

    Variable definitions:
    (a#33 -> a) File "../../test/contracts/get_scope_tests/application.mligo", line 1, characters 4-5 |resolved: int|
    references: []
    (b#31 -> b) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 7-8 |resolved: int|
    references: []
    (c#32 -> c) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    (f#30 -> f) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 6-7 |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-17
    (i#28 -> i) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 36-37 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 62-63
    (j#29 -> j) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 46-47 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-59
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 8, character 3
    (List -> List) File "", line 9, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "include.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#28 b#33 x#34 ] File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-13
    [ Bytes List a#28 b#33 ] File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 0-9
    [ Bytes List a#28 c#29 d#32 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11
    [ Bytes List a#28 c#29 e#30 f#31 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
    [ Bytes List a#28 c#29 e#30 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    [ Bytes List a#28 c#29 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    [ Bytes List a#28 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    [ Bytes List ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
    [ a#23 generated#26 head_opt#22 length#14 size#17 xs#25 ] File "", line 19, characters 23-25
    [ a#23 head_opt#22 length#14 size#17 xs#24 ] File "", line 17, characters 11-13
    [ a#18 generated#20 length#14 size#17 x#21 xs#19 ] File "", line 15, characters 22-23
    [ a#18 length#14 size#17 xs#19 ] File "", line 13, characters 11-13
    [ a#15 length#14 xs#16 ] File "", line 11, characters 53-62
    [ a#12 xs#13 ] File "", line 10, characters 55-103

    Variable definitions:
    (a#28 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    (b#33 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#29 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    (d#32 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    (e#30 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    (f#31 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    (x#34 -> x) File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-9
    (y#35 -> y) File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 8, character 3
    (List -> List) File "", line 9, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "bad_field_record.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#31 c#29 foo_record#28 j#32 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    [ Bytes List a#31 c#29 foo_record#28 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    [ Bytes List c#29 foo_record#28 i#30 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    [ Bytes List c#29 foo_record#28 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11
    [ Bytes List foo_record#28 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 4, character 8 to line 5, character 9
    [ a#23 generated#26 head_opt#22 length#14 size#17 xs#25 ] File "", line 19, characters 23-25
    [ a#23 head_opt#22 length#14 size#17 xs#24 ] File "", line 17, characters 11-13
    [ a#18 generated#20 length#14 size#17 x#21 xs#19 ] File "", line 15, characters 22-23
    [ a#18 length#14 size#17 xs#19 ] File "", line 13, characters 11-13
    [ a#15 length#14 xs#16 ] File "", line 11, characters 53-62
    [ a#12 xs#13 ] File "", line 10, characters 55-103

    Variable definitions:
    (a#31 -> a) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 8, characters 4-5 |resolved: int|
    references: []
    (b#33 -> b) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 12, characters 4-5 |unresolved|
    references: []
    (c#29 -> c) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 3, characters 4-5 |resolved: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11 ,
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    (i#30 -> i) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    (j#32 -> j) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 6-7 |unresolved|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    Type definitions:
    (foo_record#28 -> foo_record) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 1, characters 0-43 :
    record[bar -> int , foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 8, character 3
    (List -> List) File "", line 9, character 0 to line 20, character 3 |} ];

  run_ligo_good [ "info"; "get-scope" ; gs "nominal_types.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes List a#30 b#31 c#32 foo_record#29 foo_variant#28 p#33 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    [ Bytes List a#30 b#31 foo_record#29 foo_variant#28 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, character 8 to line 10, character 9
    [ Bytes List a#30 foo_record#29 foo_variant#28 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 12-14
    [ Bytes List foo_record#29 foo_variant#28 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 12-13
    [ a#23 generated#26 head_opt#22 length#14 size#17 xs#25 ] File "", line 19, characters 23-25
    [ a#23 head_opt#22 length#14 size#17 xs#24 ] File "", line 17, characters 11-13
    [ a#18 generated#20 length#14 size#17 x#21 xs#19 ] File "", line 15, characters 22-23
    [ a#18 length#14 size#17 xs#19 ] File "", line 13, characters 11-13
    [ a#15 length#14 xs#16 ] File "", line 11, characters 53-62
    [ a#12 xs#13 ] File "", line 10, characters 55-103

    Variable definitions:
    (a#30 -> a) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    (b#31 -> b) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    (c#32 -> c) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 8, characters 4-5 |resolved: foo_record|
    references: []
    (main#34 -> main) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 4-8 |core: foo_record -> foo_variant|
    references: []
    (p#33 -> p) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 10-11 |core: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    Type definitions:
    (foo_record#29 -> foo_record) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 0-58 :
    record[bar -> foo_variant , foo -> foo_variant]
    (foo_variant#28 -> foo_variant) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 0-45 :
    sum[Bar -> string , Foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 2, character 0 to line 8, character 3
    (List -> List) File "", line 9, character 0 to line 20, character 3 |} ] ;
    
  run_ligo_good [ "info"; "get-scope" ; gs "module.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ A B Bytes C D List a#29 b#30 ] File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 4-7
    [ ] File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 8-17

    Variable definitions:
    (a#29 -> a) File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (b#30 -> b) File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 4-5 |resolved: int|
    references: []
    (titi#32 -> titi) File "../../test/contracts/get_scope_tests/module.mligo", line 11, characters 4-8 |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A -> A) File "../../test/contracts/get_scope_tests/module.mligo", line 1, character 0 to line 3, character 3
    (B -> B) File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 0-12
    (Bytes -> Bytes) File "", line 2, character 0 to line 8, character 3
    (C -> C) File "../../test/contracts/get_scope_tests/module.mligo", line 12, character 4 to line 16, character 7
    (D -> D) File "../../test/contracts/get_scope_tests/module.mligo", line 15, character 4 to line 16, character 7
    (List -> List) File "", line 9, character 0 to line 20, character 3 |} ]