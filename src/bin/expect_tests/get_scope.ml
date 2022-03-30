open Cli_expect

let gs = fun s -> ("../../test/contracts/get_scope_tests/"^s)

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda_letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes Crypto List String a#59 f#64 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-7
[ Bytes Crypto List String a#59 g#62 i#60 j#61 k#63 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-21
[ Bytes Crypto List String a#59 g#62 i#60 j#61 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-25
[ Bytes Crypto List String a#59 i#60 j#61 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-21
[ Bytes Crypto List String ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 0-9
[ a#54 generated#57 head_opt#53 length#45 size#48 xs#56 ] File "", line 36, characters 23-25
[ a#54 head_opt#53 length#45 size#48 xs#55 ] File "", line 34, characters 11-13
[ a#49 generated#51 length#45 size#48 x#52 xs#50 ] File "", line 32, characters 22-23
[ a#49 length#45 size#48 xs#50 ] File "", line 30, characters 11-13
[ a#46 length#45 xs#47 ] File "", line 28, characters 53-62
[ a#43 xs#44 ] File "", line 27, characters 55-103

Variable definitions:
(a#59 -> a) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 6-7
(b#65 -> b) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#64 -> f) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-3
(g#62 -> g) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 24-25 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 16-17
(i#60 -> i) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 37-38 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 8-9
(j#61 -> j) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 47-48 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-5
(k#63 -> k) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 20-21
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 19, character 0 to line 25, character 3
(Crypto -> Crypto) File "", line 10, character 0 to line 18, character 3
(List -> List) File "", line 26, character 0 to line 37, character 3
(String -> String) File "", line 2, character 0 to line 9, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes Crypto List String a#59 c#60 d#63 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11
[ Bytes Crypto List String a#59 c#60 e#61 f#62 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
[ Bytes Crypto List String a#59 c#60 e#61 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
[ Bytes Crypto List String a#59 c#60 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
[ Bytes Crypto List String a#59 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
[ Bytes Crypto List String ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
[ a#54 generated#57 head_opt#53 length#45 size#48 xs#56 ] File "", line 36, characters 23-25
[ a#54 head_opt#53 length#45 size#48 xs#55 ] File "", line 34, characters 11-13
[ a#49 generated#51 length#45 size#48 x#52 xs#50 ] File "", line 32, characters 22-23
[ a#49 length#45 size#48 xs#50 ] File "", line 30, characters 11-13
[ a#46 length#45 xs#47 ] File "", line 28, characters 53-62
[ a#43 xs#44 ] File "", line 27, characters 55-103

Variable definitions:
(a#59 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
(b#64 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(c#60 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
(d#63 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
(e#61 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
(f#62 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 19, character 0 to line 25, character 3
(Crypto -> Crypto) File "", line 10, character 0 to line 18, character 3
(List -> List) File "", line 26, character 0 to line 37, character 3
(String -> String) File "", line 2, character 0 to line 9, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes Crypto List String a#59 f#62 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-7
[ Bytes Crypto List String a#59 i#60 j#61 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-63
[ Bytes Crypto List String ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 0-9
[ a#54 generated#57 head_opt#53 length#45 size#48 xs#56 ] File "", line 36, characters 23-25
[ a#54 head_opt#53 length#45 size#48 xs#55 ] File "", line 34, characters 11-13
[ a#49 generated#51 length#45 size#48 x#52 xs#50 ] File "", line 32, characters 22-23
[ a#49 length#45 size#48 xs#50 ] File "", line 30, characters 11-13
[ a#46 length#45 xs#47 ] File "", line 28, characters 53-62
[ a#43 xs#44 ] File "", line 27, characters 55-103

Variable definitions:
(a#59 -> a) File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 4-5 |resolved: int|
references: []
(b#63 -> b) File "../../test/contracts/get_scope_tests/lambda.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#62 -> f) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-3
(i#60 -> i) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 36-37 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 62-63
(j#61 -> j) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 46-47 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-59
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 19, character 0 to line 25, character 3
(Crypto -> Crypto) File "", line 10, character 0 to line 18, character 3
(List -> List) File "", line 26, character 0 to line 37, character 3
(String -> String) File "", line 2, character 0 to line 9, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "match.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List String a#60 b#64 c#68 mytype#59 s#70 ] File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-21
    [ Bytes Crypto List String a#60 b#64 c#68 d#69 mytype#59 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-31
    [ Bytes Crypto List String a#60 b#64 c#68 mytype#59 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, character 9 to line 20, character 13
    [ Bytes Crypto List String a#60 b#64 hd#67 mytype#59 tl#66 ] File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    [ Bytes Crypto List String a#60 b#64 c#65 mytype#59 ] File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5
    [ Bytes Crypto List String a#60 b#64 mytype#59 ] File "../../test/contracts/get_scope_tests/match.mligo", line 11, character 11 to line 14, character 5
    [ Bytes Crypto List String a#60 mytype#59 y#63 ] File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-18
    [ Bytes Crypto List String a#60 mytype#59 x#62 ] File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-18
    [ Bytes Crypto List String a#60 c#61 mytype#59 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    [ Bytes Crypto List String a#60 mytype#59 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 9-27
    [ Bytes Crypto List String mytype#59 ] File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 0-9
    [ a#54 generated#57 head_opt#53 length#45 size#48 xs#56 ] File "", line 36, characters 23-25
    [ a#54 head_opt#53 length#45 size#48 xs#55 ] File "", line 34, characters 11-13
    [ a#49 generated#51 length#45 size#48 x#52 xs#50 ] File "", line 32, characters 22-23
    [ a#49 length#45 size#48 xs#50 ] File "", line 30, characters 11-13
    [ a#46 length#45 xs#47 ] File "", line 28, characters 53-62
    [ a#43 xs#44 ] File "", line 27, characters 55-103

    Variable definitions:
    (a#60 -> a) File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-29 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13
    (b#64 -> b) File "../../test/contracts/get_scope_tests/match.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (c#61 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    (c#65 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 8-9 |resolved: int|
    references: []
    (c#68 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 10, characters 4-5 |resolved: int|
    references: []
    (d#69 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 30-31
    (d#71 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 17, characters 4-5 |resolved: int|
    references: []
    (hd#67 -> hd) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6 |resolved: int|
    references: []
    (s#70 -> s) File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 10-11 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-17
    (tl#66 -> tl) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 8-10 |resolved: list (int)|
    references: []
    (x#62 -> x) File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-14
    (y#63 -> y) File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 8-9 |resolved: string|
    references: []
    Type definitions:
    (mytype#59 -> mytype) File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 0-40 :
    sum[Bar -> string , Foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 19, character 0 to line 25, character 3
    (Crypto -> Crypto) File "", line 10, character 0 to line 18, character 3
    (List -> List) File "", line 26, character 0 to line 37, character 3
    (String -> String) File "", line 2, character 0 to line 9, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "rec.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List String a#59 b#65 c#64 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-9
    [ Bytes Crypto List String a#59 c#64 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 8, character 2 to line 9, character 10
    [ Bytes Crypto List String a#59 c#60 i#61 j#62 k#63 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-10
    [ Bytes Crypto List String a#59 c#60 i#61 j#62 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-21
    [ Bytes Crypto List String a#59 c#60 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-40
    [ Bytes Crypto List String ] File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 0-9
    [ a#54 generated#57 head_opt#53 length#45 size#48 xs#56 ] File "", line 36, characters 23-25
    [ a#54 head_opt#53 length#45 size#48 xs#55 ] File "", line 34, characters 11-13
    [ a#49 generated#51 length#45 size#48 x#52 xs#50 ] File "", line 32, characters 22-23
    [ a#49 length#45 size#48 xs#50 ] File "", line 30, characters 11-13
    [ a#46 length#45 xs#47 ] File "", line 28, characters 53-62
    [ a#43 xs#44 ] File "", line 27, characters 55-103

    Variable definitions:
    (a#59 -> a) File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 5-6
    (b#65 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 8-9
    (b#66 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#60 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-5
    (c#64 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-3
    (i#61 -> i) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-38 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-13
    (j#62 -> j) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 39-40 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 16-17
    (k#63 -> k) File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 7-8
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 19, character 0 to line 25, character 3
    (Crypto -> Crypto) File "", line 10, character 0 to line 18, character 3
    (List -> List) File "", line 26, character 0 to line 37, character 3
    (String -> String) File "", line 2, character 0 to line 9, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "shadowing.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List String a#59 c#60 d#63 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-11
    [ Bytes Crypto List String a#62 c#60 e#61 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-13
    [ Bytes Crypto List String a#59 c#60 e#61 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-21
    [ Bytes Crypto List String a#59 c#60 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-17
    [ Bytes Crypto List String a#59 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-15
    [ Bytes Crypto List String ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 0-9
    [ a#54 generated#57 head_opt#53 length#45 size#48 xs#56 ] File "", line 36, characters 23-25
    [ a#54 head_opt#53 length#45 size#48 xs#55 ] File "", line 34, characters 11-13
    [ a#49 generated#51 length#45 size#48 x#52 xs#50 ] File "", line 32, characters 22-23
    [ a#49 length#45 size#48 xs#50 ] File "", line 30, characters 11-13
    [ a#46 length#45 xs#47 ] File "", line 28, characters 53-62
    [ a#43 xs#44 ] File "", line 27, characters 55-103

    Variable definitions:
    (a#59 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-3
    (a#62 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-5
    (b#64 -> b) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#60 -> c) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 6-7
    (d#63 -> d) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 10-11
    (e#61 -> e) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 12-13
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 19, character 0 to line 25, character 3
    (Crypto -> Crypto) File "", line 10, character 0 to line 18, character 3
    (List -> List) File "", line 26, character 0 to line 37, character 3
    (String -> String) File "", line 2, character 0 to line 9, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "records.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List String a#60 b#63 g#64 myrec#59 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-41
    [ Bytes Crypto List String a#60 b#63 myrec#59 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-41
    [ Bytes Crypto List String a#60 i#61 j#62 myrec#59 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-45
    [ Bytes Crypto List String a#60 i#61 myrec#59 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 27-45
    [ Bytes Crypto List String a#60 myrec#59 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 14-55
    [ Bytes Crypto List String myrec#59 ] File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 0-9
    [ a#54 generated#57 head_opt#53 length#45 size#48 xs#56 ] File "", line 36, characters 23-25
    [ a#54 head_opt#53 length#45 size#48 xs#55 ] File "", line 34, characters 11-13
    [ a#49 generated#51 length#45 size#48 x#52 xs#50 ] File "", line 32, characters 22-23
    [ a#49 length#45 size#48 xs#50 ] File "", line 30, characters 11-13
    [ a#46 length#45 xs#47 ] File "", line 28, characters 53-62
    [ a#43 xs#44 ] File "", line 27, characters 55-103

    Variable definitions:
    (a#60 -> a) File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-41 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 40-41
    (b#63 -> b) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 4-5 |resolved: myrec|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 32-33
    (e#65 -> e) File "../../test/contracts/get_scope_tests/records.mligo", line 15, characters 4-5 |resolved: myrec|
    references: []
    (g#64 -> g) File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 19-20 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-29
    (i#61 -> i) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 18-19 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 42-43
    (j#62 -> j) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 31-32 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 44-45
    Type definitions:
    (myrec#59 -> myrec) File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 0-36 :
    record[bar -> int , foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 19, character 0 to line 25, character 3
    (Crypto -> Crypto) File "", line 10, character 0 to line 18, character 3
    (List -> List) File "", line 26, character 0 to line 37, character 3
    (String -> String) File "", line 2, character 0 to line 9, character 3 |} ] ;

  run_ligo_good [ "info" ; "get-scope" ; gs "constant.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List String a#59 e#62 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-30
    [ Bytes Crypto List String a#59 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 5-32
    [ Bytes Crypto List String a#59 c#60 d#61 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ Bytes Crypto List String a#59 c#60 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    [ Bytes Crypto List String ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 0-9
    [ a#54 generated#57 head_opt#53 length#45 size#48 xs#56 ] File "", line 36, characters 23-25
    [ a#54 head_opt#53 length#45 size#48 xs#55 ] File "", line 34, characters 11-13
    [ a#49 generated#51 length#45 size#48 x#52 xs#50 ] File "", line 32, characters 22-23
    [ a#49 length#45 size#48 xs#50 ] File "", line 30, characters 11-13
    [ a#46 length#45 xs#47 ] File "", line 28, characters 53-62
    [ a#43 xs#44 ] File "", line 27, characters 55-103

    Variable definitions:
    (a#59 -> a) File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (b#63 -> b) File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5 |resolved: list (int)|
    references: []
    (c#60 -> c) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (d#61 -> d) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#62 -> e) File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 19, character 0 to line 25, character 3
    (Crypto -> Crypto) File "", line 10, character 0 to line 18, character 3
    (List -> List) File "", line 26, character 0 to line 37, character 3
    (String -> String) File "", line 2, character 0 to line 9, character 3 |} ] 

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "application.mligo" ; "--format";"dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List String c#63 f#61 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    [ Bytes Crypto List String b#62 f#61 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-19
    [ Bytes Crypto List String f#61 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 3-36
    [ Bytes Crypto List String i#59 j#60 ] File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-63
    [ a#54 generated#57 head_opt#53 length#45 size#48 xs#56 ] File "", line 36, characters 23-25
    [ a#54 head_opt#53 length#45 size#48 xs#55 ] File "", line 34, characters 11-13
    [ a#49 generated#51 length#45 size#48 x#52 xs#50 ] File "", line 32, characters 22-23
    [ a#49 length#45 size#48 xs#50 ] File "", line 30, characters 11-13
    [ a#46 length#45 xs#47 ] File "", line 28, characters 53-62
    [ a#43 xs#44 ] File "", line 27, characters 55-103

    Variable definitions:
    (a#64 -> a) File "../../test/contracts/get_scope_tests/application.mligo", line 1, characters 4-5 |resolved: int|
    references: []
    (b#62 -> b) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 7-8 |resolved: int|
    references: []
    (c#63 -> c) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    (f#61 -> f) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 6-7 |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-17
    (i#59 -> i) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 36-37 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 62-63
    (j#60 -> j) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 46-47 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-59
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 19, character 0 to line 25, character 3
    (Crypto -> Crypto) File "", line 10, character 0 to line 18, character 3
    (List -> List) File "", line 26, character 0 to line 37, character 3
    (String -> String) File "", line 2, character 0 to line 9, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "include.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List String a#59 b#64 x#65 ] File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-13
    [ Bytes Crypto List String a#59 b#64 ] File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 0-9
    [ Bytes Crypto List String a#59 c#60 d#63 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11
    [ Bytes Crypto List String a#59 c#60 e#61 f#62 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
    [ Bytes Crypto List String a#59 c#60 e#61 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    [ Bytes Crypto List String a#59 c#60 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    [ Bytes Crypto List String a#59 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    [ Bytes Crypto List String ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
    [ a#54 generated#57 head_opt#53 length#45 size#48 xs#56 ] File "", line 36, characters 23-25
    [ a#54 head_opt#53 length#45 size#48 xs#55 ] File "", line 34, characters 11-13
    [ a#49 generated#51 length#45 size#48 x#52 xs#50 ] File "", line 32, characters 22-23
    [ a#49 length#45 size#48 xs#50 ] File "", line 30, characters 11-13
    [ a#46 length#45 xs#47 ] File "", line 28, characters 53-62
    [ a#43 xs#44 ] File "", line 27, characters 55-103

    Variable definitions:
    (a#59 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    (b#64 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#60 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    (d#63 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    (e#61 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    (f#62 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    (x#65 -> x) File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-9
    (y#66 -> y) File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 19, character 0 to line 25, character 3
    (Crypto -> Crypto) File "", line 10, character 0 to line 18, character 3
    (List -> List) File "", line 26, character 0 to line 37, character 3
    (String -> String) File "", line 2, character 0 to line 9, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "bad_field_record.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List String a#62 c#60 foo_record#59 j#63 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    [ Bytes Crypto List String a#62 c#60 foo_record#59 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    [ Bytes Crypto List String c#60 foo_record#59 i#61 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    [ Bytes Crypto List String c#60 foo_record#59 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11
    [ Bytes Crypto List String foo_record#59 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 4, character 8 to line 5, character 9
    [ a#54 generated#57 head_opt#53 length#45 size#48 xs#56 ] File "", line 36, characters 23-25
    [ a#54 head_opt#53 length#45 size#48 xs#55 ] File "", line 34, characters 11-13
    [ a#49 generated#51 length#45 size#48 x#52 xs#50 ] File "", line 32, characters 22-23
    [ a#49 length#45 size#48 xs#50 ] File "", line 30, characters 11-13
    [ a#46 length#45 xs#47 ] File "", line 28, characters 53-62
    [ a#43 xs#44 ] File "", line 27, characters 55-103

    Variable definitions:
    (a#62 -> a) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 8, characters 4-5 |resolved: int|
    references: []
    (b#64 -> b) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 12, characters 4-5 |unresolved|
    references: []
    (c#60 -> c) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 3, characters 4-5 |resolved: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11 ,
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    (i#61 -> i) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    (j#63 -> j) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 6-7 |unresolved|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    Type definitions:
    (foo_record#59 -> foo_record) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 1, characters 0-43 :
    record[bar -> int , foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 19, character 0 to line 25, character 3
    (Crypto -> Crypto) File "", line 10, character 0 to line 18, character 3
    (List -> List) File "", line 26, character 0 to line 37, character 3
    (String -> String) File "", line 2, character 0 to line 9, character 3 |} ];

  run_ligo_good [ "info"; "get-scope" ; gs "nominal_types.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List String a#61 b#62 c#63 foo_record#60 foo_variant#59 p#64 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    [ Bytes Crypto List String a#61 b#62 foo_record#60 foo_variant#59 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, character 8 to line 10, character 9
    [ Bytes Crypto List String a#61 foo_record#60 foo_variant#59 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 12-14
    [ Bytes Crypto List String foo_record#60 foo_variant#59 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 12-13
    [ a#54 generated#57 head_opt#53 length#45 size#48 xs#56 ] File "", line 36, characters 23-25
    [ a#54 head_opt#53 length#45 size#48 xs#55 ] File "", line 34, characters 11-13
    [ a#49 generated#51 length#45 size#48 x#52 xs#50 ] File "", line 32, characters 22-23
    [ a#49 length#45 size#48 xs#50 ] File "", line 30, characters 11-13
    [ a#46 length#45 xs#47 ] File "", line 28, characters 53-62
    [ a#43 xs#44 ] File "", line 27, characters 55-103

    Variable definitions:
    (a#61 -> a) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    (b#62 -> b) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    (c#63 -> c) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 8, characters 4-5 |resolved: foo_record|
    references: []
    (main#65 -> main) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 4-8 |core: foo_record -> foo_variant|
    references: []
    (p#64 -> p) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 10-11 |core: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    Type definitions:
    (foo_record#60 -> foo_record) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 0-58 :
    record[bar -> foo_variant , foo -> foo_variant]
    (foo_variant#59 -> foo_variant) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 0-45 :
    sum[Bar -> string , Foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 19, character 0 to line 25, character 3
    (Crypto -> Crypto) File "", line 10, character 0 to line 18, character 3
    (List -> List) File "", line 26, character 0 to line 37, character 3
    (String -> String) File "", line 2, character 0 to line 9, character 3 |} ] ;
    
  run_ligo_good [ "info"; "get-scope" ; gs "module.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ A B Bytes C Crypto D List String a#60 b#61 ] File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 4-7
    [ ] File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 8-17

    Variable definitions:
    (a#60 -> a) File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (b#61 -> b) File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 4-5 |resolved: int|
    references: []
    (titi#63 -> titi) File "../../test/contracts/get_scope_tests/module.mligo", line 11, characters 4-8 |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A -> A) File "../../test/contracts/get_scope_tests/module.mligo", line 1, character 0 to line 3, character 3
    (B -> B) File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 0-12
    (Bytes -> Bytes) File "", line 19, character 0 to line 25, character 3
    (C -> C) File "../../test/contracts/get_scope_tests/module.mligo", line 12, character 4 to line 16, character 7
    (Crypto -> Crypto) File "", line 10, character 0 to line 18, character 3
    (D -> D) File "../../test/contracts/get_scope_tests/module.mligo", line 15, character 4 to line 16, character 7
    (List -> List) File "", line 26, character 0 to line 37, character 3
    (String -> String) File "", line 2, character 0 to line 9, character 3 |} ]