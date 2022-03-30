open Cli_expect

let gs = fun s -> ("../../test/contracts/get_scope_tests/"^s)

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda_letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes Crypto List Map Set String a#69 f#74 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-7
[ Bytes Crypto List Map Set String a#69 g#72 i#70 j#71 k#73 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-21
[ Bytes Crypto List Map Set String a#69 g#72 i#70 j#71 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-25
[ Bytes Crypto List Map Set String a#69 i#70 j#71 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-21
[ Bytes Crypto List Map Set String ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 0-9
[ a#64 generated#67 head_opt#63 length#55 size#58 xs#66 ] File "", line 43, characters 23-25
[ a#64 head_opt#63 length#55 size#58 xs#65 ] File "", line 41, characters 11-13
[ a#59 generated#61 length#55 size#58 x#62 xs#60 ] File "", line 39, characters 22-23
[ a#59 length#55 size#58 xs#60 ] File "", line 37, characters 11-13
[ a#56 length#55 xs#57 ] File "", line 35, characters 53-62
[ a#53 xs#54 ] File "", line 34, characters 55-103

Variable definitions:
(a#69 -> a) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 6-7
(b#75 -> b) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#74 -> f) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-3
(g#72 -> g) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 24-25 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 16-17
(i#70 -> i) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 37-38 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 8-9
(j#71 -> j) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 47-48 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-5
(k#73 -> k) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 20-21
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 26, character 0 to line 32, character 3
(Crypto -> Crypto) File "", line 17, character 0 to line 25, character 3
(List -> List) File "", line 33, character 0 to line 44, character 3
(Map -> Map) File "", line 2, character 0 to line 4, character 3
(Set -> Set) File "", line 5, character 0 to line 8, character 3
(String -> String) File "", line 9, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes Crypto List Map Set String a#69 c#70 d#73 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11
[ Bytes Crypto List Map Set String a#69 c#70 e#71 f#72 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
[ Bytes Crypto List Map Set String a#69 c#70 e#71 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
[ Bytes Crypto List Map Set String a#69 c#70 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
[ Bytes Crypto List Map Set String a#69 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
[ Bytes Crypto List Map Set String ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
[ a#64 generated#67 head_opt#63 length#55 size#58 xs#66 ] File "", line 43, characters 23-25
[ a#64 head_opt#63 length#55 size#58 xs#65 ] File "", line 41, characters 11-13
[ a#59 generated#61 length#55 size#58 x#62 xs#60 ] File "", line 39, characters 22-23
[ a#59 length#55 size#58 xs#60 ] File "", line 37, characters 11-13
[ a#56 length#55 xs#57 ] File "", line 35, characters 53-62
[ a#53 xs#54 ] File "", line 34, characters 55-103

Variable definitions:
(a#69 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
(b#74 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(c#70 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
(d#73 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
(e#71 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
(f#72 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 26, character 0 to line 32, character 3
(Crypto -> Crypto) File "", line 17, character 0 to line 25, character 3
(List -> List) File "", line 33, character 0 to line 44, character 3
(Map -> Map) File "", line 2, character 0 to line 4, character 3
(Set -> Set) File "", line 5, character 0 to line 8, character 3
(String -> String) File "", line 9, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes Crypto List Map Set String a#69 f#72 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-7
[ Bytes Crypto List Map Set String a#69 i#70 j#71 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-63
[ Bytes Crypto List Map Set String ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 0-9
[ a#64 generated#67 head_opt#63 length#55 size#58 xs#66 ] File "", line 43, characters 23-25
[ a#64 head_opt#63 length#55 size#58 xs#65 ] File "", line 41, characters 11-13
[ a#59 generated#61 length#55 size#58 x#62 xs#60 ] File "", line 39, characters 22-23
[ a#59 length#55 size#58 xs#60 ] File "", line 37, characters 11-13
[ a#56 length#55 xs#57 ] File "", line 35, characters 53-62
[ a#53 xs#54 ] File "", line 34, characters 55-103

Variable definitions:
(a#69 -> a) File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 4-5 |resolved: int|
references: []
(b#73 -> b) File "../../test/contracts/get_scope_tests/lambda.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#72 -> f) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-3
(i#70 -> i) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 36-37 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 62-63
(j#71 -> j) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 46-47 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-59
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 26, character 0 to line 32, character 3
(Crypto -> Crypto) File "", line 17, character 0 to line 25, character 3
(List -> List) File "", line 33, character 0 to line 44, character 3
(Map -> Map) File "", line 2, character 0 to line 4, character 3
(Set -> Set) File "", line 5, character 0 to line 8, character 3
(String -> String) File "", line 9, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "match.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Set String a#70 b#74 c#78 mytype#69 s#80 ] File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-21
    [ Bytes Crypto List Map Set String a#70 b#74 c#78 d#79 mytype#69 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-31
    [ Bytes Crypto List Map Set String a#70 b#74 c#78 mytype#69 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, character 9 to line 20, character 13
    [ Bytes Crypto List Map Set String a#70 b#74 hd#77 mytype#69 tl#76 ] File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    [ Bytes Crypto List Map Set String a#70 b#74 c#75 mytype#69 ] File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5
    [ Bytes Crypto List Map Set String a#70 b#74 mytype#69 ] File "../../test/contracts/get_scope_tests/match.mligo", line 11, character 11 to line 14, character 5
    [ Bytes Crypto List Map Set String a#70 mytype#69 y#73 ] File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-18
    [ Bytes Crypto List Map Set String a#70 mytype#69 x#72 ] File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-18
    [ Bytes Crypto List Map Set String a#70 c#71 mytype#69 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    [ Bytes Crypto List Map Set String a#70 mytype#69 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 9-27
    [ Bytes Crypto List Map Set String mytype#69 ] File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 0-9
    [ a#64 generated#67 head_opt#63 length#55 size#58 xs#66 ] File "", line 43, characters 23-25
    [ a#64 head_opt#63 length#55 size#58 xs#65 ] File "", line 41, characters 11-13
    [ a#59 generated#61 length#55 size#58 x#62 xs#60 ] File "", line 39, characters 22-23
    [ a#59 length#55 size#58 xs#60 ] File "", line 37, characters 11-13
    [ a#56 length#55 xs#57 ] File "", line 35, characters 53-62
    [ a#53 xs#54 ] File "", line 34, characters 55-103

    Variable definitions:
    (a#70 -> a) File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-29 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13
    (b#74 -> b) File "../../test/contracts/get_scope_tests/match.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (c#71 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    (c#75 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 8-9 |resolved: int|
    references: []
    (c#78 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 10, characters 4-5 |resolved: int|
    references: []
    (d#79 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 30-31
    (d#81 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 17, characters 4-5 |resolved: int|
    references: []
    (hd#77 -> hd) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6 |resolved: int|
    references: []
    (s#80 -> s) File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 10-11 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-17
    (tl#76 -> tl) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 8-10 |resolved: list (int)|
    references: []
    (x#72 -> x) File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-14
    (y#73 -> y) File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 8-9 |resolved: string|
    references: []
    Type definitions:
    (mytype#69 -> mytype) File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 0-40 :
    sum[Bar -> string , Foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 26, character 0 to line 32, character 3
    (Crypto -> Crypto) File "", line 17, character 0 to line 25, character 3
    (List -> List) File "", line 33, character 0 to line 44, character 3
    (Map -> Map) File "", line 2, character 0 to line 4, character 3
    (Set -> Set) File "", line 5, character 0 to line 8, character 3
    (String -> String) File "", line 9, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "rec.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Set String a#69 b#75 c#74 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-9
    [ Bytes Crypto List Map Set String a#69 c#74 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 8, character 2 to line 9, character 10
    [ Bytes Crypto List Map Set String a#69 c#70 i#71 j#72 k#73 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-10
    [ Bytes Crypto List Map Set String a#69 c#70 i#71 j#72 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-21
    [ Bytes Crypto List Map Set String a#69 c#70 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-40
    [ Bytes Crypto List Map Set String ] File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 0-9
    [ a#64 generated#67 head_opt#63 length#55 size#58 xs#66 ] File "", line 43, characters 23-25
    [ a#64 head_opt#63 length#55 size#58 xs#65 ] File "", line 41, characters 11-13
    [ a#59 generated#61 length#55 size#58 x#62 xs#60 ] File "", line 39, characters 22-23
    [ a#59 length#55 size#58 xs#60 ] File "", line 37, characters 11-13
    [ a#56 length#55 xs#57 ] File "", line 35, characters 53-62
    [ a#53 xs#54 ] File "", line 34, characters 55-103

    Variable definitions:
    (a#69 -> a) File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 5-6
    (b#75 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 8-9
    (b#76 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#70 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-5
    (c#74 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-3
    (i#71 -> i) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-38 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-13
    (j#72 -> j) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 39-40 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 16-17
    (k#73 -> k) File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 7-8
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 26, character 0 to line 32, character 3
    (Crypto -> Crypto) File "", line 17, character 0 to line 25, character 3
    (List -> List) File "", line 33, character 0 to line 44, character 3
    (Map -> Map) File "", line 2, character 0 to line 4, character 3
    (Set -> Set) File "", line 5, character 0 to line 8, character 3
    (String -> String) File "", line 9, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "shadowing.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Set String a#69 c#70 d#73 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-11
    [ Bytes Crypto List Map Set String a#72 c#70 e#71 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-13
    [ Bytes Crypto List Map Set String a#69 c#70 e#71 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-21
    [ Bytes Crypto List Map Set String a#69 c#70 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-17
    [ Bytes Crypto List Map Set String a#69 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-15
    [ Bytes Crypto List Map Set String ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 0-9
    [ a#64 generated#67 head_opt#63 length#55 size#58 xs#66 ] File "", line 43, characters 23-25
    [ a#64 head_opt#63 length#55 size#58 xs#65 ] File "", line 41, characters 11-13
    [ a#59 generated#61 length#55 size#58 x#62 xs#60 ] File "", line 39, characters 22-23
    [ a#59 length#55 size#58 xs#60 ] File "", line 37, characters 11-13
    [ a#56 length#55 xs#57 ] File "", line 35, characters 53-62
    [ a#53 xs#54 ] File "", line 34, characters 55-103

    Variable definitions:
    (a#69 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-3
    (a#72 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-5
    (b#74 -> b) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#70 -> c) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 6-7
    (d#73 -> d) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 10-11
    (e#71 -> e) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 12-13
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 26, character 0 to line 32, character 3
    (Crypto -> Crypto) File "", line 17, character 0 to line 25, character 3
    (List -> List) File "", line 33, character 0 to line 44, character 3
    (Map -> Map) File "", line 2, character 0 to line 4, character 3
    (Set -> Set) File "", line 5, character 0 to line 8, character 3
    (String -> String) File "", line 9, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "records.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Set String a#70 b#73 g#74 myrec#69 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-41
    [ Bytes Crypto List Map Set String a#70 b#73 myrec#69 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-41
    [ Bytes Crypto List Map Set String a#70 i#71 j#72 myrec#69 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-45
    [ Bytes Crypto List Map Set String a#70 i#71 myrec#69 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 27-45
    [ Bytes Crypto List Map Set String a#70 myrec#69 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 14-55
    [ Bytes Crypto List Map Set String myrec#69 ] File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 0-9
    [ a#64 generated#67 head_opt#63 length#55 size#58 xs#66 ] File "", line 43, characters 23-25
    [ a#64 head_opt#63 length#55 size#58 xs#65 ] File "", line 41, characters 11-13
    [ a#59 generated#61 length#55 size#58 x#62 xs#60 ] File "", line 39, characters 22-23
    [ a#59 length#55 size#58 xs#60 ] File "", line 37, characters 11-13
    [ a#56 length#55 xs#57 ] File "", line 35, characters 53-62
    [ a#53 xs#54 ] File "", line 34, characters 55-103

    Variable definitions:
    (a#70 -> a) File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-41 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 40-41
    (b#73 -> b) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 4-5 |resolved: myrec|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 32-33
    (e#75 -> e) File "../../test/contracts/get_scope_tests/records.mligo", line 15, characters 4-5 |resolved: myrec|
    references: []
    (g#74 -> g) File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 19-20 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-29
    (i#71 -> i) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 18-19 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 42-43
    (j#72 -> j) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 31-32 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 44-45
    Type definitions:
    (myrec#69 -> myrec) File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 0-36 :
    record[bar -> int , foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 26, character 0 to line 32, character 3
    (Crypto -> Crypto) File "", line 17, character 0 to line 25, character 3
    (List -> List) File "", line 33, character 0 to line 44, character 3
    (Map -> Map) File "", line 2, character 0 to line 4, character 3
    (Set -> Set) File "", line 5, character 0 to line 8, character 3
    (String -> String) File "", line 9, character 0 to line 16, character 3 |} ] ;

  run_ligo_good [ "info" ; "get-scope" ; gs "constant.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Set String a#69 e#72 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-30
    [ Bytes Crypto List Map Set String a#69 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 5-32
    [ Bytes Crypto List Map Set String a#69 c#70 d#71 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ Bytes Crypto List Map Set String a#69 c#70 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    [ Bytes Crypto List Map Set String ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 0-9
    [ a#64 generated#67 head_opt#63 length#55 size#58 xs#66 ] File "", line 43, characters 23-25
    [ a#64 head_opt#63 length#55 size#58 xs#65 ] File "", line 41, characters 11-13
    [ a#59 generated#61 length#55 size#58 x#62 xs#60 ] File "", line 39, characters 22-23
    [ a#59 length#55 size#58 xs#60 ] File "", line 37, characters 11-13
    [ a#56 length#55 xs#57 ] File "", line 35, characters 53-62
    [ a#53 xs#54 ] File "", line 34, characters 55-103

    Variable definitions:
    (a#69 -> a) File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (b#73 -> b) File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5 |resolved: list (int)|
    references: []
    (c#70 -> c) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (d#71 -> d) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#72 -> e) File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 26, character 0 to line 32, character 3
    (Crypto -> Crypto) File "", line 17, character 0 to line 25, character 3
    (List -> List) File "", line 33, character 0 to line 44, character 3
    (Map -> Map) File "", line 2, character 0 to line 4, character 3
    (Set -> Set) File "", line 5, character 0 to line 8, character 3
    (String -> String) File "", line 9, character 0 to line 16, character 3 |} ] 

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "application.mligo" ; "--format";"dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Set String c#73 f#71 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    [ Bytes Crypto List Map Set String b#72 f#71 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-19
    [ Bytes Crypto List Map Set String f#71 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 3-36
    [ Bytes Crypto List Map Set String i#69 j#70 ] File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-63
    [ a#64 generated#67 head_opt#63 length#55 size#58 xs#66 ] File "", line 43, characters 23-25
    [ a#64 head_opt#63 length#55 size#58 xs#65 ] File "", line 41, characters 11-13
    [ a#59 generated#61 length#55 size#58 x#62 xs#60 ] File "", line 39, characters 22-23
    [ a#59 length#55 size#58 xs#60 ] File "", line 37, characters 11-13
    [ a#56 length#55 xs#57 ] File "", line 35, characters 53-62
    [ a#53 xs#54 ] File "", line 34, characters 55-103

    Variable definitions:
    (a#74 -> a) File "../../test/contracts/get_scope_tests/application.mligo", line 1, characters 4-5 |resolved: int|
    references: []
    (b#72 -> b) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 7-8 |resolved: int|
    references: []
    (c#73 -> c) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    (f#71 -> f) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 6-7 |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-17
    (i#69 -> i) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 36-37 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 62-63
    (j#70 -> j) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 46-47 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-59
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 26, character 0 to line 32, character 3
    (Crypto -> Crypto) File "", line 17, character 0 to line 25, character 3
    (List -> List) File "", line 33, character 0 to line 44, character 3
    (Map -> Map) File "", line 2, character 0 to line 4, character 3
    (Set -> Set) File "", line 5, character 0 to line 8, character 3
    (String -> String) File "", line 9, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "include.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Set String a#69 b#74 x#75 ] File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-13
    [ Bytes Crypto List Map Set String a#69 b#74 ] File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 0-9
    [ Bytes Crypto List Map Set String a#69 c#70 d#73 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11
    [ Bytes Crypto List Map Set String a#69 c#70 e#71 f#72 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
    [ Bytes Crypto List Map Set String a#69 c#70 e#71 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    [ Bytes Crypto List Map Set String a#69 c#70 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    [ Bytes Crypto List Map Set String a#69 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    [ Bytes Crypto List Map Set String ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
    [ a#64 generated#67 head_opt#63 length#55 size#58 xs#66 ] File "", line 43, characters 23-25
    [ a#64 head_opt#63 length#55 size#58 xs#65 ] File "", line 41, characters 11-13
    [ a#59 generated#61 length#55 size#58 x#62 xs#60 ] File "", line 39, characters 22-23
    [ a#59 length#55 size#58 xs#60 ] File "", line 37, characters 11-13
    [ a#56 length#55 xs#57 ] File "", line 35, characters 53-62
    [ a#53 xs#54 ] File "", line 34, characters 55-103

    Variable definitions:
    (a#69 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    (b#74 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#70 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    (d#73 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    (e#71 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    (f#72 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    (x#75 -> x) File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-9
    (y#76 -> y) File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 26, character 0 to line 32, character 3
    (Crypto -> Crypto) File "", line 17, character 0 to line 25, character 3
    (List -> List) File "", line 33, character 0 to line 44, character 3
    (Map -> Map) File "", line 2, character 0 to line 4, character 3
    (Set -> Set) File "", line 5, character 0 to line 8, character 3
    (String -> String) File "", line 9, character 0 to line 16, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "bad_field_record.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Set String a#72 c#70 foo_record#69 j#73 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    [ Bytes Crypto List Map Set String a#72 c#70 foo_record#69 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    [ Bytes Crypto List Map Set String c#70 foo_record#69 i#71 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    [ Bytes Crypto List Map Set String c#70 foo_record#69 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11
    [ Bytes Crypto List Map Set String foo_record#69 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 4, character 8 to line 5, character 9
    [ a#64 generated#67 head_opt#63 length#55 size#58 xs#66 ] File "", line 43, characters 23-25
    [ a#64 head_opt#63 length#55 size#58 xs#65 ] File "", line 41, characters 11-13
    [ a#59 generated#61 length#55 size#58 x#62 xs#60 ] File "", line 39, characters 22-23
    [ a#59 length#55 size#58 xs#60 ] File "", line 37, characters 11-13
    [ a#56 length#55 xs#57 ] File "", line 35, characters 53-62
    [ a#53 xs#54 ] File "", line 34, characters 55-103

    Variable definitions:
    (a#72 -> a) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 8, characters 4-5 |resolved: int|
    references: []
    (b#74 -> b) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 12, characters 4-5 |unresolved|
    references: []
    (c#70 -> c) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 3, characters 4-5 |resolved: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11 ,
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    (i#71 -> i) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    (j#73 -> j) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 6-7 |unresolved|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    Type definitions:
    (foo_record#69 -> foo_record) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 1, characters 0-43 :
    record[bar -> int , foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 26, character 0 to line 32, character 3
    (Crypto -> Crypto) File "", line 17, character 0 to line 25, character 3
    (List -> List) File "", line 33, character 0 to line 44, character 3
    (Map -> Map) File "", line 2, character 0 to line 4, character 3
    (Set -> Set) File "", line 5, character 0 to line 8, character 3
    (String -> String) File "", line 9, character 0 to line 16, character 3 |} ];

  run_ligo_good [ "info"; "get-scope" ; gs "nominal_types.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Set String a#71 b#72 c#73 foo_record#70 foo_variant#69 p#74 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    [ Bytes Crypto List Map Set String a#71 b#72 foo_record#70 foo_variant#69 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, character 8 to line 10, character 9
    [ Bytes Crypto List Map Set String a#71 foo_record#70 foo_variant#69 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 12-14
    [ Bytes Crypto List Map Set String foo_record#70 foo_variant#69 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 12-13
    [ a#64 generated#67 head_opt#63 length#55 size#58 xs#66 ] File "", line 43, characters 23-25
    [ a#64 head_opt#63 length#55 size#58 xs#65 ] File "", line 41, characters 11-13
    [ a#59 generated#61 length#55 size#58 x#62 xs#60 ] File "", line 39, characters 22-23
    [ a#59 length#55 size#58 xs#60 ] File "", line 37, characters 11-13
    [ a#56 length#55 xs#57 ] File "", line 35, characters 53-62
    [ a#53 xs#54 ] File "", line 34, characters 55-103

    Variable definitions:
    (a#71 -> a) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    (b#72 -> b) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    (c#73 -> c) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 8, characters 4-5 |resolved: foo_record|
    references: []
    (main#75 -> main) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 4-8 |core: foo_record -> foo_variant|
    references: []
    (p#74 -> p) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 10-11 |core: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    Type definitions:
    (foo_record#70 -> foo_record) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 0-58 :
    record[bar -> foo_variant , foo -> foo_variant]
    (foo_variant#69 -> foo_variant) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 0-45 :
    sum[Bar -> string , Foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 26, character 0 to line 32, character 3
    (Crypto -> Crypto) File "", line 17, character 0 to line 25, character 3
    (List -> List) File "", line 33, character 0 to line 44, character 3
    (Map -> Map) File "", line 2, character 0 to line 4, character 3
    (Set -> Set) File "", line 5, character 0 to line 8, character 3
    (String -> String) File "", line 9, character 0 to line 16, character 3 |} ] ;
    
  run_ligo_good [ "info"; "get-scope" ; gs "module.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ A B Bytes C Crypto D List Map Set String a#70 b#71 ] File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 4-7
    [ ] File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 8-17

    Variable definitions:
    (a#70 -> a) File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (b#71 -> b) File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 4-5 |resolved: int|
    references: []
    (titi#73 -> titi) File "../../test/contracts/get_scope_tests/module.mligo", line 11, characters 4-8 |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A -> A) File "../../test/contracts/get_scope_tests/module.mligo", line 1, character 0 to line 3, character 3
    (B -> B) File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 0-12
    (Bytes -> Bytes) File "", line 26, character 0 to line 32, character 3
    (C -> C) File "../../test/contracts/get_scope_tests/module.mligo", line 12, character 4 to line 16, character 7
    (Crypto -> Crypto) File "", line 17, character 0 to line 25, character 3
    (D -> D) File "../../test/contracts/get_scope_tests/module.mligo", line 15, character 4 to line 16, character 7
    (List -> List) File "", line 33, character 0 to line 44, character 3
    (Map -> Map) File "", line 2, character 0 to line 4, character 3
    (Set -> Set) File "", line 5, character 0 to line 8, character 3
    (String -> String) File "", line 9, character 0 to line 16, character 3 |} ]