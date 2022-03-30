open Cli_expect

let gs = fun s -> ("../../test/contracts/get_scope_tests/"^s)

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda_letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Big_map Bytes Crypto List Map Option Set String a#90 f#95 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-7
[ Big_map Bytes Crypto List Map Option Set String a#90 g#93 i#91 j#92 k#94 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-21
[ Big_map Bytes Crypto List Map Option Set String a#90 g#93 i#91 j#92 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-25
[ Big_map Bytes Crypto List Map Option Set String a#90 i#91 j#92 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-21
[ Big_map Bytes Crypto List Map Option Set String ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 0-9
[ a#85 generated#88 head_opt#84 length#76 size#79 xs#87 ] File "", line 52, characters 23-25
[ a#85 head_opt#84 length#76 size#79 xs#86 ] File "", line 50, characters 11-13
[ a#80 generated#82 length#76 size#79 x#83 xs#81 ] File "", line 48, characters 22-23
[ a#80 length#76 size#79 xs#81 ] File "", line 46, characters 11-13
[ a#77 length#76 xs#78 ] File "", line 44, characters 53-62
[ a#74 xs#75 ] File "", line 43, characters 55-103

Variable definitions:
(a#90 -> a) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 6-7
(b#96 -> b) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#95 -> f) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-3
(g#93 -> g) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 24-25 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 16-17
(i#91 -> i) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 37-38 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 8-9
(j#92 -> j) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 47-48 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-5
(k#94 -> k) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 20-21
Type definitions:
Module definitions:
(Big_map -> Big_map) File "", line 6, character 0 to line 8, character 3
(Bytes -> Bytes) File "", line 34, character 0 to line 41, character 3
(Crypto -> Crypto) File "", line 25, character 0 to line 33, character 3
(List -> List) File "", line 42, character 0 to line 53, character 3
(Map -> Map) File "", line 9, character 0 to line 12, character 3
(Option -> Option) File "", line 2, character 0 to line 5, character 3
(Set -> Set) File "", line 13, character 0 to line 16, character 3
(String -> String) File "", line 17, character 0 to line 24, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Big_map Bytes Crypto List Map Option Set String a#90 c#91 d#94 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11
[ Big_map Bytes Crypto List Map Option Set String a#90 c#91 e#92 f#93 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
[ Big_map Bytes Crypto List Map Option Set String a#90 c#91 e#92 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
[ Big_map Bytes Crypto List Map Option Set String a#90 c#91 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
[ Big_map Bytes Crypto List Map Option Set String a#90 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
[ Big_map Bytes Crypto List Map Option Set String ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
[ a#85 generated#88 head_opt#84 length#76 size#79 xs#87 ] File "", line 52, characters 23-25
[ a#85 head_opt#84 length#76 size#79 xs#86 ] File "", line 50, characters 11-13
[ a#80 generated#82 length#76 size#79 x#83 xs#81 ] File "", line 48, characters 22-23
[ a#80 length#76 size#79 xs#81 ] File "", line 46, characters 11-13
[ a#77 length#76 xs#78 ] File "", line 44, characters 53-62
[ a#74 xs#75 ] File "", line 43, characters 55-103

Variable definitions:
(a#90 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
(b#95 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(c#91 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
(d#94 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
(e#92 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
(f#93 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
Type definitions:
Module definitions:
(Big_map -> Big_map) File "", line 6, character 0 to line 8, character 3
(Bytes -> Bytes) File "", line 34, character 0 to line 41, character 3
(Crypto -> Crypto) File "", line 25, character 0 to line 33, character 3
(List -> List) File "", line 42, character 0 to line 53, character 3
(Map -> Map) File "", line 9, character 0 to line 12, character 3
(Option -> Option) File "", line 2, character 0 to line 5, character 3
(Set -> Set) File "", line 13, character 0 to line 16, character 3
(String -> String) File "", line 17, character 0 to line 24, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Big_map Bytes Crypto List Map Option Set String a#90 f#93 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-7
[ Big_map Bytes Crypto List Map Option Set String a#90 i#91 j#92 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-63
[ Big_map Bytes Crypto List Map Option Set String ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 0-9
[ a#85 generated#88 head_opt#84 length#76 size#79 xs#87 ] File "", line 52, characters 23-25
[ a#85 head_opt#84 length#76 size#79 xs#86 ] File "", line 50, characters 11-13
[ a#80 generated#82 length#76 size#79 x#83 xs#81 ] File "", line 48, characters 22-23
[ a#80 length#76 size#79 xs#81 ] File "", line 46, characters 11-13
[ a#77 length#76 xs#78 ] File "", line 44, characters 53-62
[ a#74 xs#75 ] File "", line 43, characters 55-103

Variable definitions:
(a#90 -> a) File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 4-5 |resolved: int|
references: []
(b#94 -> b) File "../../test/contracts/get_scope_tests/lambda.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#93 -> f) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-3
(i#91 -> i) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 36-37 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 62-63
(j#92 -> j) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 46-47 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-59
Type definitions:
Module definitions:
(Big_map -> Big_map) File "", line 6, character 0 to line 8, character 3
(Bytes -> Bytes) File "", line 34, character 0 to line 41, character 3
(Crypto -> Crypto) File "", line 25, character 0 to line 33, character 3
(List -> List) File "", line 42, character 0 to line 53, character 3
(Map -> Map) File "", line 9, character 0 to line 12, character 3
(Option -> Option) File "", line 2, character 0 to line 5, character 3
(Set -> Set) File "", line 13, character 0 to line 16, character 3
(String -> String) File "", line 17, character 0 to line 24, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "match.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Big_map Bytes Crypto List Map Option Set String a#91 b#95 c#99 mytype#90 s#101 ] File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-21
    [ Big_map Bytes Crypto List Map Option Set String a#91 b#95 c#99 d#100 mytype#90 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-31
    [ Big_map Bytes Crypto List Map Option Set String a#91 b#95 c#99 mytype#90 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, character 9 to line 20, character 13
    [ Big_map Bytes Crypto List Map Option Set String a#91 b#95 hd#98 mytype#90 tl#97 ] File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    [ Big_map Bytes Crypto List Map Option Set String a#91 b#95 c#96 mytype#90 ] File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5
    [ Big_map Bytes Crypto List Map Option Set String a#91 b#95 mytype#90 ] File "../../test/contracts/get_scope_tests/match.mligo", line 11, character 11 to line 14, character 5
    [ Big_map Bytes Crypto List Map Option Set String a#91 mytype#90 y#94 ] File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-18
    [ Big_map Bytes Crypto List Map Option Set String a#91 mytype#90 x#93 ] File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-18
    [ Big_map Bytes Crypto List Map Option Set String a#91 c#92 mytype#90 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    [ Big_map Bytes Crypto List Map Option Set String a#91 mytype#90 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 9-27
    [ Big_map Bytes Crypto List Map Option Set String mytype#90 ] File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 0-9
    [ a#85 generated#88 head_opt#84 length#76 size#79 xs#87 ] File "", line 52, characters 23-25
    [ a#85 head_opt#84 length#76 size#79 xs#86 ] File "", line 50, characters 11-13
    [ a#80 generated#82 length#76 size#79 x#83 xs#81 ] File "", line 48, characters 22-23
    [ a#80 length#76 size#79 xs#81 ] File "", line 46, characters 11-13
    [ a#77 length#76 xs#78 ] File "", line 44, characters 53-62
    [ a#74 xs#75 ] File "", line 43, characters 55-103

    Variable definitions:
    (a#91 -> a) File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-29 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13
    (b#95 -> b) File "../../test/contracts/get_scope_tests/match.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (c#92 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    (c#96 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 8-9 |resolved: int|
    references: []
    (c#99 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 10, characters 4-5 |resolved: int|
    references: []
    (d#100 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 30-31
    (d#102 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 17, characters 4-5 |resolved: int|
    references: []
    (hd#98 -> hd) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6 |resolved: int|
    references: []
    (s#101 -> s) File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 10-11 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-17
    (tl#97 -> tl) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 8-10 |resolved: list (int)|
    references: []
    (x#93 -> x) File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-14
    (y#94 -> y) File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 8-9 |resolved: string|
    references: []
    Type definitions:
    (mytype#90 -> mytype) File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 0-40 :
    sum[Bar -> string , Foo -> int]
    Module definitions:
    (Big_map -> Big_map) File "", line 6, character 0 to line 8, character 3
    (Bytes -> Bytes) File "", line 34, character 0 to line 41, character 3
    (Crypto -> Crypto) File "", line 25, character 0 to line 33, character 3
    (List -> List) File "", line 42, character 0 to line 53, character 3
    (Map -> Map) File "", line 9, character 0 to line 12, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 13, character 0 to line 16, character 3
    (String -> String) File "", line 17, character 0 to line 24, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "rec.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Big_map Bytes Crypto List Map Option Set String a#90 b#96 c#95 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-9
    [ Big_map Bytes Crypto List Map Option Set String a#90 c#95 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 8, character 2 to line 9, character 10
    [ Big_map Bytes Crypto List Map Option Set String a#90 c#91 i#92 j#93 k#94 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-10
    [ Big_map Bytes Crypto List Map Option Set String a#90 c#91 i#92 j#93 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-21
    [ Big_map Bytes Crypto List Map Option Set String a#90 c#91 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-40
    [ Big_map Bytes Crypto List Map Option Set String ] File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 0-9
    [ a#85 generated#88 head_opt#84 length#76 size#79 xs#87 ] File "", line 52, characters 23-25
    [ a#85 head_opt#84 length#76 size#79 xs#86 ] File "", line 50, characters 11-13
    [ a#80 generated#82 length#76 size#79 x#83 xs#81 ] File "", line 48, characters 22-23
    [ a#80 length#76 size#79 xs#81 ] File "", line 46, characters 11-13
    [ a#77 length#76 xs#78 ] File "", line 44, characters 53-62
    [ a#74 xs#75 ] File "", line 43, characters 55-103

    Variable definitions:
    (a#90 -> a) File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 5-6
    (b#96 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 8-9
    (b#97 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#91 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-5
    (c#95 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-3
    (i#92 -> i) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-38 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-13
    (j#93 -> j) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 39-40 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 16-17
    (k#94 -> k) File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 7-8
    Type definitions:
    Module definitions:
    (Big_map -> Big_map) File "", line 6, character 0 to line 8, character 3
    (Bytes -> Bytes) File "", line 34, character 0 to line 41, character 3
    (Crypto -> Crypto) File "", line 25, character 0 to line 33, character 3
    (List -> List) File "", line 42, character 0 to line 53, character 3
    (Map -> Map) File "", line 9, character 0 to line 12, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 13, character 0 to line 16, character 3
    (String -> String) File "", line 17, character 0 to line 24, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "shadowing.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Big_map Bytes Crypto List Map Option Set String a#90 c#91 d#94 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-11
    [ Big_map Bytes Crypto List Map Option Set String a#93 c#91 e#92 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-13
    [ Big_map Bytes Crypto List Map Option Set String a#90 c#91 e#92 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-21
    [ Big_map Bytes Crypto List Map Option Set String a#90 c#91 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-17
    [ Big_map Bytes Crypto List Map Option Set String a#90 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-15
    [ Big_map Bytes Crypto List Map Option Set String ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 0-9
    [ a#85 generated#88 head_opt#84 length#76 size#79 xs#87 ] File "", line 52, characters 23-25
    [ a#85 head_opt#84 length#76 size#79 xs#86 ] File "", line 50, characters 11-13
    [ a#80 generated#82 length#76 size#79 x#83 xs#81 ] File "", line 48, characters 22-23
    [ a#80 length#76 size#79 xs#81 ] File "", line 46, characters 11-13
    [ a#77 length#76 xs#78 ] File "", line 44, characters 53-62
    [ a#74 xs#75 ] File "", line 43, characters 55-103

    Variable definitions:
    (a#90 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-3
    (a#93 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-5
    (b#95 -> b) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#91 -> c) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 6-7
    (d#94 -> d) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 10-11
    (e#92 -> e) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 12-13
    Type definitions:
    Module definitions:
    (Big_map -> Big_map) File "", line 6, character 0 to line 8, character 3
    (Bytes -> Bytes) File "", line 34, character 0 to line 41, character 3
    (Crypto -> Crypto) File "", line 25, character 0 to line 33, character 3
    (List -> List) File "", line 42, character 0 to line 53, character 3
    (Map -> Map) File "", line 9, character 0 to line 12, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 13, character 0 to line 16, character 3
    (String -> String) File "", line 17, character 0 to line 24, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "records.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Big_map Bytes Crypto List Map Option Set String a#91 b#94 g#95 myrec#90 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-41
    [ Big_map Bytes Crypto List Map Option Set String a#91 b#94 myrec#90 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-41
    [ Big_map Bytes Crypto List Map Option Set String a#91 i#92 j#93 myrec#90 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-45
    [ Big_map Bytes Crypto List Map Option Set String a#91 i#92 myrec#90 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 27-45
    [ Big_map Bytes Crypto List Map Option Set String a#91 myrec#90 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 14-55
    [ Big_map Bytes Crypto List Map Option Set String myrec#90 ] File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 0-9
    [ a#85 generated#88 head_opt#84 length#76 size#79 xs#87 ] File "", line 52, characters 23-25
    [ a#85 head_opt#84 length#76 size#79 xs#86 ] File "", line 50, characters 11-13
    [ a#80 generated#82 length#76 size#79 x#83 xs#81 ] File "", line 48, characters 22-23
    [ a#80 length#76 size#79 xs#81 ] File "", line 46, characters 11-13
    [ a#77 length#76 xs#78 ] File "", line 44, characters 53-62
    [ a#74 xs#75 ] File "", line 43, characters 55-103

    Variable definitions:
    (a#91 -> a) File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-41 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 40-41
    (b#94 -> b) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 4-5 |resolved: myrec|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 32-33
    (e#96 -> e) File "../../test/contracts/get_scope_tests/records.mligo", line 15, characters 4-5 |resolved: myrec|
    references: []
    (g#95 -> g) File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 19-20 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-29
    (i#92 -> i) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 18-19 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 42-43
    (j#93 -> j) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 31-32 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 44-45
    Type definitions:
    (myrec#90 -> myrec) File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 0-36 :
    record[bar -> int , foo -> int]
    Module definitions:
    (Big_map -> Big_map) File "", line 6, character 0 to line 8, character 3
    (Bytes -> Bytes) File "", line 34, character 0 to line 41, character 3
    (Crypto -> Crypto) File "", line 25, character 0 to line 33, character 3
    (List -> List) File "", line 42, character 0 to line 53, character 3
    (Map -> Map) File "", line 9, character 0 to line 12, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 13, character 0 to line 16, character 3
    (String -> String) File "", line 17, character 0 to line 24, character 3 |} ] ;

  run_ligo_good [ "info" ; "get-scope" ; gs "constant.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Big_map Bytes Crypto List Map Option Set String a#90 e#93 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-30
    [ Big_map Bytes Crypto List Map Option Set String a#90 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 5-32
    [ Big_map Bytes Crypto List Map Option Set String a#90 c#91 d#92 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ Big_map Bytes Crypto List Map Option Set String a#90 c#91 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    [ Big_map Bytes Crypto List Map Option Set String ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 0-9
    [ a#85 generated#88 head_opt#84 length#76 size#79 xs#87 ] File "", line 52, characters 23-25
    [ a#85 head_opt#84 length#76 size#79 xs#86 ] File "", line 50, characters 11-13
    [ a#80 generated#82 length#76 size#79 x#83 xs#81 ] File "", line 48, characters 22-23
    [ a#80 length#76 size#79 xs#81 ] File "", line 46, characters 11-13
    [ a#77 length#76 xs#78 ] File "", line 44, characters 53-62
    [ a#74 xs#75 ] File "", line 43, characters 55-103

    Variable definitions:
    (a#90 -> a) File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (b#94 -> b) File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5 |resolved: list (int)|
    references: []
    (c#91 -> c) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (d#92 -> d) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#93 -> e) File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    Type definitions:
    Module definitions:
    (Big_map -> Big_map) File "", line 6, character 0 to line 8, character 3
    (Bytes -> Bytes) File "", line 34, character 0 to line 41, character 3
    (Crypto -> Crypto) File "", line 25, character 0 to line 33, character 3
    (List -> List) File "", line 42, character 0 to line 53, character 3
    (Map -> Map) File "", line 9, character 0 to line 12, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 13, character 0 to line 16, character 3
    (String -> String) File "", line 17, character 0 to line 24, character 3 |} ] 

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "application.mligo" ; "--format";"dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Big_map Bytes Crypto List Map Option Set String c#94 f#92 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    [ Big_map Bytes Crypto List Map Option Set String b#93 f#92 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-19
    [ Big_map Bytes Crypto List Map Option Set String f#92 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 3-36
    [ Big_map Bytes Crypto List Map Option Set String i#90 j#91 ] File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-63
    [ a#85 generated#88 head_opt#84 length#76 size#79 xs#87 ] File "", line 52, characters 23-25
    [ a#85 head_opt#84 length#76 size#79 xs#86 ] File "", line 50, characters 11-13
    [ a#80 generated#82 length#76 size#79 x#83 xs#81 ] File "", line 48, characters 22-23
    [ a#80 length#76 size#79 xs#81 ] File "", line 46, characters 11-13
    [ a#77 length#76 xs#78 ] File "", line 44, characters 53-62
    [ a#74 xs#75 ] File "", line 43, characters 55-103

    Variable definitions:
    (a#95 -> a) File "../../test/contracts/get_scope_tests/application.mligo", line 1, characters 4-5 |resolved: int|
    references: []
    (b#93 -> b) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 7-8 |resolved: int|
    references: []
    (c#94 -> c) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    (f#92 -> f) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 6-7 |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-17
    (i#90 -> i) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 36-37 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 62-63
    (j#91 -> j) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 46-47 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-59
    Type definitions:
    Module definitions:
    (Big_map -> Big_map) File "", line 6, character 0 to line 8, character 3
    (Bytes -> Bytes) File "", line 34, character 0 to line 41, character 3
    (Crypto -> Crypto) File "", line 25, character 0 to line 33, character 3
    (List -> List) File "", line 42, character 0 to line 53, character 3
    (Map -> Map) File "", line 9, character 0 to line 12, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 13, character 0 to line 16, character 3
    (String -> String) File "", line 17, character 0 to line 24, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "include.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Big_map Bytes Crypto List Map Option Set String a#90 b#95 x#96 ] File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-13
    [ Big_map Bytes Crypto List Map Option Set String a#90 b#95 ] File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 0-9
    [ Big_map Bytes Crypto List Map Option Set String a#90 c#91 d#94 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11
    [ Big_map Bytes Crypto List Map Option Set String a#90 c#91 e#92 f#93 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
    [ Big_map Bytes Crypto List Map Option Set String a#90 c#91 e#92 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    [ Big_map Bytes Crypto List Map Option Set String a#90 c#91 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    [ Big_map Bytes Crypto List Map Option Set String a#90 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    [ Big_map Bytes Crypto List Map Option Set String ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
    [ a#85 generated#88 head_opt#84 length#76 size#79 xs#87 ] File "", line 52, characters 23-25
    [ a#85 head_opt#84 length#76 size#79 xs#86 ] File "", line 50, characters 11-13
    [ a#80 generated#82 length#76 size#79 x#83 xs#81 ] File "", line 48, characters 22-23
    [ a#80 length#76 size#79 xs#81 ] File "", line 46, characters 11-13
    [ a#77 length#76 xs#78 ] File "", line 44, characters 53-62
    [ a#74 xs#75 ] File "", line 43, characters 55-103

    Variable definitions:
    (a#90 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    (b#95 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#91 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    (d#94 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    (e#92 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    (f#93 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    (x#96 -> x) File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-9
    (y#97 -> y) File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (Big_map -> Big_map) File "", line 6, character 0 to line 8, character 3
    (Bytes -> Bytes) File "", line 34, character 0 to line 41, character 3
    (Crypto -> Crypto) File "", line 25, character 0 to line 33, character 3
    (List -> List) File "", line 42, character 0 to line 53, character 3
    (Map -> Map) File "", line 9, character 0 to line 12, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 13, character 0 to line 16, character 3
    (String -> String) File "", line 17, character 0 to line 24, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "bad_field_record.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Big_map Bytes Crypto List Map Option Set String a#93 c#91 foo_record#90 j#94 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    [ Big_map Bytes Crypto List Map Option Set String a#93 c#91 foo_record#90 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    [ Big_map Bytes Crypto List Map Option Set String c#91 foo_record#90 i#92 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    [ Big_map Bytes Crypto List Map Option Set String c#91 foo_record#90 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11
    [ Big_map Bytes Crypto List Map Option Set String foo_record#90 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 4, character 8 to line 5, character 9
    [ a#85 generated#88 head_opt#84 length#76 size#79 xs#87 ] File "", line 52, characters 23-25
    [ a#85 head_opt#84 length#76 size#79 xs#86 ] File "", line 50, characters 11-13
    [ a#80 generated#82 length#76 size#79 x#83 xs#81 ] File "", line 48, characters 22-23
    [ a#80 length#76 size#79 xs#81 ] File "", line 46, characters 11-13
    [ a#77 length#76 xs#78 ] File "", line 44, characters 53-62
    [ a#74 xs#75 ] File "", line 43, characters 55-103

    Variable definitions:
    (a#93 -> a) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 8, characters 4-5 |resolved: int|
    references: []
    (b#95 -> b) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 12, characters 4-5 |unresolved|
    references: []
    (c#91 -> c) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 3, characters 4-5 |resolved: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11 ,
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    (i#92 -> i) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    (j#94 -> j) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 6-7 |unresolved|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    Type definitions:
    (foo_record#90 -> foo_record) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 1, characters 0-43 :
    record[bar -> int , foo -> int]
    Module definitions:
    (Big_map -> Big_map) File "", line 6, character 0 to line 8, character 3
    (Bytes -> Bytes) File "", line 34, character 0 to line 41, character 3
    (Crypto -> Crypto) File "", line 25, character 0 to line 33, character 3
    (List -> List) File "", line 42, character 0 to line 53, character 3
    (Map -> Map) File "", line 9, character 0 to line 12, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 13, character 0 to line 16, character 3
    (String -> String) File "", line 17, character 0 to line 24, character 3 |} ];

  run_ligo_good [ "info"; "get-scope" ; gs "nominal_types.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Big_map Bytes Crypto List Map Option Set String a#92 b#93 c#94 foo_record#91 foo_variant#90 p#95 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    [ Big_map Bytes Crypto List Map Option Set String a#92 b#93 foo_record#91 foo_variant#90 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, character 8 to line 10, character 9
    [ Big_map Bytes Crypto List Map Option Set String a#92 foo_record#91 foo_variant#90 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 12-14
    [ Big_map Bytes Crypto List Map Option Set String foo_record#91 foo_variant#90 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 12-13
    [ a#85 generated#88 head_opt#84 length#76 size#79 xs#87 ] File "", line 52, characters 23-25
    [ a#85 head_opt#84 length#76 size#79 xs#86 ] File "", line 50, characters 11-13
    [ a#80 generated#82 length#76 size#79 x#83 xs#81 ] File "", line 48, characters 22-23
    [ a#80 length#76 size#79 xs#81 ] File "", line 46, characters 11-13
    [ a#77 length#76 xs#78 ] File "", line 44, characters 53-62
    [ a#74 xs#75 ] File "", line 43, characters 55-103

    Variable definitions:
    (a#92 -> a) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    (b#93 -> b) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    (c#94 -> c) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 8, characters 4-5 |resolved: foo_record|
    references: []
    (main#96 -> main) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 4-8 |core: foo_record -> foo_variant|
    references: []
    (p#95 -> p) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 10-11 |core: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    Type definitions:
    (foo_record#91 -> foo_record) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 0-58 :
    record[bar -> foo_variant , foo -> foo_variant]
    (foo_variant#90 -> foo_variant) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 0-45 :
    sum[Bar -> string , Foo -> int]
    Module definitions:
    (Big_map -> Big_map) File "", line 6, character 0 to line 8, character 3
    (Bytes -> Bytes) File "", line 34, character 0 to line 41, character 3
    (Crypto -> Crypto) File "", line 25, character 0 to line 33, character 3
    (List -> List) File "", line 42, character 0 to line 53, character 3
    (Map -> Map) File "", line 9, character 0 to line 12, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 13, character 0 to line 16, character 3
    (String -> String) File "", line 17, character 0 to line 24, character 3 |} ] ;
    
  run_ligo_good [ "info"; "get-scope" ; gs "module.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ A B Big_map Bytes C Crypto D List Map Option Set String a#91 b#92 ] File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 4-7
    [ ] File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 8-17

    Variable definitions:
    (a#91 -> a) File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (b#92 -> b) File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 4-5 |resolved: int|
    references: []
    (titi#94 -> titi) File "../../test/contracts/get_scope_tests/module.mligo", line 11, characters 4-8 |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A -> A) File "../../test/contracts/get_scope_tests/module.mligo", line 1, character 0 to line 3, character 3
    (B -> B) File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 0-12
    (Big_map -> Big_map) File "", line 6, character 0 to line 8, character 3
    (Bytes -> Bytes) File "", line 34, character 0 to line 41, character 3
    (C -> C) File "../../test/contracts/get_scope_tests/module.mligo", line 12, character 4 to line 16, character 7
    (Crypto -> Crypto) File "", line 25, character 0 to line 33, character 3
    (D -> D) File "../../test/contracts/get_scope_tests/module.mligo", line 15, character 4 to line 16, character 7
    (List -> List) File "", line 42, character 0 to line 53, character 3
    (Map -> Map) File "", line 9, character 0 to line 12, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 13, character 0 to line 16, character 3
    (String -> String) File "", line 17, character 0 to line 24, character 3 |} ]