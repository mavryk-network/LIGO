open Cli_expect

let gs = fun s -> ("../../test/contracts/get_scope_tests/"^s)

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda_letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes Crypto List Map Option Set String a#80 f#85 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-7
[ Bytes Crypto List Map Option Set String a#80 g#83 i#81 j#82 k#84 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-21
[ Bytes Crypto List Map Option Set String a#80 g#83 i#81 j#82 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-25
[ Bytes Crypto List Map Option Set String a#80 i#81 j#82 ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-21
[ Bytes Crypto List Map Option Set String ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 0-9
[ a#75 generated#78 head_opt#74 length#66 size#69 xs#77 ] File "", line 48, characters 23-25
[ a#75 head_opt#74 length#66 size#69 xs#76 ] File "", line 46, characters 11-13
[ a#70 generated#72 length#66 size#69 x#73 xs#71 ] File "", line 44, characters 22-23
[ a#70 length#66 size#69 xs#71 ] File "", line 42, characters 11-13
[ a#67 length#66 xs#68 ] File "", line 40, characters 53-62
[ a#64 xs#65 ] File "", line 39, characters 55-103

Variable definitions:
(a#80 -> a) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 6-7
(b#86 -> b) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#85 -> f) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-3
(g#83 -> g) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 24-25 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 16-17
(i#81 -> i) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 37-38 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 8-9
(j#82 -> j) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 47-48 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-5
(k#84 -> k) File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 20-21
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 30, character 0 to line 37, character 3
(Crypto -> Crypto) File "", line 21, character 0 to line 29, character 3
(List -> List) File "", line 38, character 0 to line 49, character 3
(Map -> Map) File "", line 6, character 0 to line 8, character 3
(Option -> Option) File "", line 2, character 0 to line 5, character 3
(Set -> Set) File "", line 9, character 0 to line 12, character 3
(String -> String) File "", line 13, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes Crypto List Map Option Set String a#80 c#81 d#84 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11
[ Bytes Crypto List Map Option Set String a#80 c#81 e#82 f#83 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
[ Bytes Crypto List Map Option Set String a#80 c#81 e#82 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
[ Bytes Crypto List Map Option Set String a#80 c#81 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
[ Bytes Crypto List Map Option Set String a#80 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
[ Bytes Crypto List Map Option Set String ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
[ a#75 generated#78 head_opt#74 length#66 size#69 xs#77 ] File "", line 48, characters 23-25
[ a#75 head_opt#74 length#66 size#69 xs#76 ] File "", line 46, characters 11-13
[ a#70 generated#72 length#66 size#69 x#73 xs#71 ] File "", line 44, characters 22-23
[ a#70 length#66 size#69 xs#71 ] File "", line 42, characters 11-13
[ a#67 length#66 xs#68 ] File "", line 40, characters 53-62
[ a#64 xs#65 ] File "", line 39, characters 55-103

Variable definitions:
(a#80 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
(b#85 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
references: []
(c#81 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
(d#84 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
(e#82 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
(f#83 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
references:
  File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 30, character 0 to line 37, character 3
(Crypto -> Crypto) File "", line 21, character 0 to line 29, character 3
(List -> List) File "", line 38, character 0 to line 49, character 3
(Map -> Map) File "", line 6, character 0 to line 8, character 3
(Option -> Option) File "", line 2, character 0 to line 5, character 3
(Set -> Set) File "", line 9, character 0 to line 12, character 3
(String -> String) File "", line 13, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
Scopes:
[ Bytes Crypto List Map Option Set String a#80 f#83 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-7
[ Bytes Crypto List Map Option Set String a#80 i#81 j#82 ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-63
[ Bytes Crypto List Map Option Set String ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 0-9
[ a#75 generated#78 head_opt#74 length#66 size#69 xs#77 ] File "", line 48, characters 23-25
[ a#75 head_opt#74 length#66 size#69 xs#76 ] File "", line 46, characters 11-13
[ a#70 generated#72 length#66 size#69 x#73 xs#71 ] File "", line 44, characters 22-23
[ a#70 length#66 size#69 xs#71 ] File "", line 42, characters 11-13
[ a#67 length#66 xs#68 ] File "", line 40, characters 53-62
[ a#64 xs#65 ] File "", line 39, characters 55-103

Variable definitions:
(a#80 -> a) File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 4-5 |resolved: int|
references: []
(b#84 -> b) File "../../test/contracts/get_scope_tests/lambda.mligo", line 3, characters 4-5 |resolved: int|
references: []
(f#83 -> f) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 6-7 |core: int -> int -> int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-3
(i#81 -> i) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 36-37 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 62-63
(j#82 -> j) File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 46-47 |core: int|
references:
  File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-59
Type definitions:
Module definitions:
(Bytes -> Bytes) File "", line 30, character 0 to line 37, character 3
(Crypto -> Crypto) File "", line 21, character 0 to line 29, character 3
(List -> List) File "", line 38, character 0 to line 49, character 3
(Map -> Map) File "", line 6, character 0 to line 8, character 3
(Option -> Option) File "", line 2, character 0 to line 5, character 3
(Set -> Set) File "", line 9, character 0 to line 12, character 3
(String -> String) File "", line 13, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "match.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Option Set String a#81 b#85 c#89 mytype#80 s#91 ] File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-21
    [ Bytes Crypto List Map Option Set String a#81 b#85 c#89 d#90 mytype#80 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-31
    [ Bytes Crypto List Map Option Set String a#81 b#85 c#89 mytype#80 ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, character 9 to line 20, character 13
    [ Bytes Crypto List Map Option Set String a#81 b#85 hd#88 mytype#80 tl#87 ] File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    [ Bytes Crypto List Map Option Set String a#81 b#85 c#86 mytype#80 ] File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5
    [ Bytes Crypto List Map Option Set String a#81 b#85 mytype#80 ] File "../../test/contracts/get_scope_tests/match.mligo", line 11, character 11 to line 14, character 5
    [ Bytes Crypto List Map Option Set String a#81 mytype#80 y#84 ] File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-18
    [ Bytes Crypto List Map Option Set String a#81 mytype#80 x#83 ] File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-18
    [ Bytes Crypto List Map Option Set String a#81 c#82 mytype#80 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    [ Bytes Crypto List Map Option Set String a#81 mytype#80 ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 9-27
    [ Bytes Crypto List Map Option Set String mytype#80 ] File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 0-9
    [ a#75 generated#78 head_opt#74 length#66 size#69 xs#77 ] File "", line 48, characters 23-25
    [ a#75 head_opt#74 length#66 size#69 xs#76 ] File "", line 46, characters 11-13
    [ a#70 generated#72 length#66 size#69 x#73 xs#71 ] File "", line 44, characters 22-23
    [ a#70 length#66 size#69 xs#71 ] File "", line 42, characters 11-13
    [ a#67 length#66 xs#68 ] File "", line 40, characters 53-62
    [ a#64 xs#65 ] File "", line 39, characters 55-103

    Variable definitions:
    (a#81 -> a) File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-29 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13
    (b#85 -> b) File "../../test/contracts/get_scope_tests/match.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (c#82 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    (c#86 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 8-9 |resolved: int|
    references: []
    (c#89 -> c) File "../../test/contracts/get_scope_tests/match.mligo", line 10, characters 4-5 |resolved: int|
    references: []
    (d#90 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 13-14 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 30-31
    (d#92 -> d) File "../../test/contracts/get_scope_tests/match.mligo", line 17, characters 4-5 |resolved: int|
    references: []
    (hd#88 -> hd) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6 |resolved: int|
    references: []
    (s#91 -> s) File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 10-11 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-17
    (tl#87 -> tl) File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 8-10 |resolved: list (int)|
    references: []
    (x#83 -> x) File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-14
    (y#84 -> y) File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 8-9 |resolved: string|
    references: []
    Type definitions:
    (mytype#80 -> mytype) File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 0-40 :
    sum[Bar -> string , Foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 30, character 0 to line 37, character 3
    (Crypto -> Crypto) File "", line 21, character 0 to line 29, character 3
    (List -> List) File "", line 38, character 0 to line 49, character 3
    (Map -> Map) File "", line 6, character 0 to line 8, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 9, character 0 to line 12, character 3
    (String -> String) File "", line 13, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "rec.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Option Set String a#80 b#86 c#85 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-9
    [ Bytes Crypto List Map Option Set String a#80 c#85 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 8, character 2 to line 9, character 10
    [ Bytes Crypto List Map Option Set String a#80 c#81 i#82 j#83 k#84 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-10
    [ Bytes Crypto List Map Option Set String a#80 c#81 i#82 j#83 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-21
    [ Bytes Crypto List Map Option Set String a#80 c#81 ] File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-40
    [ Bytes Crypto List Map Option Set String ] File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 0-9
    [ a#75 generated#78 head_opt#74 length#66 size#69 xs#77 ] File "", line 48, characters 23-25
    [ a#75 head_opt#74 length#66 size#69 xs#76 ] File "", line 46, characters 11-13
    [ a#70 generated#72 length#66 size#69 x#73 xs#71 ] File "", line 44, characters 22-23
    [ a#70 length#66 size#69 xs#71 ] File "", line 42, characters 11-13
    [ a#67 length#66 xs#68 ] File "", line 40, characters 53-62
    [ a#64 xs#65 ] File "", line 39, characters 55-103

    Variable definitions:
    (a#80 -> a) File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 5-6
    (b#86 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 8-9
    (b#87 -> b) File "../../test/contracts/get_scope_tests/rec.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#81 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-5
    (c#85 -> c) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11 |core:
    ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-3
    (i#82 -> i) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-38 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-13
    (j#83 -> j) File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 39-40 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 16-17
    (k#84 -> k) File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 7-8
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 30, character 0 to line 37, character 3
    (Crypto -> Crypto) File "", line 21, character 0 to line 29, character 3
    (List -> List) File "", line 38, character 0 to line 49, character 3
    (Map -> Map) File "", line 6, character 0 to line 8, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 9, character 0 to line 12, character 3
    (String -> String) File "", line 13, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "shadowing.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Option Set String a#80 c#81 d#84 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-11
    [ Bytes Crypto List Map Option Set String a#83 c#81 e#82 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-13
    [ Bytes Crypto List Map Option Set String a#80 c#81 e#82 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-21
    [ Bytes Crypto List Map Option Set String a#80 c#81 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-17
    [ Bytes Crypto List Map Option Set String a#80 ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-15
    [ Bytes Crypto List Map Option Set String ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 0-9
    [ a#75 generated#78 head_opt#74 length#66 size#69 xs#77 ] File "", line 48, characters 23-25
    [ a#75 head_opt#74 length#66 size#69 xs#76 ] File "", line 46, characters 11-13
    [ a#70 generated#72 length#66 size#69 x#73 xs#71 ] File "", line 44, characters 22-23
    [ a#70 length#66 size#69 xs#71 ] File "", line 42, characters 11-13
    [ a#67 length#66 xs#68 ] File "", line 40, characters 53-62
    [ a#64 xs#65 ] File "", line 39, characters 55-103

    Variable definitions:
    (a#80 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-3
    (a#83 -> a) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-5
    (b#85 -> b) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#81 -> c) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 6-7
    (d#84 -> d) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 10-11
    (e#82 -> e) File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 12-13
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 30, character 0 to line 37, character 3
    (Crypto -> Crypto) File "", line 21, character 0 to line 29, character 3
    (List -> List) File "", line 38, character 0 to line 49, character 3
    (Map -> Map) File "", line 6, character 0 to line 8, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 9, character 0 to line 12, character 3
    (String -> String) File "", line 13, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "records.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Option Set String a#81 b#84 g#85 myrec#80 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-41
    [ Bytes Crypto List Map Option Set String a#81 b#84 myrec#80 ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-41
    [ Bytes Crypto List Map Option Set String a#81 i#82 j#83 myrec#80 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-45
    [ Bytes Crypto List Map Option Set String a#81 i#82 myrec#80 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 27-45
    [ Bytes Crypto List Map Option Set String a#81 myrec#80 ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 14-55
    [ Bytes Crypto List Map Option Set String myrec#80 ] File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 0-9
    [ a#75 generated#78 head_opt#74 length#66 size#69 xs#77 ] File "", line 48, characters 23-25
    [ a#75 head_opt#74 length#66 size#69 xs#76 ] File "", line 46, characters 11-13
    [ a#70 generated#72 length#66 size#69 x#73 xs#71 ] File "", line 44, characters 22-23
    [ a#70 length#66 size#69 xs#71 ] File "", line 42, characters 11-13
    [ a#67 length#66 xs#68 ] File "", line 40, characters 53-62
    [ a#64 xs#65 ] File "", line 39, characters 55-103

    Variable definitions:
    (a#81 -> a) File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-41 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 40-41
    (b#84 -> b) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 4-5 |resolved: myrec|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 32-33
    (e#86 -> e) File "../../test/contracts/get_scope_tests/records.mligo", line 15, characters 4-5 |resolved: myrec|
    references: []
    (g#85 -> g) File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 19-20 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-29
    (i#82 -> i) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 18-19 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 42-43
    (j#83 -> j) File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 31-32 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 44-45
    Type definitions:
    (myrec#80 -> myrec) File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 0-36 :
    record[bar -> int , foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 30, character 0 to line 37, character 3
    (Crypto -> Crypto) File "", line 21, character 0 to line 29, character 3
    (List -> List) File "", line 38, character 0 to line 49, character 3
    (Map -> Map) File "", line 6, character 0 to line 8, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 9, character 0 to line 12, character 3
    (String -> String) File "", line 13, character 0 to line 20, character 3 |} ] ;

  run_ligo_good [ "info" ; "get-scope" ; gs "constant.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Option Set String a#80 e#83 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-30
    [ Bytes Crypto List Map Option Set String a#80 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 5-32
    [ Bytes Crypto List Map Option Set String a#80 c#81 d#82 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ Bytes Crypto List Map Option Set String a#80 c#81 ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    [ Bytes Crypto List Map Option Set String ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 0-9
    [ a#75 generated#78 head_opt#74 length#66 size#69 xs#77 ] File "", line 48, characters 23-25
    [ a#75 head_opt#74 length#66 size#69 xs#76 ] File "", line 46, characters 11-13
    [ a#70 generated#72 length#66 size#69 x#73 xs#71 ] File "", line 44, characters 22-23
    [ a#70 length#66 size#69 xs#71 ] File "", line 42, characters 11-13
    [ a#67 length#66 xs#68 ] File "", line 40, characters 53-62
    [ a#64 xs#65 ] File "", line 39, characters 55-103

    Variable definitions:
    (a#80 -> a) File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (b#84 -> b) File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5 |resolved: list (int)|
    references: []
    (c#81 -> c) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (d#82 -> d) File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#83 -> e) File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 30, character 0 to line 37, character 3
    (Crypto -> Crypto) File "", line 21, character 0 to line 29, character 3
    (List -> List) File "", line 38, character 0 to line 49, character 3
    (Map -> Map) File "", line 6, character 0 to line 8, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 9, character 0 to line 12, character 3
    (String -> String) File "", line 13, character 0 to line 20, character 3 |} ] 

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "application.mligo" ; "--format";"dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Option Set String c#84 f#82 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    [ Bytes Crypto List Map Option Set String b#83 f#82 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-19
    [ Bytes Crypto List Map Option Set String f#82 ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 3-36
    [ Bytes Crypto List Map Option Set String i#80 j#81 ] File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-63
    [ a#75 generated#78 head_opt#74 length#66 size#69 xs#77 ] File "", line 48, characters 23-25
    [ a#75 head_opt#74 length#66 size#69 xs#76 ] File "", line 46, characters 11-13
    [ a#70 generated#72 length#66 size#69 x#73 xs#71 ] File "", line 44, characters 22-23
    [ a#70 length#66 size#69 xs#71 ] File "", line 42, characters 11-13
    [ a#67 length#66 xs#68 ] File "", line 40, characters 53-62
    [ a#64 xs#65 ] File "", line 39, characters 55-103

    Variable definitions:
    (a#85 -> a) File "../../test/contracts/get_scope_tests/application.mligo", line 1, characters 4-5 |resolved: int|
    references: []
    (b#83 -> b) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 7-8 |resolved: int|
    references: []
    (c#84 -> c) File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 26-27 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    (f#82 -> f) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 6-7 |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-17
    (i#80 -> i) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 36-37 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 62-63
    (j#81 -> j) File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 46-47 |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-59
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 30, character 0 to line 37, character 3
    (Crypto -> Crypto) File "", line 21, character 0 to line 29, character 3
    (List -> List) File "", line 38, character 0 to line 49, character 3
    (Map -> Map) File "", line 6, character 0 to line 8, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 9, character 0 to line 12, character 3
    (String -> String) File "", line 13, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "include.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Option Set String a#80 b#85 x#86 ] File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-13
    [ Bytes Crypto List Map Option Set String a#80 b#85 ] File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 0-9
    [ Bytes Crypto List Map Option Set String a#80 c#81 d#84 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11
    [ Bytes Crypto List Map Option Set String a#80 c#81 e#82 f#83 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
    [ Bytes Crypto List Map Option Set String a#80 c#81 e#82 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    [ Bytes Crypto List Map Option Set String a#80 c#81 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    [ Bytes Crypto List Map Option Set String a#80 ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    [ Bytes Crypto List Map Option Set String ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
    [ a#75 generated#78 head_opt#74 length#66 size#69 xs#77 ] File "", line 48, characters 23-25
    [ a#75 head_opt#74 length#66 size#69 xs#76 ] File "", line 46, characters 11-13
    [ a#70 generated#72 length#66 size#69 x#73 xs#71 ] File "", line 44, characters 22-23
    [ a#70 length#66 size#69 xs#71 ] File "", line 42, characters 11-13
    [ a#67 length#66 xs#68 ] File "", line 40, characters 53-62
    [ a#64 xs#65 ] File "", line 39, characters 55-103

    Variable definitions:
    (a#80 -> a) File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    (b#85 -> b) File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5 |resolved: int|
    references: []
    (c#81 -> c) File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    (d#84 -> d) File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    (e#82 -> e) File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    (f#83 -> f) File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    (x#86 -> x) File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 4-5 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-9
    (y#87 -> y) File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (Bytes -> Bytes) File "", line 30, character 0 to line 37, character 3
    (Crypto -> Crypto) File "", line 21, character 0 to line 29, character 3
    (List -> List) File "", line 38, character 0 to line 49, character 3
    (Map -> Map) File "", line 6, character 0 to line 8, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 9, character 0 to line 12, character 3
    (String -> String) File "", line 13, character 0 to line 20, character 3 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "bad_field_record.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Option Set String a#83 c#81 foo_record#80 j#84 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    [ Bytes Crypto List Map Option Set String a#83 c#81 foo_record#80 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    [ Bytes Crypto List Map Option Set String c#81 foo_record#80 i#82 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    [ Bytes Crypto List Map Option Set String c#81 foo_record#80 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11
    [ Bytes Crypto List Map Option Set String foo_record#80 ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 4, character 8 to line 5, character 9
    [ a#75 generated#78 head_opt#74 length#66 size#69 xs#77 ] File "", line 48, characters 23-25
    [ a#75 head_opt#74 length#66 size#69 xs#76 ] File "", line 46, characters 11-13
    [ a#70 generated#72 length#66 size#69 x#73 xs#71 ] File "", line 44, characters 22-23
    [ a#70 length#66 size#69 xs#71 ] File "", line 42, characters 11-13
    [ a#67 length#66 xs#68 ] File "", line 40, characters 53-62
    [ a#64 xs#65 ] File "", line 39, characters 55-103

    Variable definitions:
    (a#83 -> a) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 8, characters 4-5 |resolved: int|
    references: []
    (b#85 -> b) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 12, characters 4-5 |unresolved|
    references: []
    (c#81 -> c) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 3, characters 4-5 |resolved: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11 ,
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    (i#82 -> i) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 6-7 |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    (j#84 -> j) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 6-7 |unresolved|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    Type definitions:
    (foo_record#80 -> foo_record) File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 1, characters 0-43 :
    record[bar -> int , foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 30, character 0 to line 37, character 3
    (Crypto -> Crypto) File "", line 21, character 0 to line 29, character 3
    (List -> List) File "", line 38, character 0 to line 49, character 3
    (Map -> Map) File "", line 6, character 0 to line 8, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 9, character 0 to line 12, character 3
    (String -> String) File "", line 13, character 0 to line 20, character 3 |} ];

  run_ligo_good [ "info"; "get-scope" ; gs "nominal_types.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ Bytes Crypto List Map Option Set String a#82 b#83 c#84 foo_record#81 foo_variant#80 p#85 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    [ Bytes Crypto List Map Option Set String a#82 b#83 foo_record#81 foo_variant#80 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, character 8 to line 10, character 9
    [ Bytes Crypto List Map Option Set String a#82 foo_record#81 foo_variant#80 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 12-14
    [ Bytes Crypto List Map Option Set String foo_record#81 foo_variant#80 ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 12-13
    [ a#75 generated#78 head_opt#74 length#66 size#69 xs#77 ] File "", line 48, characters 23-25
    [ a#75 head_opt#74 length#66 size#69 xs#76 ] File "", line 46, characters 11-13
    [ a#70 generated#72 length#66 size#69 x#73 xs#71 ] File "", line 44, characters 22-23
    [ a#70 length#66 size#69 xs#71 ] File "", line 42, characters 11-13
    [ a#67 length#66 xs#68 ] File "", line 40, characters 53-62
    [ a#64 xs#65 ] File "", line 39, characters 55-103

    Variable definitions:
    (a#82 -> a) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    (b#83 -> b) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 4-5 |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    (c#84 -> c) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 8, characters 4-5 |resolved: foo_record|
    references: []
    (main#86 -> main) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 4-8 |core: foo_record -> foo_variant|
    references: []
    (p#85 -> p) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 10-11 |core: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    Type definitions:
    (foo_record#81 -> foo_record) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 0-58 :
    record[bar -> foo_variant , foo -> foo_variant]
    (foo_variant#80 -> foo_variant) File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 0-45 :
    sum[Bar -> string , Foo -> int]
    Module definitions:
    (Bytes -> Bytes) File "", line 30, character 0 to line 37, character 3
    (Crypto -> Crypto) File "", line 21, character 0 to line 29, character 3
    (List -> List) File "", line 38, character 0 to line 49, character 3
    (Map -> Map) File "", line 6, character 0 to line 8, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 9, character 0 to line 12, character 3
    (String -> String) File "", line 13, character 0 to line 20, character 3 |} ] ;
    
  run_ligo_good [ "info"; "get-scope" ; gs "module.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ A B Bytes C Crypto D List Map Option Set String a#81 b#82 ] File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 4-7
    [ ] File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 8-17

    Variable definitions:
    (a#81 -> a) File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 4-5 |resolved: int|
    references: []
    (b#82 -> b) File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 4-5 |resolved: int|
    references: []
    (titi#84 -> titi) File "../../test/contracts/get_scope_tests/module.mligo", line 11, characters 4-8 |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A -> A) File "../../test/contracts/get_scope_tests/module.mligo", line 1, character 0 to line 3, character 3
    (B -> B) File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 0-12
    (Bytes -> Bytes) File "", line 30, character 0 to line 37, character 3
    (C -> C) File "../../test/contracts/get_scope_tests/module.mligo", line 12, character 4 to line 16, character 7
    (Crypto -> Crypto) File "", line 21, character 0 to line 29, character 3
    (D -> D) File "../../test/contracts/get_scope_tests/module.mligo", line 15, character 4 to line 16, character 7
    (List -> List) File "", line 38, character 0 to line 49, character 3
    (Map -> Map) File "", line 6, character 0 to line 8, character 3
    (Option -> Option) File "", line 2, character 0 to line 5, character 3
    (Set -> Set) File "", line 9, character 0 to line 12, character 3
    (String -> String) File "", line 13, character 0 to line 20, character 3 |} ]