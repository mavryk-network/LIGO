let%expect_test "error_recovery_fuzzing_jsligo" =
  printf "%s" @@ Error_recovery.test_jsligo ();
  [%expect
    {|
    STATUS, LOC, CST, SYMBOLS, TOKENS, ERRORS, FILE
    PASS, 101, 510, 250, 425, 36, ./16tuples_sequences_functions.jsligo
    PASS, 38, 553, 55, 820, 4, ./1high-order.jsligo
    PASS, 36, 175, 19, 139, 4, ./1never.jsligo
    PASS, 16, 89, 7, 38, 1, ./2amount.jsligo
    PASS, 30, 226, 34, 208, 11, ./2bytes_unpack.jsligo
    PASS, 10, 92, 10, 58, 1, ./2counter.jsligo
    PASS, 12, 28, 6, 41, 0, ./2if_if_return.jsligo
    PASS, 17, 137, 23, 85, 4, ./2key_hash.jsligo
    PASS, 11, 4, 4, 30, 1, ./2local_type_decl.jsligo
    PASS, 38, 229, 63, 214, 13, ./2match_bis.jsligo
    PASS, 15, 6, 6, 140, 4, ./2super-counter.jsligo
    PASS, 22, 136, 42, 186, 6, ./4balance_constant.jsligo
    PASS, 12, 37, 19, 86, 1, ./4check_signature.jsligo
    PASS, 10, 2, 2, 13, 0, ./4if_if_return.jsligo
    PASS, 14, 132, 16, 118, 2, ./4lambda2.jsligo
    PASS, 24, 162, 54, 134, 14, ./4match.jsligo
    PASS, 40, 171, 51, 128, 12, ./4never.jsligo
    PASS, 35, 251, 47, 190, 12, ./4recursion.jsligo
    PASS, 20, 155, 55, 143, 9, ./8assert.jsligo
    PASS, 11, 90, 12, 87, 1, ./8eq_bool.jsligo
    PASS, 16, 183, 9, 222, 0, ./8recursion.jsligo |}]
