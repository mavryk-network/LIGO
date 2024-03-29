let%expect_test "error_recovery_simple_cameligo" =
  printf "%s" @@ Error_recovery.test_cameligo ();
  [%expect
    {|
    STATUS, LOC, CST, SYMBOLS, TOKENS, ERRORS, FILE
    PASS, 5, 39, 25, 94, 3, ./case_kw_instead_of_match_kw.mligo
    PASS, 16, 70, 70, 177, 0, ./extra_arrow_in_case_expr.mligo
    PASS, 2, 25, 3, 5, 0, ./extra_colon_in_return_type.mligo
    PASS, 0, 0, 0, 0, 0, ./extra_eq_in_func_decl.mligo
    PASS, 0, 0, 0, 0, 0, ./extra_in_kw.mligo
    PASS, 0, 0, 0, 0, 0, ./extra_then_kw.mligo
    PASS, 1, 5, 5, 84, 0, ./extra_vertical_bar.mligo
    PASS, 7, 93, 53, 105, 8, ./fun_kw_instead_of_let_kw.mligo
    PASS, 2, 18, 12, 13, 1, ./missing_argument_bracketR.mligo
    PASS, 2, 25, 7, 10, 1, ./missing_arrow_in_lambda_expr.mligo
    PASS, 0, 0, 0, 0, 0, ./missing_arrow_in_match_expr.mligo
    PASS, 2, 16, 6, 5, 1, ./missing_colon_in_list_pat.mligo
    PASS, 2, 29, 9, 8, 1, ./missing_comma_in_arguments.mligo
    PASS, 0, 0, 0, 0, 0, ./missing_curly_bracket_in_record_decl.mligo
    PASS, 10, 238, 0, 100, 0, ./missing_end_kw_in_the_nested_module.mligo
    PASS, 6, 29, 13, 168, 2, ./missing_eq_in_record_expr.mligo
    PASS, 0, 0, 0, 0, 0, ./missing_eq_in_type_decl.mligo
    PASS, 2, 39, 5, 14, 0, ./missing_expr_parenthesesL.mligo
    PASS, 2, 22, 2, 6, 0, ./missing_expr_parenthesesR.mligo
    PASS, 2, 2, 2, 6, 0, ./missing_ident_in_type_decl.mligo
    PASS, 0, 0, 0, 0, 0, ./missing_in_kw.mligo
    PASS, 2, 5, 5, 10, 0, ./missing_int.mligo
    PASS, 12, 93, 3, 179, 1, ./missing_module_kw_in_module_decl.mligo
    PASS, 2, 2, 2, 6, 0, ./missing_module_name.mligo
    PASS, 2, 2, 2, 18, 0, ./missing_name_of_argument.mligo
    PASS, 3, 17, 11, 194, 0, ./missing_semicolon_in_record_decl.mligo
    PASS, 14, 95, 3, 77, 1, ./missing_struct_kw_in_module_decl.mligo
    PASS, 10, 158, 16, 93, 2, ./missing_then_kw.mligo
    PASS, 3, 31, 13, 90, 0, ./missing_vertical_bar.mligo
    PASS, 0, 0, 0, 0, 0, ./missing_with_kw_in_match_expr.mligo
    PASS, 2, 30, 12, 21, 0, ./typo_in_fun_kw.mligo
    PASS, 5, 39, 25, 94, 3, ./typo_in_match_kw.mligo
    PASS, 2, 27, 17, 10, 1, ./typo_in_with_kw_in_record_update.mligo
    PASS, 2, 435, 7, 343, 0, ./unfinished_code00.mligo
    PASS, 2, 2, 2, 2, 0, ./unfinished_code01.mligo
    PASS, 3, 245, 17, 516, 1, ./unfinished_code02.mligo
    PASS, 3, 9, 9, 625, 0, ./unfinished_code03.mligo
    PASS, 3, 32, 30, 190, 0, ./unfinished_code04.mligo
    PASS, 3, 14, 12, 149, 0, ./unfinished_code05.mligo
    PASS, 3, 8, 6, 323, 0, ./unfinished_code06.mligo
    PASS, 5, 2, 2, 34, 0, ./unfinished_code07.mligo
    PASS, 5, 7, 5, 514, 0, ./unfinished_code08.mligo
    PASS, 3, 18, 6, 95, 0, ./unfinished_code09.mligo
    PASS, 6, 26, 22, 274, 0, ./unfinished_code10.mligo
    PASS, 7, 3, 3, 118, 0, ./unfinished_code11.mligo
    PASS, 3, 7, 5, 184, 0, ./unfinished_code12.mligo
    PASS, 47, 288, 28, 186, 1, ./unfinished_code13.mligo
    FAIL : can't recover test file./unreadable_symbol.mligo |}]
