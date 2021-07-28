open Trace
open Test_helpers
open Main_errors

open Ast_imperative.Combinators

let init_env = Environment.default Environment.Protocols.current

let type_file_w f =
  type_file_w f Env options

let type_alias ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/type-alias.ligo" in
  expect_eq_evaluate ~raise program "foo" (e_int 23)

let function_ ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/function.ligo" in
  let make_expect = fun n -> n in
  expect_eq_n_int ~raise program "main" make_expect

let blockless ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/blockless.ligo" in
  let make_expect = fun n-> n + 10 in
  expect_eq_n_int ~raise program "blockless" make_expect

(* Procedures are not supported yet
  let procedure () : unit result =
  let program = type_file "./contracts/procedure.ligo" in
  let make_expect = fun n -> n + 1 in
  expect_eq_n_int program "main" make_expect *)

let assign ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/assign.ligo" in
  let make_expect = fun n -> n + 1 in
  expect_eq_n_int ~raise program "main" make_expect

let annotation ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/annotation.ligo" in
  let () =
    expect_eq_evaluate ~raise program "lst" (e_list [])
  in
  let () =
    expect_eq_evaluate ~raise program "my_address" (e_address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
  in
  ()

let complex_function ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/function-complex.ligo" in
  let make_expect = fun n -> (3 * n + 2) in
  expect_eq_n_int ~raise program "main" make_expect

let anon_function ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/function-anon.ligo" in
  let () =
    expect_eq_evaluate ~raise program "x" (e_int 42)
  in
  ()

let application ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/application.ligo" in
  let () =
    let expected = e_int 42 in
    expect_eq_evaluate ~raise program "x" expected in
  let () =
    let expected = e_int 42 in
    expect_eq_evaluate ~raise program "y" expected in
  let () =
    let expected = e_int 42 in
    expect_eq_evaluate ~raise program "z" expected in
  ()

let variant ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/variant.ligo" in
  let () =
    let expected = e_constructor "Foo" (e_int 42) in
    expect_eq_evaluate ~raise program "foo" expected in
  let () =
    let expected = e_constructor "Bar" (e_bool true) in
    expect_eq_evaluate ~raise program "bar" expected in
  let () =
    let expected = e_constructor "Kee" (e_nat 23) in
    expect_eq_evaluate ~raise program "kee" expected in
  ()

let variant_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/variant.mligo" in
  let () =
    let expected = e_constructor "Foo" (e_int 42) in
    expect_eq_evaluate ~raise program "foo" expected in
  let () =
    let expected = e_constructor "Bar" (e_bool true) in
    expect_eq_evaluate ~raise program "bar" expected in
  let () =
    let expected = e_constructor "Kee" (e_nat 23) in
    expect_eq_evaluate ~raise program "kee" expected in
  ()

let variant_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/variant.religo" in
  let () =
    let expected = e_constructor "Foo" (e_int 42) in
    expect_eq_evaluate ~raise program "foo" expected in
  let () =
    let expected = e_constructor "Bar" (e_bool true) in
    expect_eq_evaluate ~raise program "bar" expected in
  let () =
    let expected = e_constructor "Kee" (e_nat 23) in
    expect_eq_evaluate ~raise program "kee" expected in
  ()

let variant_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/variant.jsligo" in
  let () =
    let expected = e_constructor "Foo" (e_int 42) in
    expect_eq_evaluate ~raise program "foo" expected in
  let () =
    let expected = e_constructor "Bar" (e_bool true) in
    expect_eq_evaluate ~raise program "bar" expected in
  let () =
    let expected = e_constructor "Kee" (e_nat 23) in
    expect_eq_evaluate ~raise program "kee" expected in
  ()
  
let variant_matching ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/variant-matching.ligo" in
  let () =
    let make_input = fun n -> e_constructor "Foo" (e_int n) in
    let make_expected = e_int in
    expect_eq ~raise program "fb" (make_input 0) (make_expected 0);
    expect_eq_n ~raise program "fb" make_input make_expected;
    expect_eq ~raise program "fb" (e_constructor "Kee" (e_nat 50)) (e_int 23);
    expect_eq ~raise program "fb" (e_constructor "Bar" (e_bool true)) (e_int 42);
    ()
  in
  ()

let closure ~add_warning ~raise () : unit =
  let program   = type_file_w ~raise "./contracts/closure.ligo" in
  let program_1 = type_file_w ~raise "./contracts/closure-1.ligo" in
  let program_2 = type_file_w ~raise "./contracts/closure-2.ligo" in
  let program_3 = type_file_w ~raise "./contracts/closure-3.ligo" in
  let _ =
    let make_expect = fun n -> (49 + n) in
    expect_eq_n_int ~raise program_3 "foobar" make_expect
  in
  let _ =
    let make_expect = fun n -> (45 + n) in
    expect_eq_n_int ~raise program_2 "foobar" make_expect
  in
  let () =
    let make_expect = fun n -> (2 * n) in
    expect_eq_n_int ~raise program_1 "foo" make_expect
  in
  let _ =
    let make_expect = fun n -> (4 * n) in
    expect_eq_n_int ~raise program "toto" make_expect
  in
  ()

let closure_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/closure.mligo" in
  let _ =
    let input = e_int 0 in
    let expected = e_int 25 in
    expect_eq ~raise program "test" input expected
  in
  ()

let closure_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/closure.religo" in
  let _ =
    let input = e_int 0 in
    let expected = e_int 25 in
    expect_eq ~raise program "test" input expected
  in
  ()

let closure_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/closure.jsligo" in
  let _ =
    let input = e_int 0 in
    let expected = e_int 25 in
    expect_eq ~raise program "test" input expected
  in
  ()
  

let shadow ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/shadow.ligo" in
  let make_expect = fun _ -> 0 in
  expect_eq_n_int ~raise program "foo" make_expect

let shadowing ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/shadowing.mligo" in
  let _ =
    let input = e_constructor "A" (e_int 1) in
    let expected = e_list [e_constructor "A" (e_int 1)] in
    expect_eq ~raise program "main" input expected
  in
  ()

let higher_order ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/high-order.ligo" in
  let make_expect = fun n -> n in
  let _ = expect_eq_n_int ~raise program "foobar" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar2" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar3" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar4" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar5" make_expect in
  (* let _ = applies_expect_eq_n_int program "foobar5" make_expect in *)
  ()

let higher_order_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/high-order.mligo" in
  let make_expect = fun n -> n in
  let _ = expect_eq_n_int ~raise program "foobar" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar2" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar3" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar4" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar5" make_expect in
  ()

let higher_order_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/high-order.religo" in
  let make_expect = fun n -> n in
  let _ = expect_eq_n_int ~raise program "foobar" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar2" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar3" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar4" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar5" make_expect in
  ()

let higher_order_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/high-order.jsligo" in
  let make_expect = fun n -> n in
  let _ = expect_eq_n_int ~raise program "foobar" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar2" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar3" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar4" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar5" make_expect in
  ()
  

let shared_function ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/function-shared.ligo" in
  let () =
    let make_expect = fun n -> (n + 1) in
    expect_eq_n_int ~raise program "inc" make_expect
  in
  let () =
    expect_eq ~raise program "double_inc" (e_int 0) (e_int 2)
  in
  let () =
    let make_expect = fun n -> (n + 2) in
    expect_eq_n_int ~raise program "double_inc" make_expect
  in
  let () =
    let make_expect = fun n -> (2 * n + 3) in
    expect_eq ~raise program "foo" (e_int 0) (e_int @@ make_expect 0)
  in
  let () =
    let make_expect = fun n -> (2 * n + 3) in
    expect_eq_n_int ~raise program "foo" make_expect
  in
  ()

let shared_function_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/function-shared.mligo" in
  let () =
    let make_expect = fun n -> (2 * n + 70) in
    expect_eq_n_int ~raise program "foobar" make_expect
  in
  ()

let shared_function_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/function-shared.religo" in
  let () =
    let make_expect = fun n -> (2 * n + 70) in
    expect_eq_n_int ~raise program "foobar" make_expect
  in
  ()

let shared_function_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/function-shared.jsligo" in
  let () =
    let make_expect = fun n -> (2 * n + 70) in
    expect_eq_n_int ~raise program "foobar" make_expect
  in
  ()
  
let bool_expression ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/boolean_operators.ligo" in
  let _ =
    let aux (name , f) = expect_eq_b_bool program name f in
    List.map ~f:aux [
      ("or_true", fun b -> b || true) ;
      ("or_false", fun b -> b || false) ;
      ("and_true", fun b -> b && true) ;
      ("and_false", fun b -> b && false) ;
      ("not_bool", fun b -> not b) ;
    ] in
  ()

let bool_expression_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/boolean_operators.mligo" in
  let _ =
    let aux (name, f) = expect_eq_b_bool program name f in
    List.map ~f:aux [
      ("or_true", fun b -> b || true) ;
      ("or_false", fun b -> b || false) ;
      ("and_true", fun b -> b && true) ;
      ("and_false", fun b -> b && false) ;
      ("not_bool", fun b -> not b) ;
    ] in
  ()

let bool_expression_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/boolean_operators.religo" in
  let _ =
    let aux (name, f) = expect_eq_b_bool program name f in
    List.map ~f:aux [
      ("or_true", fun b -> b || true) ;
      ("or_false", fun b -> b || false) ;
      ("and_true", fun b -> b && true) ;
      ("and_false", fun b -> b && false) ;
      ("not_bool", fun b -> not b) ;
    ] in
  ()

let bool_expression_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/boolean_operators.jsligo" in
  let _ =
    let aux (name, f) = expect_eq_b_bool program name f in
    List.map ~f:aux [
      ("or_true", fun b -> b || true) ;
      ("or_false", fun b -> b || false) ;
      ("and_true", fun b -> b && true) ;
      ("and_false", fun b -> b && false) ;
      ("not_bool", fun b -> not b) ;
    ] in
  ()

let arithmetic ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/arithmetic.ligo" in
  let _ =
    let aux (name , f) = expect_eq_n_int ~raise program name f in
    List.map ~f:aux [
      ("plus_op", fun n -> (n + 42)) ;
      ("minus_op", fun n -> (n - 42)) ;
      ("times_op", fun n -> (n * 42)) ;
      ("neg_op", fun n -> (-n)) ;
    ] in
  let () = expect_eq_n_pos ~raise program "int_op" e_nat e_int in
  let () = expect_eq_n_pos ~raise program "mod_op" e_int (fun n -> e_nat (n mod 42)) in
  let () = expect_eq_n_pos ~raise program "div_op" e_int (fun n -> e_int (n / 2)) in
  let () = expect_eq_n_pos ~raise program "ediv_op" e_int (fun n -> e_some (e_pair (e_int (n/2)) (e_nat (n mod 2)))) in
  ()

let arithmetic_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/arithmetic.mligo" in
  let _ =
    let aux (name, f) = expect_eq_n_int ~raise program name f in
    List.map ~f:aux [
      ("plus_op", fun n -> (n + 42)) ;
      ("minus_op", fun n -> (n - 42)) ;
      ("times_op", fun n -> (n * 42)) ;
      ("neg_op", fun n -> (-n)) ;
      ("neg_op_2", fun n -> -(n + 10)) ;
    ] in
  let () = expect_eq_n_pos ~raise program "mod_op" e_int (fun n -> e_nat (n mod 42)) in
  let () = expect_eq_n_pos ~raise program "div_op" e_int (fun n -> e_int (n / 2)) in
  let () = expect_eq_n_pos ~raise program "ediv_op" e_int (fun n -> e_some (e_pair (e_int (n/2)) (e_nat (n mod 2)))) in
  let _ = expect_eq_evaluate ~raise program "mul_woo" (e_unit ()) in
  ()

let arithmetic_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/arithmetic.religo" in
  let _ =
    let aux (name, f) = expect_eq_n_int ~raise program name f in
    List.map ~f:aux [
      ("plus_op", fun n -> (n + 42)) ;
      ("minus_op", fun n -> (n - 42)) ;
      ("times_op", fun n -> (n * 42)) ;
      ("neg_op", fun n -> (-n)) ;
      ("neg_op_2", fun n -> -(n + 10)) ;
    ] in
  let () = expect_eq_n_pos ~raise program "mod_op" e_int (fun n -> e_nat (n mod 42)) in
  let () = expect_eq_n_pos ~raise program "div_op" e_int (fun n -> e_int (n / 2)) in
  let () = expect_eq_n_pos ~raise program "ediv_op" e_int (fun n -> e_some (e_pair (e_int (n/2)) (e_nat (n mod 2)))) in
  ()

let arithmetic_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/arithmetic.jsligo" in
  let _ =
    let aux (name, f) = expect_eq_n_int ~raise program name f in
    List.map ~f:aux [
      ("plus_op", fun n -> (n + 42)) ;
      ("minus_op", fun n -> (n - 42)) ;
      ("times_op", fun n -> (n * 42)) ;
      ("neg_op", fun n -> (-n)) ;
      ("neg_op_2", fun n -> -(n + 10)) ;
    ] in
  let () = expect_eq_n_pos ~raise program "mod_op" e_int (fun n -> e_nat (n mod 42)) in
  let () = expect_eq_n_pos ~raise program "div_op" e_int (fun n -> e_int (n / 2)) in
  let () = expect_eq_n_pos ~raise program "ediv_op" e_int (fun n -> e_some (e_pair (e_int (n/2)) (e_nat (n mod 2)))) in
  ()
  

let bitwise_arithmetic ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/bitwise_arithmetic.ligo" in
  let () = expect_eq ~raise program "or_op" (e_nat 7) (e_nat 7) in
  let () = expect_eq ~raise program "or_op" (e_nat 3) (e_nat 7) in
  let () = expect_eq ~raise program "or_op" (e_nat 2) (e_nat 6) in
  let () = expect_eq ~raise program "or_op" (e_nat 14) (e_nat 14) in
  let () = expect_eq ~raise program "or_op" (e_nat 10) (e_nat 14) in
  let () = expect_eq ~raise program "and_op" (e_nat 7) (e_nat 7) in
  let () = expect_eq ~raise program "and_op" (e_nat 3) (e_nat 3) in
  let () = expect_eq ~raise program "and_op" (e_nat 2) (e_nat 2) in
  let () = expect_eq ~raise program "and_op" (e_nat 14) (e_nat 6) in
  let () = expect_eq ~raise program "and_op" (e_nat 10) (e_nat 2) in
  let () = expect_eq ~raise program "xor_op" (e_nat 0) (e_nat 7) in
  let () = expect_eq ~raise program "xor_op" (e_nat 7) (e_nat 0) in
  let () = expect_eq ~raise program "lsl_op" (e_nat 1000) (e_nat 128000) in
  let () = expect_eq ~raise program "lsr_op" (e_nat 128000) (e_nat 1000) in
  ()

let bitwise_arithmetic_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/bitwise_arithmetic.mligo" in
  let () = expect_eq ~raise program "or_op" (e_nat 7) (e_nat 7) in
  let () = expect_eq ~raise program "or_op" (e_nat 3) (e_nat 7) in
  let () = expect_eq ~raise program "or_op" (e_nat 2) (e_nat 6) in
  let () = expect_eq ~raise program "or_op" (e_nat 14) (e_nat 14) in
  let () = expect_eq ~raise program "or_op" (e_nat 10) (e_nat 14) in
  let () = expect_eq ~raise program "and_op" (e_nat 7) (e_nat 7) in
  let () = expect_eq ~raise program "and_op" (e_nat 3) (e_nat 3) in
  let () = expect_eq ~raise program "and_op" (e_nat 2) (e_nat 2) in
  let () = expect_eq ~raise program "and_op" (e_nat 14) (e_nat 6) in
  let () = expect_eq ~raise program "and_op" (e_nat 10) (e_nat 2) in
  let () = expect_eq ~raise program "xor_op" (e_nat 0) (e_nat 7) in
  let () = expect_eq ~raise program "xor_op" (e_nat 7) (e_nat 0) in
  let () = expect_eq ~raise program "lsl_op" (e_nat 1000) (e_nat 128000) in
  let () = expect_eq ~raise program "lsr_op" (e_nat 128000) (e_nat 1000) in
  ()

let bitwise_arithmetic_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/bitwise_arithmetic.religo" in
  let () = expect_eq ~raise program "or_op" (e_nat 7) (e_nat 7) in
  let () = expect_eq ~raise program "or_op" (e_nat 3) (e_nat 7) in
  let () = expect_eq ~raise program "or_op" (e_nat 2) (e_nat 6) in
  let () = expect_eq ~raise program "or_op" (e_nat 14) (e_nat 14) in
  let () = expect_eq ~raise program "or_op" (e_nat 10) (e_nat 14) in
  let () = expect_eq ~raise program "and_op" (e_nat 7) (e_nat 7) in
  let () = expect_eq ~raise program "and_op" (e_nat 3) (e_nat 3) in
  let () = expect_eq ~raise program "and_op" (e_nat 2) (e_nat 2) in
  let () = expect_eq ~raise program "and_op" (e_nat 14) (e_nat 6) in
  let () = expect_eq ~raise program "and_op" (e_nat 10) (e_nat 2) in
  let () = expect_eq ~raise program "xor_op" (e_nat 0) (e_nat 7) in
  let () = expect_eq ~raise program "xor_op" (e_nat 7) (e_nat 0) in
  let () = expect_eq ~raise program "lsl_op" (e_nat 1000) (e_nat 128000) in
  let () = expect_eq ~raise program "lsr_op" (e_nat 128000) (e_nat 1000) in
  ()

let bitwise_arithmetic_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/bitwise_arithmetic.jsligo" in
  let () = expect_eq ~raise program "or_op" (e_nat 7) (e_nat 7) in
  let () = expect_eq ~raise program "or_op" (e_nat 3) (e_nat 7) in
  let () = expect_eq ~raise program "or_op" (e_nat 2) (e_nat 6) in
  let () = expect_eq ~raise program "or_op" (e_nat 14) (e_nat 14) in
  let () = expect_eq ~raise program "or_op" (e_nat 10) (e_nat 14) in
  let () = expect_eq ~raise program "and_op" (e_nat 7) (e_nat 7) in
  let () = expect_eq ~raise program "and_op" (e_nat 3) (e_nat 3) in
  let () = expect_eq ~raise program "and_op" (e_nat 2) (e_nat 2) in
  let () = expect_eq ~raise program "and_op" (e_nat 14) (e_nat 6) in
  let () = expect_eq ~raise program "and_op" (e_nat 10) (e_nat 2) in
  let () = expect_eq ~raise program "xor_op" (e_nat 0) (e_nat 7) in
  let () = expect_eq ~raise program "xor_op" (e_nat 7) (e_nat 0) in
  let () = expect_eq ~raise program "lsl_op" (e_nat 1000) (e_nat 128000) in
  let () = expect_eq ~raise program "lsr_op" (e_nat 128000) (e_nat 1000) in
  ()

let string_arithmetic ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/string_arithmetic.ligo" in
  let () = expect_eq ~raise program "concat_op" (e_string "foo") (e_string "foototo") in
  let () = expect_eq ~raise program "concat_op" (e_string "") (e_string "toto") in
  let () = expect_eq ~raise program "slice_op" (e_string "tata") (e_string "at") in
  let () = expect_eq ~raise program "slice_op" (e_string "foo") (e_string "oo") in
  let () = expect_fail ~raise program "slice_op" (e_string "ba") in
  ()

let string_arithmetic_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/string_arithmetic.mligo" in
  let () = expect_eq ~raise program "size_op"  (e_string "tata") (e_nat 4) in
  let () = expect_eq ~raise program "slice_op" (e_string "tata") (e_string "at") in
  let () = expect_eq ~raise program "slice_op" (e_string "foo") (e_string "oo") in
  let () = expect_eq ~raise program "concat_syntax" (e_string "string_") (e_string "string_test_literal")
  in ()

let string_arithmetic_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/string_arithmetic.religo" in
  let () = expect_eq ~raise program "size_op"  (e_string "tata") (e_nat 4) in
  let () = expect_eq ~raise program "slice_op" (e_string "tata") (e_string "at") in
  let () = expect_eq ~raise program "slice_op" (e_string "foo") (e_string "oo") in
  let () = expect_eq ~raise program "concat_syntax" (e_string "string_") (e_string "string_test_literal")
  in ()

let string_arithmetic_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/string_arithmetic.jsligo" in
  let () = expect_eq ~raise program "size_op"  (e_string "tata") (e_nat 4) in
  let () = expect_eq ~raise program "slice_op" (e_string "tata") (e_string "at") in
  let () = expect_eq ~raise program "slice_op" (e_string "foo") (e_string "oo") in
  let () = expect_eq ~raise program "concat_syntax" (e_string "string_") (e_string "string_test_literal")
  in ()
  

let bytes_arithmetic ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/bytes_arithmetic.ligo" in
  let foo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let foototo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let toto = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "7070" in
  let empty = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "" in
  let tata = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "ff7a7aff" in
  let at = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "7a7a" in
  let ba = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "ba" in
  let () = expect_eq ~raise program "concat_op" foo foototo in
  let () = expect_eq ~raise program "concat_op" empty toto in
  let () = expect_eq ~raise program "slice_op" tata at in
  let () = expect_fail ~raise program "slice_op" foo in
  let () = expect_fail ~raise program "slice_op" ba in
  let b1 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman" foo in
  let () = expect_eq_core ~raise program "hasherman" foo b1 in
  let b3 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@ Ast_core.Misc.assert_value_eq (b3 , b1) in
  ()

let comparable_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/comparable.mligo" in
  let () = expect_eq ~raise program "address_" (e_address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx") (e_bool false) in
  let () = expect_eq ~raise program "bool_" (e_bool true) (e_bool false) in
  let () = expect_eq ~raise program "bytes_" (e_bytes_string "deadbeaf") (e_bool false) in
  let () = expect_eq ~raise program "int_" (e_int 1) (e_bool false) in
  let () = expect_eq ~raise program "mutez_" (e_mutez 1) (e_bool false) in
  let () = expect_eq ~raise program "nat_" (e_nat 1) (e_bool false) in
  let () = expect_eq ~raise program "option_" (e_some (e_int 1)) (e_bool false) in
  (*
  let () = expect_eq ~raise program "sum_" (e_constructor "A" (e_int 1)) (e_bool false) in
  *)
  let () = expect_eq ~raise program "string_" (e_string "foo") (e_bool false) in
  let () = expect_eq ~raise program "timestamp_" (e_timestamp 101112) (e_bool false) in
  let () = expect_eq ~raise program "unit_" (e_unit ()) (e_bool false) in
  (*
  let () = expect_eq ~raise program "sum" (e_constructor "A" (e_int 1)) (e_bool false) in
  *)
  let open Tezos_crypto in
  let pkh, pk, sk = Signature.generate_key () in
  let key_hash = Signature.Public_key_hash.to_b58check @@ pkh in
  let () = expect_eq ~raise program "key_hash_" (e_key_hash key_hash) (e_bool false) in
  let key = Signature.Public_key.to_b58check @@ pk in
  let () = expect_eq ~raise program "key_" (e_key key) (e_bool false) in
  let signed = Signature.to_b58check @@ Signature.sign sk (Bytes.of_string "hello world") in
  let () = expect_eq ~raise program "signature_" (e_signature signed) (e_bool false) in
  let chain_id = Tezos_crypto.Base58.simple_encode
    Tezos_base__TzPervasives.Chain_id.b58check_encoding
    Tezos_base__TzPervasives.Chain_id.zero in
  let () = expect_eq ~raise program "chain_id_" (e_chain_id chain_id) (e_bool false) in

  let pair = e_pair (e_int 1) (e_int 2) in
  let () = expect_eq ~raise program "comp_pair" pair (e_bool false) in
  (* let tuple = e_tuple [e_int 1; e_int 2; e_int 3] in
  let () = expect_string_failwith program "uncomp_pair_1" tuple "" in
  let pair = e_pair pair (e_int 3) in
  let () = expect_string_failwith program "uncomp_pair_2" pair "" in *)
  let comb = e_pair (e_int 3) (e_pair (e_int 1) (e_nat 2)) in
  let () = expect_eq ~raise program "comb_record" comb (e_bool false) in
  ()

let crypto ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/crypto.ligo" in
  let foo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let foototo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let b1 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman512" foo in
  let () = expect_eq_core ~raise program "hasherman512" foo b1 in
  let b2 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman512" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b2 , b1) in
  let b4 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman_blake" foo in
  let () = expect_eq_core ~raise program "hasherman_blake" foo b4 in
  let b5 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman_blake" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b5 , b4) in
  ()

let crypto_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/crypto.mligo" in
  let foo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let foototo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let b1 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman512" foo in
  let () = expect_eq_core ~raise program "hasherman512" foo b1 in
  let b2 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman512" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b2 , b1) in
  let b4 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman_blake" foo in
  let () = expect_eq_core ~raise program "hasherman_blake" foo b4 in
  let b5 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman_blake" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b5 , b4) in
  ()

let crypto_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/crypto.religo" in
  let foo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let foototo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let b1 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman512" foo in
  let () = expect_eq_core ~raise program "hasherman512" foo b1 in
  let b2 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman512" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@ Ast_core.Misc.assert_value_eq (b2 , b1) in
  let b4 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman_blake" foo in
  let () = expect_eq_core ~raise program "hasherman_blake" foo b4 in
  let b5 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman_blake" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@ Ast_core.Misc.assert_value_eq (b5 , b4) in
  ()

let crypto_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/crypto.jsligo" in
  let foo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let foototo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let b1 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman512" foo in
  let () = expect_eq_core ~raise program "hasherman512" foo b1 in
  let b2 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman512" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@ Ast_core.Misc.assert_value_eq (b2 , b1) in
  let b4 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman_blake" foo in
  let () = expect_eq_core ~raise program "hasherman_blake" foo b4 in
  let b5 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman_blake" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@ Ast_core.Misc.assert_value_eq (b5 , b4) in
  ()
  

let bytes_arithmetic_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/bytes_arithmetic.mligo" in
  let foo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let foototo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let toto = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "7070" in
  let empty = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "" in
  let tata = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "ff7a7aff" in
  let at = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "7a7a" in
  let ba = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "ba" in
  let () = expect_eq ~raise program "concat_op" foo foototo in
  let () = expect_eq ~raise program "concat_op" empty toto in
  let () = expect_eq ~raise program "slice_op" tata at in
  let () = expect_fail ~raise program "slice_op" foo in
  let () = expect_fail ~raise program "slice_op" ba in
  let b1 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman" foo in
  let () = expect_eq_core ~raise program "hasherman" foo b1 in
  let b3 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b3 , b1) in
  ()

let bytes_arithmetic_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/bytes_arithmetic.religo" in
  let foo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let foototo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let toto = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "7070" in
  let empty = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "" in
  let tata = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "ff7a7aff" in
  let at = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "7a7a" in
  let ba = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "ba" in
  let () = expect_eq ~raise program "concat_op" foo foototo in
  let () = expect_eq ~raise program "concat_op" empty toto in
  let () = expect_eq ~raise program "slice_op" tata at in
  let () = expect_fail ~raise program "slice_op" foo in
  let () = expect_fail ~raise program "slice_op" ba in
  let b1 = Test_helpers.run_typed_program_with_imperative_input ~raise program"hasherman" foo in
  let () = expect_eq_core ~raise program "hasherman" foo b1 in
  let b3 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b3 , b1) in
  ()

let bytes_arithmetic_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/bytes_arithmetic.jsligo" in
  let foo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let foototo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let toto = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "7070" in
  let empty = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "" in
  let tata = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "ff7a7aff" in
  let at = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "7a7a" in
  let ba = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "ba" in
  let () = expect_eq ~raise program "concat_op" foo foototo in
  let () = expect_eq ~raise program "concat_op" empty toto in
  let () = expect_eq ~raise program "slice_op" tata at in
  let () = expect_fail ~raise program "slice_op" foo in
  let () = expect_fail ~raise program "slice_op" ba in
  let b1 = Test_helpers.run_typed_program_with_imperative_input ~raise program"hasherman" foo in
  let () = expect_eq_core ~raise program "hasherman" foo b1 in
  let b3 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b3 , b1) in
  ()
  

let set_arithmetic ~add_warning ~raise () : unit =
  let program   = type_file_w ~raise "./contracts/set_arithmetic.ligo" in
  let program_1 = type_file_w ~raise "./contracts/set_arithmetic-1.ligo" in
  let () =
    expect_eq ~raise program_1 "iter_op"
      (e_set [e_int 2 ; e_int 4 ; e_int 7])
      (e_int 13) in
  let () =
    expect_eq ~raise program "add_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let () =
    expect_eq ~raise program "add_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let () =
    expect_eq ~raise program "remove_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let () =
    expect_eq ~raise program "remove_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let () =
    expect_eq ~raise program "remove_syntax"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let () =
    expect_eq ~raise program "remove_deep"
      (e_pair
         (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
         (e_nat 42))
      (e_pair
        (e_set [e_string "foo" ; e_string "bar"])
        (e_nat 42))
  in
  let () =
    expect_eq ~raise program "patch_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar"; e_string "foobar"]) in
  let () =
    expect_eq ~raise program "patch_op_deep"
      (e_pair
         (e_set [e_string "foo" ; e_string "bar"])
         (e_nat 42))
      (e_pair
         (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
         (e_nat 42)) in
  let () =
    expect_eq ~raise program "mem_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_bool true) in
  let () =
    expect_eq ~raise program "mem_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_bool false) in
  let () =
    expect_eq ~raise program "fold_op"
      (e_set [ e_int 4 ; e_int 10 ])
      (e_int 29)
  in
  ()

let set_arithmetic_mligo ~add_warning ~raise () : unit =
  let program   = type_file_w ~raise "./contracts/set_arithmetic.mligo" in
  let program_1 = type_file_w ~raise "./contracts/set_arithmetic-1.mligo" in
  let () =
    expect_eq ~raise program "literal_op"
      (e_unit ())
      (e_set [e_string "foo"; e_string "bar"; e_string "foobar"])
  in
  let () =
    expect_eq ~raise program "size_op"
      (e_set [e_string "foo"; e_string "bar"; e_string "foobar"])
      (e_nat 3) in
  let () =
    expect_eq ~raise program "add_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let () =
    expect_eq ~raise program "add_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let () =
    expect_eq ~raise program "remove_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let () =
    expect_eq ~raise program "remove_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let () =
    expect_eq ~raise program_1 "fold_op"
      (e_set [ e_int 4 ; e_int 10 ])
      (e_list [e_int 10; e_int  4 ])
  in
  let () =
    expect_eq ~raise program_1 "fold_right"
      (e_set [ e_int 4 ; e_int 10 ])
      (e_list [e_int 4; e_int  10 ])
  in
  ()

let set_arithmetic_religo ~add_warning ~raise () : unit =
  let program   = type_file_w ~raise "./contracts/set_arithmetic.religo" in
  let program_1 = type_file_w ~raise "./contracts/set_arithmetic-1.ligo" in
  let () =
    expect_eq ~raise program "literal_op"
      (e_unit ())
      (e_set [e_string "foo"; e_string "bar"; e_string "foobar"])
  in
  let () =
    expect_eq ~raise program "size_op"
      (e_set [e_string "foo"; e_string "bar"; e_string "foobar"])
      (e_nat 3) in
  let () =
    expect_eq ~raise program "add_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let () =
    expect_eq ~raise program "add_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let () =
    expect_eq ~raise program "remove_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let () =
    expect_eq ~raise program "remove_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let () =
    expect_eq ~raise program_1 "fold_op"
      (e_set [ e_int 4 ; e_int 10 ])
      (e_int 29)
  in
  ()

let set_arithmetic_jsligo ~add_warning ~raise () : unit =
  let program   = type_file_w ~raise "./contracts/set_arithmetic.jsligo" in
  let program_1 = type_file_w ~raise "./contracts/set_arithmetic-1.ligo" in
  let () =
    expect_eq ~raise program "literal_op"
      (e_unit ())
      (e_set [e_string "foo"; e_string "bar"; e_string "foobar"])
  in
  let () =
    expect_eq ~raise program "size_op"
      (e_set [e_string "foo"; e_string "bar"; e_string "foobar"])
      (e_nat 3) in
  let () =
    expect_eq ~raise program "add_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let () =
    expect_eq ~raise program "add_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let () =
    expect_eq ~raise program "remove_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let () =
    expect_eq ~raise program "remove_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let () =
    expect_eq ~raise program_1 "fold_op"
      (e_set [ e_int 4 ; e_int 10 ])
      (e_int 29)
  in
  ()

let unit_expression ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/unit.ligo" in
  expect_eq_evaluate ~raise program "u" (e_unit ())

let string_expression ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/string.ligo" in
  let _ = expect_eq_evaluate ~raise program "s" (e_string "toto") in
  expect_eq_evaluate ~raise program "y" (e_string "foototobar")

let include_ ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/includer.ligo" in
  expect_eq_evaluate ~raise program "bar" (e_int 144)

let include_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/includer.mligo" in
  expect_eq_evaluate ~raise program "bar" (e_int 144)

let include_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/includer.religo" in
  expect_eq_evaluate ~raise program "bar" (e_int 144)

let include_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/includer.jsligo" in
  expect_eq_evaluate ~raise program "bar" (e_int 144)


let modules ~raise program : unit =
  let () = expect_eq_evaluate ~raise program "toto" (e_int 42) in
  expect_eq ~raise program "add" (e_pair (e_int 1) (e_int 2)) (e_int 3)

let modules_ligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/modules.ligo" in
  modules ~raise program

let modules_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/modules.mligo" in
  modules ~raise program

let modules_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/modules.religo" in
  modules ~raise program

let modules_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/modules.jsligo" in
  modules ~raise program
  

let record_ez_int names n =
  e_record_ez @@ List.map ~f:(fun x -> x, e_int n) names

let tuple_ez_int names n =
  e_tuple @@ List.map ~f:(fun _ -> e_int n) names

let multiple_parameters ~add_warning ~raise () : unit  =
  let program = type_file_w ~raise "./contracts/multiple-parameters.ligo" in
  let aux ((name : string) , make_input , make_output) =
    let make_output' = fun n -> e_int @@ make_output n in
    expect_eq_n ~raise program name make_input make_output'
  in
  let _ = List.map ~f:aux [
      ("ab", tuple_ez_int ["a";"b"], fun n -> 2 * n) ;
      ("abcd", tuple_ez_int ["a";"b";"c";"d"], fun n -> 4 * n + 2) ;
      ("abcde", tuple_ez_int ["a";"b";"c";"d";"e"], fun n -> 2 * n + 3) ;
    ] in
  ()

let multiple_parameters_mligo ~add_warning ~raise () : unit  =
  let program = type_file_w ~raise "./contracts/multiple-parameters.mligo" in
  let aux ((name : string) , make_input , make_output) =
    let make_output' = fun n -> e_int @@ make_output n in
    expect_eq_n ~raise program name make_input make_output'
  in
  let _ = List.map ~f:aux [
      (* Didn't include the other tests because they're probably not necessary *)
      ("abcde", tuple_ez_int ["a";"b";"c";"d";"e"], fun n -> 2 * n + 3) ;
    ] in
  ()

let multiple_parameters_religo ~add_warning ~raise () : unit  =
  let program = type_file_w ~raise "./contracts/multiple-parameters.religo" in
  let aux ((name : string) , make_input , make_output) =
    let make_output' = fun n -> e_int @@ make_output n in
    expect_eq_n ~raise program name make_input make_output'
  in
  let _ = List.map ~f:aux [
      (* Didn't include the other tests because they're probably not necessary *)
      ("abcde", tuple_ez_int ["a";"b";"c";"d";"e"], fun n -> 2 * n + 3) ;
    ] in
  ()

let multiple_parameters_jsligo ~add_warning ~raise () : unit  =
  let program = type_file_w ~raise "./contracts/multiple-parameters.jsligo" in
  let aux ((name : string) , make_input , make_output) =
    let make_output' = fun n -> e_int @@ make_output n in
    expect_eq_n ~raise program name make_input make_output'
  in
  let _ = List.map ~f:aux [
      (* Didn't include the other tests because they're probably not necessary *)
      ("abcde", tuple_ez_int ["a";"b";"c";"d";"e"], fun n -> 2 * n + 3) ;
    ] in
  ()

let record ~add_warning ~raise () : unit  =
  let program = type_file_w ~raise "./contracts/record.ligo" in
  let () =
    let expected = record_ez_int ["foo" ; "bar"] 0 in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let () = expect_eq_evaluate ~raise program "a" (e_int 42) in
    let () = expect_eq_evaluate ~raise program "b" (e_int 142) in
    let () = expect_eq_evaluate ~raise program "c" (e_int 242) in
    ()
  in
  let () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection" make_input make_expected
  in
  let () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_record_ez [("foo" , e_int 256) ; ("bar" , e_int n) ] in
    expect_eq_n ~raise program "modify" make_input make_expected
  in
  let () =
    let make_input = record_ez_int ["a" ; "b" ; "c"] in
    let make_expected = fun n -> e_record_ez [
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int 42)
      ] in
    expect_eq_n ~raise program "modify_abc" make_input make_expected
  in
  let () =
    let expected = record_ez_int ["a";"b";"c";"d";"e"] 23 in
    expect_eq_evaluate ~raise program "br" expected
  in
  let () =
    let make_input = fun n -> e_record_ez [("inner", record_ez_int ["a";"b";"c"] n)] in
    let make_expected = fun n -> e_record_ez [("inner", e_record_ez[
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int n)
    ])] in
    expect_eq_n ~raise program "modify_inner" make_input make_expected
  in
  ()

let record_mligo ~add_warning ~raise () : unit  =
  let program = type_file_w ~raise "./contracts/record.mligo" in
  let () =
    let expected = record_ez_int ["foo" ; "bar"] 0 in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let () = expect_eq_evaluate ~raise program "a" (e_int 42) in
    let () = expect_eq_evaluate ~raise program "b" (e_int 142) in
    let () = expect_eq_evaluate ~raise program "c" (e_int 242) in
    ()
  in
  let () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection" make_input make_expected
  in
  let () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_record_ez [("foo" , e_int 256) ; ("bar" , e_int n) ] in
    expect_eq_n ~raise program "modify" make_input make_expected
  in
  let () =
    let make_input = record_ez_int ["a" ; "b" ; "c"] in
    let make_expected = fun n -> e_record_ez [
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int 42)
      ] in
    expect_eq_n ~raise program "modify_abc" make_input make_expected
  in
  let () =
    let expected = record_ez_int ["a";"b";"c";"d";"e"] 23 in
    expect_eq_evaluate ~raise program "br" expected
  in
  let () =
    let make_input = fun n -> e_record_ez [("inner", record_ez_int ["a";"b";"c"] n)] in
    let make_expected = fun n -> e_record_ez [("inner", e_record_ez [
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int n)
    ])] in
    expect_eq_n ~raise program "modify_inner" make_input make_expected
  in
  ()

let record_religo ~add_warning ~raise () : unit  =
  let program = type_file_w ~raise "./contracts/record.religo" in
  let () =
    let expected = record_ez_int ["foo" ; "bar"] 0 in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let () = expect_eq_evaluate ~raise program "a" (e_int 42) in
    let () = expect_eq_evaluate ~raise program "b" (e_int 142) in
    let () = expect_eq_evaluate ~raise program "c" (e_int 242) in
    ()
  in
  let () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection" make_input make_expected
  in
  let () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_record_ez [("foo" , e_int 256) ; ("bar" , e_int n) ] in
    expect_eq_n ~raise program "modify" make_input make_expected
  in
  let () =
    let make_input = record_ez_int ["a" ; "b" ; "c"] in
    let make_expected = fun n -> e_record_ez [
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int 42)
      ] in
    expect_eq_n ~raise program "modify_abc" make_input make_expected
  in
  let () =
    let expected = record_ez_int ["a";"b";"c";"d";"e"] 23 in
    expect_eq_evaluate ~raise program "br" expected
  in
  let () =
    let make_input = fun n -> e_record_ez [("inner", record_ez_int ["a";"b";"c"] n)] in
    let make_expected = fun n -> e_record_ez [("inner", e_record_ez[
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int n)
    ])] in
    expect_eq_n ~raise program "modify_inner" make_input make_expected
  in
  ()

let record_jsligo ~add_warning ~raise () : unit  =
  let program = type_file_w ~raise "./contracts/record.jsligo" in
  let () =
    let expected = record_ez_int ["foo" ; "bar"] 0 in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let () = expect_eq_evaluate ~raise program "a" (e_int 42) in
    let () = expect_eq_evaluate ~raise program "b" (e_int 142) in
    let () = expect_eq_evaluate ~raise program "c" (e_int 242) in
    ()
  in
  let () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection" make_input make_expected
  in
  let () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_record_ez [("foo" , e_int 256) ; ("bar" , e_int n) ] in
    expect_eq_n ~raise program "modify" make_input make_expected
  in
  let () =
    let make_input = record_ez_int ["a" ; "b" ; "c"] in
    let make_expected = fun n -> e_record_ez [
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int 42)
      ] in
    expect_eq_n ~raise program "modify_abc" make_input make_expected
  in
  let () =
    let expected = record_ez_int ["a";"b";"c";"d";"e"] 23 in
    expect_eq_evaluate ~raise program "br" expected
  in
  let () =
    let make_input = fun n -> e_record_ez [("inner", record_ez_int ["a";"b";"c"] n)] in
    let make_expected = fun n -> e_record_ez [("inner", e_record_ez[
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int n)
    ])] in
    expect_eq_n ~raise program "modify_inner" make_input make_expected
  in
  ()

let tuple ~add_warning ~raise () : unit  =
  let program = type_file_w ~raise "./contracts/tuple.ligo" in
  let ez n =
    e_tuple (List.map ~f:e_int n) in
  let () =
    let expected = ez [0 ; 0] in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let make_input = fun n -> ez [n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection" make_input make_expected
  in
  let () =
    let make_input = fun n -> ez [n ; 2 * n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection_abc" make_input make_expected
  in
  let () =
    let make_input = fun n -> ez [n ; n ; n] in
    let make_expected = fun n -> ez [n ; 2048 ; n] in
    expect_eq ~raise program "modify_abc" (make_input 12) (make_expected 12)
  in
  let () =
    let make_input = fun n -> ez [n ; n ; n] in
    let make_expected = fun n -> ez [n ; 2048 ; n] in
    expect_eq_n ~raise program "modify_abc" make_input make_expected
  in
  let () =
    let expected = ez [0 ; 1 ; 2 ; 3 ; 4; 5; 6; 7; 8; 9; 10; 11] in
    expect_eq_evaluate ~raise program "br" expected
  in
  let () =
    let make_input = fun n -> ez [n; n; n; n; n; n; n; n; n; n; n; n] in
    let make_expected = fun n -> ez [n; n; n; n; n; n; n; n; n; n; n; 2048] in
    expect_eq_n ~raise program "update" make_input make_expected
  in
  ()

let tuple_mligo ~add_warning ~raise () : unit  =
  let program = type_file_w ~raise "./contracts/tuple.mligo" in
  let ez n =
    e_tuple (List.map ~f:e_int n) in
  let () =
    let expected = ez [0 ; 0] in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let make_input = fun n -> ez [n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection" make_input make_expected
  in
  let () =
    let make_input = fun n -> ez [n ; 2 * n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection_abc" make_input make_expected
  in
  let () =
    let expected = ez [23 ; 23 ; 23 ; 23 ; 23] in
    expect_eq_evaluate ~raise program "br" expected
  in
  ()


let tuple_religo ~add_warning ~raise () : unit  =
  let program = type_file_w ~raise "./contracts/tuple.religo" in
  let ez n =
    e_tuple (List.map ~f:e_int n) in
  let () =
    let expected = ez [0 ; 0] in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let make_input = fun n -> ez [n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection" make_input make_expected
  in
  let () =
    let make_input = fun n -> ez [n ; 2 * n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection_abc" make_input make_expected
  in
  let () =
    let expected = ez [23 ; 23 ; 23 ; 23 ; 23] in
    expect_eq_evaluate ~raise program "br" expected
  in
  ()

let tuple_jsligo ~add_warning ~raise () : unit  =
  let program = type_file_w ~raise "./contracts/tuple.jsligo" in
  let ez n =
    e_tuple (List.map ~f:e_int n) in
  let () =
    let expected = ez [0 ; 0] in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let make_input = fun n -> ez [n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection" make_input make_expected
  in
  let () =
    let make_input = fun n -> ez [n ; 2 * n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection_abc" make_input make_expected
  in
  let () =
    let expected = ez [23 ; 23 ; 23 ; 23 ; 23] in
    expect_eq_evaluate ~raise program "br" expected
  in
  ()

let option ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/option.ligo" in
  let () =
    let expected = e_some (e_int 42) in
    expect_eq_evaluate ~raise program "s" expected
  in
  let () =
    let expected = e_typed_none (t_int ()) in
    expect_eq_evaluate ~raise program "n" expected
  in
  let () =
    let expected = e_typed_none (t_int ()) in
    expect_eq ~raise program "assign" (e_int 12) expected
  in
  ()

let moption ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/option.mligo" in
  let () =
    let expected = e_some (e_int 42) in
    expect_eq_evaluate ~raise program "s" expected
  in
  let () =
    let expected = e_typed_none (t_int ()) in
    expect_eq_evaluate ~raise program "n" expected
  in
  ()

let reoption ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/option.religo" in
  let () =
    let expected = e_some (e_int 42) in
    expect_eq_evaluate ~raise program "s" expected
  in
  let () =
    let expected = e_typed_none (t_int ()) in
    expect_eq_evaluate ~raise program "n" expected
  in
  ()

let jsoption ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/option.jsligo" in
  let () =
    let expected = e_some (e_int 42) in
    expect_eq_evaluate ~raise program "s" expected
  in
  let () =
    let expected = e_typed_none (t_int ()) in
    expect_eq_evaluate ~raise program "n" expected
  in
  ()
  

let map_ ~raise type_f path : unit =
  let program = type_f path in
  let ez lst =
    let lst' = List.map ~f:(fun (x, y) -> e_int x, e_int y) lst in
    e_typed_map lst' (t_int ()) (t_int ())
  in
   let () =
    let make_input = fun n ->
      let m = ez [(23 , 0) ; (42 , 0)] in
      e_tuple [(e_int n) ; m]
    in
    let make_expected = fun n -> ez [(23 , n) ; (42 , 0)] in
    expect_eq_n_pos_small ~raise program "set_" make_input make_expected
  in
  let () =
    let input = (e_pair (e_int 23) (ez [(42, 42)])) in
    let expected = ez [(23, 23) ; (42, 42)] in
    expect_eq ~raise program "add" input expected
  in
  let () =
    let input = ez [(23, 23) ; (42, 42)] in
    let expected = ez [23, 23] in
    expect_eq ~raise program "rm" input expected
  in
  let () =
    let input = ez [(0,0) ; (1,1) ; (2,2)] in
    let expected = ez [(0, 5) ; (1, 6) ; (2, 7)] in
    expect_eq ~raise program "patch_" input expected
  in
  let () =
    let input = (e_pair
                   (ez [(0,0) ; (1,1) ; (2,2)])
                   (e_nat 10)) in
    let expected = (e_pair
                      (ez [(0,0) ; (1,9) ; (2,2)])
                      (e_nat 10)) in
    expect_eq ~raise program "patch_deep" input expected
  in
  let () =
    let make_input = fun n -> ez List.(map ~f:(fun x -> (x, x)) @@ range 0 n) in
    let make_expected = e_nat in
    expect_eq_n_strict_pos_small ~raise program "size_" make_input make_expected
  in
  let () =
    let make_input = fun n -> ez [(23, n) ; (42, 4)] in
    let make_expected = fun _ -> e_some @@ e_int 4 in
    expect_eq_n ~raise program "get" make_input make_expected
  in
  let () =
    let input_map = ez [(23, 10) ; (42, 4)] in
    expect_eq ~raise program "mem" (e_tuple [(e_int 23) ; input_map]) (e_bool true)
  in
  let () =
    let input_map = ez [(23, 10) ; (42, 4)] in
    expect_eq ~raise program "mem" (e_tuple [(e_int 1000) ; input_map]) (e_bool false)
  in
  let () = expect_eq_evaluate ~raise program "empty_map"
    (e_annotation (e_map []) (t_map (t_int()) (t_int()))) in
  let () =
    let expected = ez @@ List.map ~f:(fun x -> (x, 23)) [144 ; 51 ; 42 ; 120 ; 421] in
    expect_eq_evaluate ~raise program "map1" expected
  in
  let () =
    let expected = ez [(23, 0) ; (42, 0)] in
    expect_eq_evaluate ~raise program "map2" expected
  in
  let () =
    let input = ez [(1 , 1) ; (2 , 2) ; (3 , 3) ] in
    let expected = e_unit () in
    expect_eq ~raise program "iter_op" input expected
  in
  let () =
    let input = ez [(1 , 10) ; (2 , 20) ; (3 , 30) ] in
    let expected = ez [(1 , 11) ; (2 , 21) ; (3 , 31) ] in
    expect_eq ~raise program "map_op" input expected
  in
  let () =
    let input = ez [(1 , 10) ; (2 , 20) ; (3 , 30) ] in
    let expected = e_int 76 in
    expect_eq ~raise program "fold_op" input expected
  in
  let () =
    let input = ez [(2 , 20) ; (42 , 10)] in
    let expected = ez [(2 , 20) ; (32 , 16) ] in
    expect_eq ~raise program "deep_op" input expected
  in
  ()

let big_map_ ~raise type_f path : unit =
  let program = type_f path in
  let ez lst =
    let lst' = List.map ~f:(fun (x, y) -> e_int x, e_int y) lst in
    (e_typed_big_map lst' (t_int ()) (t_int()))
  in
  let () =
    let make_input = fun n ->
      let m = ez [(23 , 0) ; (42 , 0)] in
      e_tuple [(e_int n) ; m]
    in
    let make_expected = fun n -> ez [(23 , n) ; (42 , 0)] in
    expect_eq_n_pos_small ~raise program "set_" make_input make_expected
  in
  let () =
    let input = (e_pair (e_int 23) (ez [(42, 42)])) in
    let expected = ez [(23, 23) ; (42, 42)] in
    expect_eq ~raise program "add" input expected
  in
  let () =
    let make_input = fun n -> ez [(23, n) ; (42, 4)] in
    let make_expected = fun _ -> e_some @@ e_int 4 in
    expect_eq_n ~raise program "get" make_input make_expected
  in
  let () =
    let input = ez [(23, 23) ; (42, 42)] in
    let expected = ez [23, 23] in
    expect_eq ~raise program "rm" input expected
  in
  ()


let map       ~add_warning ~raise () : unit = map_     ~raise (type_file_w ~raise) "./contracts/map.ligo"
let mmap      ~add_warning ~raise () : unit = map_     ~raise (type_file_w ~raise) "./contracts/map.mligo"
let remap     ~add_warning ~raise () : unit = map_     ~raise (type_file_w ~raise) "./contracts/map.religo"
let jsmap     ~add_warning ~raise () : unit = map_     ~raise (type_file_w ~raise) "./contracts/map.jsligo"
let big_map   ~add_warning ~raise () : unit = big_map_ ~raise (type_file_w ~raise) "./contracts/big_map.ligo"
let mbig_map  ~add_warning ~raise () : unit = big_map_ ~raise (type_file_w ~raise) "./contracts/big_map.mligo"
let rebig_map ~add_warning ~raise () : unit = big_map_ ~raise (type_file_w ~raise) "./contracts/big_map.religo"
let jsbig_map ~add_warning ~raise () : unit = big_map_ ~raise (type_file_w ~raise) "./contracts/big_map.jsligo"



let list ~add_warning ~raise () : unit =
  Format.printf "Pre_type \n%!";
  let program = type_file_w ~raise "./contracts/list.ligo" in
  let ez lst =
    let lst' = List.map ~f:e_int lst in
    e_typed_list lst' (t_int ())
  in
  Format.printf "Post_type \n%!";
  let () =
    let expected = ez [23 ; 42] in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let expected = ez [144 ; 23 ; 42] in
    expect_eq_evaluate ~raise program "fb2" expected
  in
  let () =
    let expected = ez [688 ; 144 ; 23 ; 42] in
    expect_eq_evaluate ~raise program "fb3" expected
  in
  let () =
    let expected = e_some @@ e_int 23 in
    expect_eq_evaluate ~raise program "fb_head" expected
  in
  let () =
    let expected = e_some @@ ez [42] in
    expect_eq_evaluate ~raise program "fb_tail" expected
  in
  let () =
    let make_input = fun n -> (ez @@ List.range 0 n) in
    let make_expected = e_nat in
    expect_eq_n_strict_pos_small ~raise program "size_" make_input make_expected
  in
  let () =
    let expected = ez [144 ; 51 ; 42 ; 120 ; 421] in
    expect_eq_evaluate ~raise program "bl" expected
  in
  let () =
    expect_eq ~raise program "fold_op"
      (e_list [e_int 2 ; e_int 4 ; e_int 7])
      (e_int 23)
  in
  (* not working since purification (problem with effect in out of iter
  let () =
    expect_eq ~raise program "iter_op"
      (e_list [e_int 2 ; e_int 4 ; e_int 7])
      (e_int 13)
  in
  *)
  let () =
    expect_eq ~raise program "map_op"
      (e_list [e_int 2 ; e_int 4 ; e_int 7])
      (e_list [e_int 3 ; e_int 5 ; e_int 8])
  in
  ()

let condition ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/condition.ligo" in
  let _ =
    let make_input = e_int in
    let make_expected = fun n -> e_int (if n = 2 then 42 else 0) in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let _ =
    let make_expected = fun b -> e_int (if b then 42 else 1) in
    expect_eq_b ~raise program "foo" make_expected
  in
  ()

let condition_mligo ~add_warning ~raise () : unit =
  let _ =
    let aux file =
      let program = type_file_w ~raise file in
      let make_input = e_int in
      let make_expected = fun n -> e_int (if n = 2 then 42 else 0) in
      expect_eq_n ~raise program "main"  make_input make_expected in
    List.map ~f:aux [
      "./contracts/condition.mligo";
      "./contracts/condition-shadowing.mligo";
      "./contracts/condition-annot.mligo";
    ] in
  ()

let condition_religo ~add_warning ~raise () : unit =
  let _ =
    let aux file =
      let program = type_file_w ~raise file in
      let make_input = e_int in
      let make_expected = fun n -> e_int (if n = 2 then 42 else 0) in
      expect_eq_n ~raise program "main"  make_input make_expected in
    List.map ~f:aux [
      "./contracts/condition.religo";
      "./contracts/condition-shadowing.religo";
      "./contracts/condition-annot.religo";
    ] in
  ()

let condition_jsligo ~add_warning ~raise () : unit =
  let _ =
    let aux file =
      let program = type_file_w ~raise file in
      let make_input = e_int in
      let make_expected = fun n -> e_int (if n = 2 then 42 else 0) in
      expect_eq_n ~raise program "main"  make_input make_expected in
    List.map ~f:aux [
      "./contracts/condition.jsligo";
      "./contracts/condition-shadowing.jsligo";
      "./contracts/condition-annot.jsligo";
    ] in
  ()

let sequence_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/sequence.mligo" in
  expect_eq ~raise program "y" (e_unit ()) (e_nat 1)

let eq_bool_common ~raise program =
  let _ =
    List.map ~f:(fun ( a , b , expected ) ->
        expect_eq ~raise program "main" (e_pair (e_bool a) (e_bool b)) (e_int expected))
    [
      ( false , false , 999 ) ;
      ( false , true  , 1   ) ;
      ( true  , false , 1   ) ;
      ( true  , true  , 999 ) ;
    ]
  in
  ()

let eq_bool ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/eq_bool.ligo" in
  eq_bool_common ~raise program

let eq_bool_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/eq_bool.mligo" in
  eq_bool_common ~raise program

let eq_bool_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/eq_bool.religo" in
  eq_bool_common ~raise program

let eq_bool_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/eq_bool.jsligo" in
  eq_bool_common ~raise program

let condition_simple ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/condition-simple.ligo" in
  let make_input = e_int in
  let make_expected = fun _ -> e_int 42 in
  expect_eq_n ~raise program "main" make_input make_expected

let loop1 ~add_warning ~raise () : unit =
  let _program = type_file_w ~raise "./contracts/loop1.ligo" in
  ()

let loop2 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop2.ligo" in
  let () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos ~raise program "dummy" make_input make_expected in
  ()

let loop3 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop3.ligo" in
  let () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos_mid ~raise program "counter" make_input make_expected in
  ()

let loop4 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop4.ligo" in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_nat (n * (n + 1) / 2) in
    expect_eq_n_pos_mid ~raise program "while_sum" make_input make_expected in
  ()

let loop5 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop5.ligo" in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_int (n * (n + 1) / 2) in
    expect_eq_n_pos_mid ~raise program "for_sum" make_input make_expected in
  ()

let loop6 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop6.ligo" in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_int (n * n) in
    expect_eq_n_pos_mid ~raise program "for_sum_step" make_input make_expected in
  ()

let loop7 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop7.ligo" in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_int 3) (e_string "totototo") in
    expect_eq ~raise program "for_collection_list" input expected in
  ()

let loop8 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop8.ligo" in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_int 6) (e_string "totototo") in
    expect_eq ~raise program "for_collection_set" input expected in
  ()

let loop9 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop9.ligo" in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_int 6) (e_string "123") in
    expect_eq ~raise program "for_collection_map_kv" input expected in
  ()

let loop10 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop10.ligo" in
  let input = e_unit () in
  let () =
    let expected = (e_int 0) in
    expect_eq ~raise program "for_collection_empty" input expected in
  ()

let loop11 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop11.ligo" in
  let input = e_unit () in
  let () =
    let expected = (e_int 13) in
    expect_eq ~raise program "for_collection_if_and_local_var" input expected in
  ()

let loop12 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop12.ligo" in
  let input = e_unit () in
  let () =
    let expected = (e_int 1020) in
    expect_eq ~raise program "for_collection_rhs_capture" input expected in
  ()

let loop13 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop13.ligo" in
  let input = e_unit () in
  let () =
    let expected = (e_int 1040) in
    expect_eq ~raise program "for_collection_proc_call" input expected in
  ()

let loop14 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop14.ligo" in
  let input = e_unit () in
  let () =
    let expected = (e_int 20) in
    expect_eq ~raise program "for_collection_comp_with_acc" input expected in
  ()

let loop15 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop15.ligo" in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_int 24)
      (e_string "1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two ") in
    expect_eq ~raise program "nested_for_collection" input expected in
  ()

let loop16 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop16.ligo" in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_int 24)
      (e_string "123123123") in
    expect_eq ~raise program "nested_for_collection_local_var" input expected in
  ()

let loop17 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop.ligo" in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_bool true) (e_int 4) in
    expect_eq ~raise program "inner_capture_in_conditional_block"  input expected in
  ()

let loop18 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop.ligo" in
  let input = e_unit () in
  let () =
    let ez lst =
      let lst' = List.map ~f:(fun (x, y) -> e_string x, e_int y) lst in
        e_typed_map lst' (t_string ()) (t_int ())
    in
    let expected = ez [ ("I" , 12) ; ("am" , 12) ; ("foo" , 12) ] in
    expect_eq ~raise program "for_collection_with_patches" input expected in
  ()

let loop ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop.ligo" in
  let () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos ~raise program "dummy" make_input make_expected in
  let () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos_mid ~raise program "counter" make_input make_expected in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_nat (n * (n + 1) / 2) in
    expect_eq_n_pos_mid ~raise program "while_sum" make_input make_expected in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_int (n * (n + 1) / 2) in
    expect_eq_n_pos_mid ~raise program "for_sum" make_input make_expected in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_int (n * n) in
    expect_eq_n_pos_mid ~raise program "for_sum_step" make_input make_expected in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_int 3) (e_string "totototo") in
    expect_eq ~raise program "for_collection_list" input expected in
  let () =
    let expected = e_pair (e_int 6) (e_string "totototo") in
    expect_eq ~raise program "for_collection_set" input expected in
  let () =
    let expected = e_pair (e_int 6) (e_string "123") in
    expect_eq ~raise program "for_collection_map_kv" input expected in
  let () =
    let expected = (e_int 0) in
    expect_eq ~raise program "for_collection_empty" input expected in
  let () =
    let expected = (e_int 13) in
    expect_eq ~raise program "for_collection_if_and_local_var" input expected in
  let () =
    let expected = (e_int 1020) in
    expect_eq ~raise program "for_collection_rhs_capture" input expected in
  let () =
    let expected = (e_int 1040) in
    expect_eq ~raise program "for_collection_proc_call" input expected in
  let () =
    let expected = (e_int 20) in
    expect_eq ~raise program "for_collection_comp_with_acc" input expected in
  let () =
    let expected = e_pair (e_int 24)
      (e_string "1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two ") in
    expect_eq ~raise program "nested_for_collection" input expected in
  let () =
    let expected = e_pair (e_int 24)
      (e_string "123123123") in
    expect_eq ~raise program "nested_for_collection_local_var" input expected in
  let () =
    let expected = e_pair (e_bool true) (e_int 4) in
    expect_eq ~raise program "inner_capture_in_conditional_block"  input expected in
  let () =
    let ez lst =
      let lst' = List.map ~f:(fun (x, y) -> e_string x, e_int y) lst in
        e_typed_map lst' (t_string ()) (t_int ())
    in
    let expected = ez [ ("I" , 12) ; ("am" , 12) ; ("foo" , 12) ] in
    expect_eq ~raise program "for_collection_with_patches" input expected in
  ()

(* Don't know how to assert parse error happens in this test framework
let for_fail ~add_warning ~raise () : unit =
  let program = type_file "./contracts/for_fail.ligo" in
  let () = expect_fail program "main" (e_nat 0)
  in () *)

let loop_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop.mligo" in
  let () =
    let input = e_int 0 in
    let expected = e_int 100 in
    expect_eq ~raise program "counter_simple" input expected
  in
  let () =
    let input = e_int 100 in
    let expected = e_int 5050 in
    expect_eq ~raise program "counter" input expected
  in
  let () =
    let input = e_int 100 in
    let expected = e_int 10000 in
    expect_eq ~raise program "counter_nest" input expected
  in ()

let loop_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop.religo" in
  let () =
    let input = e_int 0 in
    let expected = e_int 100 in
    expect_eq ~raise program "counter_simple" input expected
  in
  let () =
    let input = e_int 100 in
    let expected = e_int 5050 in
    expect_eq ~raise program "counter" input expected
  in
  let () =
    let input = e_int 100 in
    let expected = e_int 10000 in
    expect_eq ~raise program "counter_nest" input expected
  in ()

let loop_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop.jsligo" in
  let () =
    let input = e_int 0 in
    let expected = e_int 100 in
    expect_eq ~raise program "counter_simple" input expected
  in
  let () =
    let input = e_int 100 in
    let expected = e_int 5050 in
    expect_eq ~raise program "counter" input expected
  in
  let () =
    let input = e_int 100 in
    let expected = e_int 10000 in
    expect_eq ~raise program "counter_nest" input expected
  in ()

let loop2_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop2.jsligo" in
  (* let () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos program "dummy" make_input make_expected in *)
  let () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos_mid ~raise program "counter" make_input make_expected in
  (* let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_nat (n * (n + 1) / 2) in
    expect_eq_n_pos_mid program "while_sum" make_input make_expected in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_int (n * (n + 1) / 2) in
    expect_eq_n_pos_mid program "for_sum" make_input make_expected in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_int (n * n) in
    expect_eq_n_pos_mid program "for_sum_step" make_input make_expected in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_int 3) (e_string "totototo") in
    expect_eq ~raise program "for_collection_list" input expected in
  let () =
    let expected = e_pair (e_int 6) (e_string "totototo") in
    expect_eq ~raise program "for_collection_set" input expected in
  let () =
    let expected = e_pair (e_int 6) (e_string "123") in
    expect_eq ~raise program "for_collection_map_kv" input expected in
  let () =
    let expected = (e_int 0) in
    expect_eq ~raise program "for_collection_empty" input expected in
  let () =
    let expected = (e_int 13) in
    expect_eq ~raise program "for_collection_if_and_local_var" input expected in
  let () =
    let expected = (e_int 1020) in
    expect_eq ~raise program "for_collection_rhs_capture" input expected in
  let () =
    let expected = (e_int 1040) in
    expect_eq ~raise program "for_collection_proc_call" input expected in
  let () =
    let expected = (e_int 20) in
    expect_eq ~raise program "for_collection_comp_with_acc" input expected in
  let () =
    let expected = e_pair (e_int 24)
      (e_string "1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two ") in
    expect_eq ~raise program "nested_for_collection" input expected in
  let () =
    let expected = e_pair (e_int 24)
      (e_string "123123123") in
    expect_eq ~raise program "nested_for_collection_local_var" input expected in
  let () =
    let expected = e_pair (e_bool true) (e_int 4) in
    expect_eq ~raise program "inner_capture_in_conditional_block"  input expected in
  let () =
    let ez lst =
      let lst' = List.map ~f:(fun (x, y) -> e_string x, e_int y) lst in
        e_typed_map lst' (t_string ()) (t_int ())
    in
    let expected = ez [ ("I" , 12) ; ("am" , 12) ; ("foo" , 12) ] in
    expect_eq ~raise program "for_collection_with_patches" input expected in *)
  ()
  

let matching ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/match.ligo" in
  let () =
    let make_input = e_int in
    let make_expected = fun n -> e_int (if n = 2 then 42 else 0) in
    expect_eq_n ~raise program "match_bool" make_input make_expected
  in
  let () =
    let make_input = e_int in
    let make_expected = fun n-> e_int (if n = 2 then 42 else 0) in
    expect_eq_n ~raise program "match_expr_bool" make_input make_expected
  in
  let () =
    let aux n =
      let input = match n with
        | Some s -> e_some (e_int s)
        | None -> e_typed_none (t_int ()) in
      let expected = e_int (match n with
          | Some s -> s
          | None -> 23) in
      expect_eq ~raise program "match_option" input expected
    in
    List.iter ~f:aux
      [Some 0 ; Some 2 ; Some 42 ; Some 163 ; Some (-1) ; None]
  in
  let () =
    let aux n =
      let input = match n with
        | Some s -> e_some (e_int s)
        | None -> e_typed_none (t_int ()) in
      let expected = e_int (match n with
          | Some s -> s
          | None -> 42) in
      expect_eq ~raise program "match_expr_option" input expected
    in
    List.iter ~f:aux
      [Some 0 ; Some 2 ; Some 42 ; Some 163 ; Some (-1) ; None]
  in
  let () =
    let aux lst = e_annotation (e_list @@ List.map ~f:e_int lst) (t_list (t_int ())) in
    let () = expect_eq ~raise program "match_expr_list" (aux [ 14 ; 2 ; 3 ]) (e_int 14) in
    let () = expect_eq ~raise program "match_expr_list" (aux [ 13 ; 2 ; 3 ]) (e_int 13) in
    let () = expect_eq ~raise program "match_expr_list" (aux []) (e_int (-1)) in
    ()
  in
  ()

let declarations ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/declarations.ligo" in
  let make_input = e_int in
  let make_expected = fun n -> e_int (42 + n) in
  expect_eq ~raise program "main" (make_input 0) (make_expected 0);
  expect_eq_n ~raise program "main" make_input make_expected

let declaration_local ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/declaration-local.ligo" in
  let make_input = e_int in
  let make_expected = fun _ -> e_int 42 in
  expect_eq_n ~raise program "main" make_input make_expected

let quote_declaration ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/quote-declaration.ligo" in
  let make_input = e_int in
  let make_expected = fun n -> e_int (42 + 2 * n) in
  expect_eq_n ~raise program "main" make_input make_expected

let quote_declarations ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/quote-declarations.ligo" in
  let make_input = e_int in
  let make_expected = fun n -> e_int (74 + 2 * n) in
  expect_eq_n ~raise program "main" make_input make_expected

let counter_contract ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/counter.ligo" in
  let make_input = fun n-> e_pair (e_int n) (e_int 42) in
  let make_expected = fun n -> e_pair (e_typed_list [] (t_operation ())) (e_int (42 + n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let super_counter_contract ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/super-counter.ligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation ())) (e_int (op 42 n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let super_counter_contract_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/super-counter.mligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation ())) (e_int (op 42 n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let super_counter_contract_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/super-counter.religo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation())) (e_int (op 42 n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let super_counter_contract_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/super-counter.jsligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation())) (e_int (op 42 n)) in
  expect_eq_n ~raise program "main" make_input make_expected
  
let dispatch_counter_contract ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/dispatch-counter.ligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation())) (e_int (op 42 n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let failwith_ligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/failwith.ligo" in
  let should_fail = expect_fail ~raise program "main" in
  let should_work input = expect_eq ~raise program "main" input (e_pair (e_typed_list [] (t_operation())) (e_unit ())) in
  let _ = should_work (e_pair (e_constructor "Zero" (e_nat 0)) (e_unit ())) in
  let _ = should_fail (e_pair (e_constructor "Zero" (e_nat 1)) (e_unit ())) in
  let _ = should_work (e_pair (e_constructor "Pos" (e_nat 1)) (e_unit ())) in
  let _ = should_fail (e_pair (e_constructor "Pos" (e_nat 0)) (e_unit ())) in
  let should_fail input = expect_fail ~raise program "foobar" (e_int input) in
  let should_work input n = expect_eq ~raise program "foobar" (e_int input) (e_int n) in
  let () = should_fail 10 in
  let () = should_fail @@ -10 in
  let () = should_work 5 6 in
  ()

let failwith_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/failwith.mligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  expect_fail ~raise program "main" make_input

let failwith_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/failwith.religo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  expect_fail ~raise program "main" make_input

let failwith_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/failwith.jsligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  expect_fail ~raise program "main" make_input

let assert_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/assert.mligo" in
  let make_input b = e_pair (e_bool b) (e_unit ()) in
  let make_expected = e_pair (e_typed_list [] (t_operation())) (e_unit ()) in
  let _ = expect_fail ~raise program "main" (make_input false) in
  let _ = expect_eq ~raise program "main" (make_input true) make_expected in
  let _ = expect_fail ~raise program "some" (e_none ()) in
  let _ = expect_eq ~raise program "some" (e_some (e_unit ())) (e_unit ()) in
  ()

let assert_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/assert.religo" in
  let make_input b = e_pair (e_bool b) (e_unit ()) in
  let make_expected = e_pair (e_typed_list [] (t_operation())) (e_unit ()) in
  let _ = expect_fail ~raise program "main" (make_input false) in
  let _ = expect_eq ~raise program "main" (make_input true) make_expected in
  ()

let assert_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/assert.jsligo" in
  let make_input b = e_pair (e_bool b) (e_unit ()) in
  let make_expected = e_pair (e_typed_list [] (t_operation())) (e_unit ()) in
  let _ = expect_fail ~raise program "main" (make_input false) in
  let _ = expect_eq ~raise program "main" (make_input true) make_expected in
  ()
    
let recursion_ligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/recursion.ligo" in
  let _ =
    let make_input = e_pair (e_int 10) (e_int 0) in
    let make_expected = e_int 55 in
    expect_eq ~raise program "sum" make_input make_expected
  in
  let _ =
    let make_input = e_tuple [(e_int 10); (e_int 1); (e_int 1)] in
    let make_expected = e_int 89 in
    expect_eq ~raise program "fibo" make_input make_expected
  in ()


let recursion_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/recursion.mligo" in
  let _ =
    let make_input = e_pair (e_int 10) (e_int 0) in
    let make_expected = e_int 55 in
    expect_eq ~raise program "sum" make_input make_expected
  in
  let _ =
    let make_input = e_tuple [(e_int 10); (e_int 1); (e_int 1)] in
    let make_expected = e_int 89 in
    expect_eq ~raise program "fibo" make_input make_expected
  in ()

let recursion_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/recursion.religo" in
  let _ =
    let make_input = e_pair (e_int 10) (e_int 0) in
    let make_expected = e_int 55 in
    expect_eq ~raise program "sum" make_input make_expected
  in
  let _ =
    let make_input = e_tuple [(e_int 10); (e_int 1); (e_int 1)] in
    let make_expected = e_int 89 in
    expect_eq ~raise program "fibo" make_input make_expected
  in ()

let recursion_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/recursion.jsligo" in
  let _ =
    let make_input = e_pair (e_int 10) (e_int 0) in
    let make_expected = e_int 55 in
    expect_eq ~raise program "sum" make_input make_expected
  in
  let _ =
    let make_input = e_tuple [(e_int 10); (e_int 1); (e_int 1)] in
    let make_expected = e_int 89 in
    expect_eq ~raise program "fibo" make_input make_expected
  in ()
  

let guess_string_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/guess_string.mligo" in
  let make_input = fun n -> e_pair (e_int n) (e_int 42) in
  let make_expected = fun n -> e_pair (e_typed_list [] (t_operation())) (e_int (42 + n))
  in expect_eq_n ~raise program "main" make_input make_expected

let basic_mligo ~add_warning ~raise () : unit =
  let typed = type_file_w ~raise "./contracts/basic.mligo" in
  expect_eq_evaluate ~raise typed "foo" (e_int (42+127))

let basic_religo ~add_warning ~raise () : unit =
  let typed = type_file_w ~raise "./contracts/basic.religo" in
  expect_eq_evaluate ~raise typed "foo" (e_int (42+127))

let counter_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/counter.mligo" in
  let make_input n = e_pair (e_int n) (e_int 42) in
  let make_expected n = e_pair (e_typed_list [] (t_operation ())) (e_int (42 + n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let counter_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/counter.religo" in
  let make_input n = e_pair (e_int n) (e_int 42) in
  let make_expected n = e_pair (e_typed_list [] (t_operation ())) (e_int (42 + n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let counter_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/counter.jsligo" in
  let make_input n = e_pair (e_int n) (e_int 42) in
  let make_expected n = e_pair (e_typed_list [] (t_operation ())) (e_int (42 + n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let let_in_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/letin.mligo" in
  let () =
    let make_input n = e_pair (e_int n) (e_pair (e_int 3) (e_int 5)) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ())) (e_pair (e_int (7+n)) (e_int (3+5)))
    in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let () =
    expect_eq ~raise program "letin_nesting" (e_unit ()) (e_string "test")
  in
  let () =
    expect_eq ~raise program "letin_nesting2" (e_int 4) (e_int 9)
  in
  ()

let let_in_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/letin.religo" in
  let () =
    let make_input n = e_pair (e_int n) (e_pair (e_int 3) (e_int 5)) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ())) (e_pair (e_int (7+n)) (e_int (3+5)))
    in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let () =
    expect_eq ~raise program "letin_nesting" (e_unit ()) (e_string "test")
  in
  let () =
    expect_eq ~raise program "letin_nesting2" (e_int 4) (e_int 9)
  in
  ()

let let_in_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/letin.jsligo" in
  let () =
    let make_input n = e_pair (e_int n) (e_pair (e_int 3) (e_int 5)) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ())) (e_pair (e_int (7+n)) (e_int (3+5)))
    in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let () =
    expect_eq ~raise program "letin_nesting" (e_unit ()) (e_string "test")
  in
  let () =
    expect_eq ~raise program "letin_nesting2" (e_int 4) (e_int 9)
  in
  ()
  

let local_type_decl ~raise program : unit =
  let () =
    expect_eq ~raise program "local_type" (e_unit ()) (e_int 3)
  in
  ()

let local_type_decl_ligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/local_type_decl.ligo" in
  local_type_decl ~raise program

let local_type_decl_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/local_type_decl.mligo" in
  local_type_decl ~raise program

let local_type_decl_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/local_type_decl.religo" in
  local_type_decl ~raise program

let local_type_decl_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/local_type_decl.jsligo" in
  local_type_decl ~raise program  

let match_variant ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/match.mligo" in
  let () =
    let make_input n =
      e_pair (e_constructor "Sub" (e_int n)) (e_int 3) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n ~raise program "main" make_input make_expected in
  let () =
    let input = e_bool true in
    let expected = e_int 10 in
    expect_eq ~raise program "match_bool" input expected in
  let () =
    let input = e_bool false in
    let expected = e_int 0 in
    expect_eq ~raise program "match_bool" input expected in
  let () =
    let input = e_list [e_int 3] in
    let expected = e_int 3 in
    expect_eq ~raise program "match_list" input expected in
  let () =
    let input = e_typed_list [] (t_int ()) in
    let expected = e_int 10 in
    expect_eq ~raise program "match_list" input expected in
  let () =
    let make_input n = e_some (e_int n) in
    let make_expected n = e_int n in
    expect_eq_n ~raise program "match_option" make_input make_expected in
  ()

let match_variant_re ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/match.religo" in
  let make_input n =
    e_pair (e_constructor "Sub" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n ~raise program "main" make_input make_expected

let match_variant_js ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/match.jsligo" in
  let make_input n =
    e_pair (e_constructor "Sub" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n ~raise program "main" make_input make_expected
  

let match_matej ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/match_bis.mligo" in
  let make_input n =
    e_pair (e_constructor "Decrement" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n ~raise program "main" make_input make_expected

let match_matej_re ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/match_bis.religo" in
  let make_input n =
    e_pair (e_constructor "Decrement" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n ~raise program "main" make_input make_expected

let match_matej_js ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/match_bis.jsligo" in
  let make_input n =
    e_pair (e_constructor "Decrement" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n ~raise program "main" make_input make_expected
  

let mligo_list ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/list.mligo" in
  let () = expect_eq ~raise program "size_" (e_list [e_int 0; e_int 1; e_int 2]) (e_nat 3) in
  let aux lst = e_list @@ List.map ~f:e_int lst in
  let () = expect_eq ~raise program "fold_op" (aux [ 1 ; 2 ; 3 ]) (e_int 16) in
  let () = expect_eq ~raise program "fold_left"  (aux [ 1 ; 2 ; 3 ]) (aux [ 3 ; 2 ; 1 ]) in
  let () = expect_eq ~raise program "fold_right" (aux [ 1 ; 2 ; 3 ]) (aux [ 1 ; 2 ; 3 ]) in
  let () =
    let make_input n =
      e_pair (e_list [e_int n; e_int (2*n)])
        (e_pair (e_int 3) (e_list [e_int 8])) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ()))
        (e_pair (e_int (n+3)) (e_list [e_int (2*n)]))
    in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let () = expect_eq_evaluate ~raise program "x" (e_list []) in
  let () = expect_eq_evaluate ~raise program "y" (e_list @@ List.map ~f:e_int [3 ; 4 ; 5]) in
  let () = expect_eq_evaluate ~raise program "z" (e_list @@ List.map ~f:e_int [2 ; 3 ; 4 ; 5]) in
  let () = expect_eq ~raise program "map_op" (aux [2 ; 3 ; 4 ; 5]) (aux [3 ; 4 ; 5 ; 6]) in
  let () = expect_eq ~raise program "iter_op" (aux [2 ; 3 ; 4 ; 5]) (e_unit ()) in
  ()

let religo_list ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/list.religo" in
  let () = expect_eq ~raise program "size_" (e_list [e_int 0; e_int 1; e_int 2]) (e_nat 3) in
  let aux lst = e_list @@ List.map ~f:e_int lst in
  let () = expect_eq ~raise program "fold_op" (aux [ 1 ; 2 ; 3 ]) (e_int 16) in
  let () =
    let make_input n =
      e_pair (e_list [e_int n; e_int (2*n)])
        (e_pair (e_int 3) (e_list [e_int 8])) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ()))
        (e_pair (e_int (n+3)) (e_list [e_int (2*n)]))
    in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let () = expect_eq_evaluate ~raise program "x" (e_list []) in
  let () = expect_eq_evaluate ~raise program "y" (e_list @@ List.map ~f:e_int [3 ; 4 ; 5]) in
  let () = expect_eq_evaluate ~raise program "z" (e_list @@ List.map ~f:e_int [2 ; 3 ; 4 ; 5]) in
  let () = expect_eq ~raise program "map_op" (aux [2 ; 3 ; 4 ; 5]) (aux [3 ; 4 ; 5 ; 6]) in
  let () = expect_eq ~raise program "iter_op" (aux [2 ; 3 ; 4 ; 5]) (e_unit ()) in
  ()

let jsligo_list ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/list.jsligo" in
  let () = expect_eq ~raise program "size_" (e_list [e_int 0; e_int 1; e_int 2]) (e_nat 3) in
  let aux lst = e_list @@ List.map ~f:e_int lst in
  let () = expect_eq ~raise program "fold_op" (aux [ 1 ; 2 ; 3 ]) (e_int 16) in
  let () =
    let make_input n =
      e_pair (e_list [e_int n; e_int (2*n)])
        (e_pair (e_int 3) (e_list [e_int 8])) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ()))
        (e_pair (e_int (n+3)) (e_list [e_int (2*n)]))
    in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let () = expect_eq_evaluate ~raise program "x" (e_list []) in
  let () = expect_eq_evaluate ~raise program "y" (e_list @@ List.map ~f:e_int [3 ; 4 ; 5]) in
  let () = expect_eq_evaluate ~raise program "z" (e_list @@ List.map ~f:e_int [2 ; 3 ; 4 ; 5]) in
  let () = expect_eq ~raise program "map_op" (aux [2 ; 3 ; 4 ; 5]) (aux [3 ; 4 ; 5 ; 6]) in
  let () = expect_eq ~raise program "iter_op" (aux [2 ; 3 ; 4 ; 5]) (e_unit ()) in
  ()
  

let lambda_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/lambda.mligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq ~raise program "main" make_input make_expected

let lambda_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/lambda.religo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq ~raise program "main" make_input make_expected

let lambda_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/lambda.jsligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq ~raise program "main" make_input make_expected

let lambda_ligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/lambda.ligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq ~raise program "main" make_input make_expected

let lambda2_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/lambda2.mligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq ~raise program "main" make_input make_expected

let lambda2_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/lambda2.religo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq ~raise program "main" make_input make_expected

let lambda2_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/lambda2.jsligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq ~raise program "main" make_input make_expected
  

let fibo_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/fibo.mligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_int 42) in
  expect_eq ~raise program "main" make_input make_expected

let michelson_insertion ~raise program : unit =
  let program = program in
  let make_input = fun n -> e_pair (e_nat n) (e_nat 1) in
  let make_expected = fun n -> e_nat (n+1) in
  expect_eq_n_pos ~raise program "michelson_add" make_input make_expected

let michelson_insertion_ligo ~add_warning ~raise () : unit =
  michelson_insertion ~raise @@ type_file_w ~raise "./contracts/michelson_insertion.ligo"

let michelson_insertion_mligo ~add_warning ~raise () : unit =
  michelson_insertion ~raise @@ type_file_w ~raise "./contracts/michelson_insertion.mligo"

let michelson_insertion_religo ~add_warning ~raise () : unit =
  michelson_insertion ~raise @@ type_file_w ~raise "./contracts/michelson_insertion.religo"

let michelson_insertion_jsligo ~add_warning ~raise () : unit =
  michelson_insertion ~raise @@ type_file_w ~raise "./contracts/michelson_insertion.jsligo"

let website1_ligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/website1.ligo" in
  let make_input = fun n-> e_pair (e_int n) (e_int 42) in
  let make_expected = fun _n -> e_pair (e_typed_list [] (t_operation ())) (e_int (42 + 1)) in
  expect_eq_n ~raise program "main" make_input make_expected

let website2_ligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/website2.ligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation ())) (e_int (op 42 n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let tez_ligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/tez.ligo" in
  let _ = expect_eq_evaluate ~raise program "add_tez" (e_mutez 42) in
  let _ = expect_eq_evaluate ~raise program "sub_tez" (e_mutez 1) in
  let _ = expect_eq_evaluate ~raise program "not_enough_tez" (e_mutez 4611686018427387903) in
  let _ = expect_eq_evaluate ~raise program "nat_mul_tez" (e_mutez 100) in
  let _ = expect_eq_evaluate ~raise program "tez_mul_nat" (e_mutez 1000) in
  let _ = expect_eq_evaluate ~raise program "tez_div_tez1" (e_nat 100) in
  let _ = expect_eq_evaluate ~raise program "tez_div_tez2" (e_nat 1) in
  let _ = expect_eq_evaluate ~raise program "tez_div_tez3" (e_nat 0) in
  let _ = expect_eq_evaluate ~raise program "tez_mod_tez1" (e_mutez 0) in
  let _ = expect_eq_evaluate ~raise program "tez_mod_tez2" (e_mutez 10) in
  let _ = expect_eq_evaluate ~raise program "tez_mod_tez3" (e_mutez 100) in
  ()

let tez_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/tez.mligo" in
  let _ = expect_eq_evaluate ~raise program "add_tez" (e_mutez 42) in
  let _ = expect_eq_evaluate ~raise program "sub_tez" (e_mutez 1) in
  let _ = expect_eq_evaluate ~raise program "not_enough_tez" (e_mutez 4611686018427387903) in
  let _ = expect_eq_evaluate ~raise program "add_more_tez" (e_mutez 111111000) in
  ()

let website2_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/website2.mligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation ())) (e_int (op 42 n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let website2_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/website2.religo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation ())) (e_int (op 42 n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let website2_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/website2.jsligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation ())) (e_int (op 42 n)) in
  expect_eq_n ~raise program "main" make_input make_expected
  
let mligo_let_multiple ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/let_multiple.mligo" in
  let () =
    let input = e_unit () in
    let expected = e_int 3 in
    expect_eq ~raise program "main" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_int 6 in
    expect_eq ~raise program "main_paren" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_tuple [e_int 23 ; e_int 42] in
    expect_eq ~raise program "correct_values_bound" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_int 19 in
    expect_eq ~raise program "non_tuple_rhs" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_tuple [e_int 10; e_int 20; e_int 30; e_int 40; e_int 50] in
    expect_eq ~raise program "correct_values_big_tuple" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_tuple [e_int 10 ; e_string "hello"] in
    expect_eq ~raise program "correct_values_different_types" input expected
  in
  ()

let religo_let_multiple ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/let_multiple.religo" in
  let () =
    let input = e_unit () in
    let expected = e_int 3 in
    expect_eq ~raise program "main" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_int 6 in
    expect_eq ~raise program "main_paren" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_int 65 in
    expect_eq ~raise program "non_tuple_rhs" input expected
  in
  ()

let jsligo_let_multiple ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/let_multiple.jsligo" in
  let () =
    let input = e_unit () in
    let expected = e_int 3 in
    expect_eq ~raise program "main" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_int 6 in
    expect_eq ~raise program "main_paren" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_int 65 in
    expect_eq ~raise program "non_tuple_rhs" input expected
  in
  ()
  

let balance_test_options ~raise () =
  let balance = trace_option ~raise (test_internal "could not convert balance") @@
    Memory_proto_alpha.Protocol.Alpha_context.Tez.of_string "4000000" in
  Proto_alpha_utils.Memory_proto_alpha.make_options ~balance ()

let balance_constant ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/balance_constant.ligo" in
  let input = e_tuple [e_unit () ; e_mutez 0]  in
  let expected = e_tuple [e_list []; e_mutez 4000000000000] in
  let options = balance_test_options ~raise () in
  expect_eq ~raise ~options program "main" input expected


let balance_constant_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/balance_constant.mligo" in
  let input = e_tuple [e_unit () ; e_mutez 0]  in
  let expected = e_tuple [e_list []; e_mutez 4000000000000] in
  let options = balance_test_options ~raise () in
  expect_eq ~raise ~options program "main" input expected

let balance_constant_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/balance_constant.religo" in
  let input = e_tuple [e_unit () ; e_mutez 0]  in
  let expected = e_tuple [e_list []; e_mutez 4000000000000] in
  let options = balance_test_options ~raise () in
  expect_eq ~raise ~options program "main" input expected

let balance_constant_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/balance_constant.jsligo" in
  let input = e_tuple [e_unit () ; e_mutez 0]  in
  let expected = e_tuple [e_list []; e_mutez 4000000000000] in
  let options = balance_test_options ~raise () in
  expect_eq ~raise ~options program "main" input expected
  

let amount ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/amount.ligo" in
  let input = e_unit () in
  let expected = e_int 42 in
  let amount =
    match Memory_proto_alpha.Protocol.Alpha_context.Tez.of_string "100" with
    | Some t -> t
    | None -> Memory_proto_alpha.Protocol.Alpha_context.Tez.one
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount () in
  expect_eq ~raise ~options program "check" input expected

let amount_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/amount.mligo" in
  let input = e_unit () in
  let expected = e_int 42 in
  let amount =
    match Memory_proto_alpha.Protocol.Alpha_context.Tez.of_string "100" with
    | Some t -> t
    | None -> Memory_proto_alpha.Protocol.Alpha_context.Tez.one
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount () in
  expect_eq ~raise ~options program "check_" input expected

let amount_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/amount.religo" in
  let input = e_unit () in
  let expected = e_int 42 in
  let amount =
    match Memory_proto_alpha.Protocol.Alpha_context.Tez.of_string "100" with
    | Some t -> t
    | None -> Memory_proto_alpha.Protocol.Alpha_context.Tez.one
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount () in
  expect_eq ~raise ~options program "check_" input expected

let amount_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/amount.jsligo" in
  let input = e_unit () in
  let expected = e_int 42 in
  let amount =
    match Memory_proto_alpha.Protocol.Alpha_context.Tez.of_string "100" with
    | Some t -> t
    | None -> Memory_proto_alpha.Protocol.Alpha_context.Tez.one
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount () in
  expect_eq ~raise ~options program "check_" input expected
  

let addr_test ~raise program =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let addr = Protocol.Alpha_context.Contract.to_b58check @@
      (List.nth_exn dummy_environment.identities 0).implicit_contract in
  let open Tezos_crypto in
  let key_hash = Signature.Public_key_hash.to_b58check @@
      (List.nth_exn dummy_environment.identities 0).public_key_hash in
  expect_eq ~raise program "main" (e_key_hash key_hash) (e_address addr)

let address ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/address.ligo" in
  addr_test ~raise program

let address_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/address.mligo" in
  addr_test ~raise program

let address_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/address.religo" in
  addr_test ~raise program

let address_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/address.jsligo" in
  addr_test ~raise program
  

let self_address ~add_warning ~raise () : unit =
  let _ = type_file_w ~raise "./contracts/self_address.ligo" in
  ()

let self_address_mligo ~add_warning ~raise () : unit =
  let _ = type_file_w ~raise "./contracts/self_address.mligo" in
  ()

let self_address_religo ~add_warning ~raise () : unit =
  let _ = type_file_w ~raise "./contracts/self_address.religo" in
  ()

let self_address_jsligo ~add_warning ~raise () : unit =
  let _ = type_file_w ~raise "./contracts/self_address.jsligo" in
  ()

let implicit_account ~add_warning ~raise () : unit =
  let _ = type_file_w ~raise "./contracts/implicit_account.ligo" in
  ()

let implicit_account_mligo ~add_warning ~raise () : unit =
  let _ = type_file_w ~raise "./contracts/implicit_account.mligo" in
  ()


let implicit_account_religo ~add_warning ~raise () : unit =
  let _ = type_file_w ~raise "./contracts/implicit_account.religo" in
  ()

let implicit_account_jsligo ~add_warning ~raise () : unit =
  let _ = type_file_w ~raise "./contracts/implicit_account.jsligo" in
  ()
  

let tuples_sequences_functions_religo ~add_warning ~raise () : unit =
  let _ = type_file_w ~raise "./contracts/tuples_sequences_functions.religo" in
  ()

let tuples_sequences_functions_jsligo ~add_warning ~raise () : unit =
  let _ = type_file_w ~raise "./contracts/tuples_sequences_functions.jsligo" in
  ()
  

let is_nat ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/isnat.ligo" in
  let () =
    let input = e_int 10 in
    let expected = e_some (e_nat 10) in
    expect_eq ~raise program "main" input expected
  in
  let () =
    let input = e_int (-10) in
    let expected = e_none () in
    expect_eq ~raise program "main" input expected
  in ()

let is_nat_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/isnat.mligo" in
  let () =
    let input = e_int 10 in
    let expected = e_some (e_nat 10) in
    expect_eq ~raise program "main" input expected
  in
  let () =
    let input = e_int (-10) in
    let expected = e_none () in
    expect_eq ~raise program "main" input expected
  in ()

let is_nat_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/isnat.religo" in
  let () =
    let input = e_int 10 in
    let expected = e_some (e_nat 10) in
    expect_eq ~raise program "main" input expected
  in
  let () =
    let input = e_int (-10) in
    let expected = e_none () in
    expect_eq ~raise program "main" input expected
  in ()

let is_nat_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/isnat.jsligo" in
  let () =
    let input = e_int 10 in
    let expected = e_some (e_nat 10) in
    expect_eq ~raise program "main" input expected
  in
  let () =
    let input = e_int (-10) in
    let expected = e_none () in
    expect_eq ~raise program "main" input expected
  in ()

let simple_access_ligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/simple_access.ligo" in
  let make_input = e_tuple [e_int 0; e_int 1] in
  let make_expected = e_int 2 in
  expect_eq ~raise program "main" make_input make_expected

let deep_access_ligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/deep_access.ligo" in
  let () =
    let make_input = e_unit () in
    let make_expected = e_int 2 in
    expect_eq ~raise program "main" make_input make_expected in
  let () =
    let make_input = e_unit () in
    let make_expected = e_int 6 in
    expect_eq ~raise program "asymetric_tuple_access" make_input make_expected in
  let () =
    let make_input = e_record_ez [ ("nesty",
      e_record_ez [ ("mymap", e_typed_map [] (t_int ()) (t_string ())) ] ) ; ] in
    let make_expected = e_string "one" in
    expect_eq ~raise program "nested_record" make_input make_expected in
  ()

let attributes_ligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/attributes.ligo" in
  let () =
    let input = e_int 3 in
    let expected = e_int 5 in
    expect_eq ~raise program "foo" input expected
  in
  ()

let attributes_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/attributes.mligo" in
  let () =
    let input = e_int 3 in
    let expected = e_int 5 in
    expect_eq ~raise program "foo" input expected
  in
  ()

let attributes_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/attributes.religo" in
  let () =
    let input = e_int 3 in
    let expected = e_int 5 in
    expect_eq ~raise program "foo" input expected
  in
  ()

let attributes_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/attributes.jsligo" in
  let () =
    let input = e_int 3 in
    let expected = e_int 5 in
    expect_eq ~raise program "foo" input expected
  in
  ()
  

let get_contract_ligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/get_contract.ligo" in
  let () =
    let make_input = fun _n -> e_unit () in
    let make_expected : int -> Ast_core.expression -> unit = fun _n result ->
      let (ops , storage) = trace_option ~raise (test_internal __LOC__) @@ Ast_core.get_e_pair result.expression_content in
      let () =
        let lst = trace_option ~raise (test_internal __LOC__) @@ Ast_core.get_e_list ops.expression_content in
        Assert.assert_list_size ~raise (test_internal __LOC__) lst 1 in
      let expected_storage = Ast_core.e_unit () in
      trace_option ~raise (test_internal __LOC__) @@ Ast_core.Misc.assert_value_eq (expected_storage , storage)
      in
    let () =
      let amount = Memory_proto_alpha.Protocol.Alpha_context.Tez.zero in
      let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount () in
      let () = expect_n_strict_pos_small ~raise ~options program "cb" make_input make_expected in
      expect_n_strict_pos_small ~raise ~options program "cbo" make_input make_expected in
    ()
  in
  ()

let entrypoints_ligo ~add_warning ~raise () : unit =
  let _program = type_file_w ~raise "./contracts/entrypoints.ligo" in
  (* hmm... *)
  ()

let simple1 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/simple1.ligo" in
  expect_eq_evaluate ~raise program "i" (e_int 42)

let simple2 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/simple2.ligo" in
  expect_eq_evaluate ~raise program "i" (e_int 42)

let simple3 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/simple3.ligo" in
  expect_eq_evaluate ~raise program "my_address" (e_address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")

let simple4 ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/simple4.ligo" in
  expect_eq_evaluate ~raise program "my_string_option" (e_string "hello")

let chain_id ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/chain_id.ligo" in
  let pouet = Tezos_crypto.Base58.simple_encode
    Tezos_base__TzPervasives.Chain_id.b58check_encoding
    Tezos_base__TzPervasives.Chain_id.zero in
  let make_input = e_chain_id pouet in
  let make_expected = e_chain_id pouet in
  let () = expect_eq ~raise program "chain_id" make_input make_expected in
  ()

let key_hash ~add_warning ~raise () : unit =
  let open Tezos_crypto in
  let (raw_pkh,raw_pk,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let program = type_file_w ~raise "./contracts/key_hash.ligo" in
  let make_input = e_pair (e_key_hash pkh_str) (e_key pk_str) in
  let make_expected = e_pair (e_bool true) (e_key_hash pkh_str) in
  let () = expect_eq ~raise program "check_hash_key" make_input make_expected in
  ()

let key_hash_mligo ~add_warning ~raise () : unit =
  let open Tezos_crypto in
  let (raw_pkh,raw_pk,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let program = type_file_w ~raise "./contracts/key_hash.mligo" in
  let make_input = e_pair (e_key_hash pkh_str) (e_key pk_str) in
  let make_expected = e_pair (e_bool true) (e_key_hash pkh_str) in
  let () = expect_eq ~raise program "check_hash_key" make_input make_expected in
  ()

let key_hash_religo ~add_warning ~raise () : unit =
  let open Tezos_crypto in
  let (raw_pkh,raw_pk,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let program = type_file_w ~raise "./contracts/key_hash.religo" in
  let make_input = e_pair (e_key_hash pkh_str) (e_key pk_str) in
  let make_expected = e_pair (e_bool true) (e_key_hash pkh_str) in
  let () = expect_eq ~raise program "check_hash_key" make_input make_expected in
  ()

let key_hash_jsligo ~add_warning ~raise () : unit =
  let open Tezos_crypto in
  let (raw_pkh,raw_pk,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let program = type_file_w ~raise "./contracts/key_hash.jsligo" in
  let make_input = e_pair (e_key_hash pkh_str) (e_key pk_str) in
  let make_expected = e_pair (e_bool true) (e_key_hash pkh_str) in
  let () = expect_eq ~raise program "check_hash_key" make_input make_expected in
  ()
  

let check_signature ~add_warning ~raise () : unit =
  let open Tezos_crypto in
  let (_, raw_pk, sk) = Signature.generate_key () in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let signed = Signature.sign sk (Bytes.of_string "hello world") in
  let program = type_file_w ~raise "./contracts/check_signature.ligo" in
  let make_input = e_tuple [e_key pk_str ;
                            e_signature (Signature.to_b58check signed) ;
                            e_bytes_string "hello world"] in
  let make_expected = e_bool true in
  let () = expect_eq ~raise program "check_signature" make_input make_expected in
  ()

let check_signature_mligo ~add_warning ~raise () : unit =
  let open Tezos_crypto in
  let (_, raw_pk, sk) = Signature.generate_key () in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let signed = Signature.sign sk (Bytes.of_string "hello world") in
  let program = type_file_w ~raise "./contracts/check_signature.mligo" in
  let make_input = e_tuple [e_key pk_str ;
                            e_signature (Signature.to_b58check signed) ;
                            e_bytes_string "hello world"] in
  let make_expected = e_bool true in
  let () = expect_eq ~raise program "check_signature" make_input make_expected in
  let () = expect_eq_evaluate ~raise program "example" (e_bool true) in
  ()

let check_signature_religo ~add_warning ~raise () : unit =
  let open Tezos_crypto in
  let (_, raw_pk, sk) = Signature.generate_key () in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let signed = Signature.sign sk (Bytes.of_string "hello world") in
  let program = type_file_w ~raise "./contracts/check_signature.religo" in
  let make_input = e_tuple [e_key pk_str ;
                            e_signature (Signature.to_b58check signed) ;
                            e_bytes_string "hello world"] in
  let make_expected = e_bool true in
  let () = expect_eq ~raise program "check_signature" make_input make_expected in
  ()

let check_signature_jsligo ~add_warning ~raise () : unit =
  let open Tezos_crypto in
  let (_, raw_pk, sk) = Signature.generate_key () in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let signed = Signature.sign sk (Bytes.of_string "hello world") in
  let program = type_file_w ~raise "./contracts/check_signature.jsligo" in
  let make_input = e_tuple [e_key pk_str ;
                            e_signature (Signature.to_b58check signed) ;
                            e_bytes_string "hello world"] in
  let make_expected = e_bool true in
  let () = expect_eq ~raise program "check_signature" make_input make_expected in
  ()
  

let curry ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/curry.mligo" in
  let () =
    expect_eq ~raise program "main" (e_int 2) (e_int 12)
  in
  let () =
    expect_eq ~raise program "partial_apply" (e_int 2) (e_int 12)
  in
  ()

let set_delegate ~add_warning ~raise () : unit =
  let open Tezos_crypto in
  let (raw_pkh,_,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let program = type_file_w ~raise "./contracts/set_delegate.ligo" in
  let () = expect_eq ~raise program "main" (e_key_hash pkh_str) (e_typed_list [] (t_operation ()))
  in ()

let set_delegate_mligo ~add_warning ~raise () : unit =
  let open Tezos_crypto in
  let (raw_pkh,_,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let program = type_file_w ~raise "./contracts/set_delegate.mligo" in
  let () = expect_eq ~raise program "main" (e_key_hash pkh_str) (e_typed_list [] (t_operation ()))
  in ()

let set_delegate_religo ~add_warning ~raise () : unit =
  let open Tezos_crypto in
  let (raw_pkh,_,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let program = type_file_w ~raise "./contracts/set_delegate.religo" in
  let () = expect_eq ~raise program "main" (e_key_hash pkh_str) (e_typed_list [] (t_operation ()))
  in ()

let set_delegate_jsligo ~add_warning ~raise () : unit =
  let open Tezos_crypto in
  let (raw_pkh,_,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let program = type_file_w ~raise "./contracts/set_delegate.jsligo" in
  let () = expect_eq ~raise program "main" (e_key_hash pkh_str) (e_typed_list [] (t_operation ()))
  in ()
  

let type_tuple_destruct ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/type_tuple_destruct.mligo" in
  let () = expect_eq ~raise program "type_tuple_d" (e_unit ()) (e_int 35) in
  let () = expect_eq ~raise program "type_tuple_d_2" (e_unit ()) (e_string "helloworld") in
  ()

let tuple_param_destruct ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/tuple_param_destruct.mligo" in
  let () = expect_eq ~raise program "sum" (e_tuple [e_int 20; e_int 10]) (e_int 10) in
  let () = expect_eq ~raise program "parentheses" (e_tuple [e_int 20; e_int 10]) (e_int 10) in
  ()

let tuple_param_destruct_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/tuple_param_destruct.religo" in
  let () = expect_eq ~raise program "sum" (e_tuple [e_int 20; e_int 10]) (e_int 10) in
  let () = expect_eq ~raise program "parentheses" (e_tuple [e_int 20; e_int 10]) (e_int 10) in
  ()

let let_in_multi_bind ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/let_in_multi_bind.mligo" in
  let () = expect_eq ~raise program "sum" (e_tuple [e_int 10; e_int 10]) (e_int 20) in
  let () = expect_eq ~raise program "sum2"
      (e_tuple
         [e_string "my" ;
          e_string "name" ;
          e_string "is" ;
          e_string "bob" ])
      (e_string "mynameisbob")
  in ()

let bytes_unpack ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/bytes_unpack.ligo" in
  let () = expect_eq ~raise program "id_string" (e_string "teststring") (e_some (e_string "teststring")) in
  let () = expect_eq ~raise program "id_int" (e_int 42) (e_some (e_int 42)) in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let addr = Protocol.Alpha_context.Contract.to_b58check @@
      (List.nth_exn dummy_environment.identities 0).implicit_contract in
  let () = expect_eq ~raise program "id_address" (e_address addr) (e_some (e_address addr)) in
  ()

let bytes_unpack_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/bytes_unpack.mligo" in
  let () = expect_eq ~raise program "id_string" (e_string "teststring") (e_some (e_string "teststring")) in
  let () = expect_eq ~raise program "id_int" (e_int 42) (e_some (e_int 42)) in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let addr = Protocol.Alpha_context.Contract.to_b58check @@
      (List.nth_exn dummy_environment.identities 0).implicit_contract in
  let () = expect_eq ~raise program "id_address" (e_address addr) (e_some (e_address addr)) in
  ()

let bytes_unpack_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/bytes_unpack.religo" in
  let () = expect_eq ~raise program "id_string" (e_string "teststring") (e_some (e_string "teststring")) in
  let () = expect_eq ~raise program "id_int" (e_int 42) (e_some (e_int 42)) in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let addr = Protocol.Alpha_context.Contract.to_b58check @@
      (List.nth_exn dummy_environment.identities 0).implicit_contract in
  let () = expect_eq ~raise program "id_address" (e_address addr) (e_some (e_address addr)) in
  ()

let bytes_unpack_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/bytes_unpack.jsligo" in
  let () = expect_eq ~raise program "id_string" (e_string "teststring") (e_some (e_string "teststring")) in
  let () = expect_eq ~raise program "id_int" (e_int 42) (e_some (e_int 42)) in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let addr = Protocol.Alpha_context.Contract.to_b58check @@
      (List.nth_exn dummy_environment.identities 0).implicit_contract in
  let () = expect_eq ~raise program "id_address" (e_address addr) (e_some (e_address addr)) in
  ()

let empty_case ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/empty_case.ligo" in
  let () =
    let input _ = e_constructor "Bar" (e_int 1) in
    let expected _ = e_int 1 in
    expect_eq_n ~raise program "main" input expected
  in
  let () =
    let input _ = e_constructor "Baz" (e_unit ()) in
    let expected _ = e_int (-1) in
    expect_eq_n ~raise program "main" input expected
  in
  ()

let empty_case_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/empty_case.mligo" in
  let () =
    let input _ = e_constructor "Bar" (e_int 1) in
    let expected _ = e_int 1 in
    expect_eq_n ~raise program "main" input expected
  in
  let () =
    let input _ = e_constructor "Baz" (e_unit ()) in
    let expected _ = e_int (-1) in
    expect_eq_n ~raise program "main" input expected
  in
  ()

let empty_case_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/empty_case.religo" in
  let () =
    let input _ = e_constructor "Bar" (e_int 1) in
    let expected _ = e_int 1 in
    expect_eq_n ~raise program "main" input expected
  in
  let () =
    let input _ = e_constructor "Baz" (e_unit ()) in
    let expected _ = e_int (-1) in
    expect_eq_n ~raise program "main" input expected
  in
  ()

let empty_case_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/empty_case.jsligo" in
  let () =
    let input _ = e_constructor "Bar" (e_int 1) in
    let expected _ = e_int 1 in
    expect_eq_n ~raise program "main" input expected
  in
  let () =
    let input _ = e_constructor "Baz" (e_unit ()) in
    let expected _ = e_int (-1) in
    expect_eq_n ~raise program "main" input expected
  in
  ()
  

let tuple_type_mligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/tuple_type.mligo" in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "test1" input expected
  in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 12 in
    expect_eq_n ~raise program "test2" input expected
  in
  ()

let tuple_type_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/tuple_type.religo" in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "arguments_test" input expected
  in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "tuple_test" input expected
  in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "arguments_test_inline" input expected
  in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "tuple_test_inline" input expected
  in
  ()

let tuple_type_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/tuple_type.jsligo" in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "arguments_test" input expected
  in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "tuple_test" input expected
  in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "arguments_test_inline" input expected
  in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "tuple_test_inline" input expected
  in
  ()
  

let no_semicolon_religo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/no_semicolon.religo" in
  let () =
    let input _ = e_int 2 in
    let expected _ = e_int 3 in
    expect_eq_n ~raise program "a" input expected
  in
  ()

let no_semicolon_jsligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/no_semicolon.jsligo" in
  let () =
    let input _ = e_int 2 in
    let expected _ = e_int 3 in
    expect_eq_n ~raise program "a" input expected
  in
  ()

let tuple_list_religo ~add_warning ~raise () : unit =
  let _ = type_file_w ~raise "./contracts/tuple_list.religo" in
  ()

let tuple_list_jsligo ~add_warning ~raise () : unit =
  let _ = type_file_w ~raise "./contracts/tuple_list.jsligo" in
  ()

let single_record_expr_religo ~add_warning ~raise () : unit =
  let _ = type_file_w ~raise "./contracts/single_record_item.religo" in
  ()

let loop_bugs_ligo ~add_warning ~raise () : unit =
  let program = type_file_w ~raise "./contracts/loop_bugs.ligo" in
  let input = e_unit () in
  let () =
    let expected = e_string "tata" in
    expect_eq ~raise program "shadowing_in_body" input expected in
  let () =
    let expected = e_string "toto" in
    expect_eq ~raise program "shadowing_assigned_in_body" input expected in
  ()

let main = test_suite "Integration (End to End)"
  [
    test_ww "simple1" simple1 ;
    test_ww "simple2" simple2 ;
    test_ww "simple3" simple3 ;
    test_ww "chain id" chain_id ;
    test_ww "bytes unpack" bytes_unpack ;
    test_ww "bytes unpack (mligo)" bytes_unpack_mligo ;
    test_ww "bytes unpack (religo)" bytes_unpack_religo ; 
    test_ww "bytes unpack (jsligo)" bytes_unpack_jsligo ; 
    test_ww "key hash" key_hash ;
    test_ww "key hash (mligo)" key_hash_mligo ;
    test_ww "key hash (religo)" key_hash_religo ;
    test_ww "key hash (jsligo)" key_hash_jsligo ;
    test_ww "check signature" check_signature ;
    test_ww "check signature (mligo)" check_signature_mligo ;
    test_ww "check signature (religo)" check_signature_religo ;
    test_ww "check signature (jsligo)" check_signature_jsligo ;

    test_ww "type alias" type_alias ;

    test_ww "function" function_ ;                        (* tests don't typecheck the test case's application *)

    test_ww "blockless function" blockless;
    (* t_west "procedure"  procedure ; *)
    test_ww "assign" assign ;
    test_ww "declaration local" declaration_local ;
    test_ww "complex function" complex_function ;
    test_ww "anon function" anon_function ;

    test_ww "various applications" application ;

    test_ww "closure" closure ;
    test_ww "closure (mligo)" closure_mligo ;
    test_ww "closure (religo)" closure_religo ;
    test_ww "closure (jsligo)" closure_jsligo ;
    test_ww "shared function" shared_function ;
    test_ww "shared function (mligo)" shared_function_mligo ;
    test_ww "shared function (religo)" shared_function_religo ;
    test_ww "shared function (jsligo)" shared_function_jsligo ;
    test_ww "higher order" higher_order ;
    test_ww "higher order (mligo)" higher_order_mligo ;
    test_ww "higher order (religo)" higher_order_religo ;
    test_ww "higher order (jsligo)" higher_order_jsligo ;
    test_ww "variant" variant ;
    test_ww "variant (mligo)" variant_mligo ;
    test_ww "variant (religo)" variant_religo ;
    test_ww "variant (jsligo)" variant_jsligo ;

    test_ww "variant matching" variant_matching ;
    test_ww "tuple" tuple ;
    test_ww "tuple (mligo)" tuple_mligo ;
    test_ww "tuple (religo)" tuple_religo ;
    test_ww "tuple (jsligo)" tuple_jsligo ;
    test_ww "record" record ;
    test_ww "record (mligo)" record_mligo ;
    test_ww "record (religo)" record_religo ;
    test_ww "record (jsligo)" record_jsligo ;
    test_ww "condition simple" condition_simple ;
    test_ww "condition (ligo)" condition ;
    test_ww "condition (mligo)" condition_mligo ;
    test_ww "condition (religo)" condition_religo ;
    test_ww "condition (jsligo)" condition_jsligo ;
    test_ww "sequence (mligo" sequence_mligo ;
    test_ww "eq bool (ligo)" eq_bool ;
    test_ww "eq bool (mligo)" eq_bool_mligo ;
    test_ww "eq bool (religo)" eq_bool_religo ;
    test_ww "eq bool (jsligo)" eq_bool_jsligo ;
    test_ww "shadow" shadow ;
    test_ww "annotation" annotation ;

    test_ww "multiple parameters" multiple_parameters ;
    test_ww "multiple parameters (mligo)" multiple_parameters_mligo ;
    test_ww "multiple parameters (religo)" multiple_parameters_religo ;
    test_ww "multiple parameters (jsligo)" multiple_parameters_jsligo ;
    test_ww "bool" bool_expression ;
    test_ww "bool (mligo)" bool_expression_mligo ;
    test_ww "bool (religo)" bool_expression_religo ;
    test_ww "bool (jsligo)" bool_expression_jsligo ;
    test_ww "arithmetic" arithmetic ;
    test_ww "arithmetic (mligo)" arithmetic_mligo ;
    test_ww "arithmetic (religo)" arithmetic_religo ;
    test_ww "arithmetic (jsligo)" arithmetic_jsligo ;
    test_ww "bitwise_arithmetic" bitwise_arithmetic ;
    test_ww "bitwise_arithmetic (mligo)" bitwise_arithmetic_mligo;
    test_ww "bitwise_arithmetic (religo)" bitwise_arithmetic_religo;
    test_ww "bitwise_arithmetic (jsligo)" bitwise_arithmetic_jsligo;
    test_ww "string_arithmetic" string_arithmetic ;
    test_ww "string_arithmetic (mligo)" string_arithmetic_mligo ;
    test_ww "string_arithmetic (religo)" string_arithmetic_religo ;
    test_ww "bytes_arithmetic" bytes_arithmetic ;
    test_ww "bytes_arithmetic (mligo)" bytes_arithmetic_mligo ;
    test_ww "bytes_arithmetic (religo)" bytes_arithmetic_religo ;
    test_ww "bytes_arithmetic (jsligo)" bytes_arithmetic_jsligo ;
    test_ww "comparable (mligo)" comparable_mligo;
    test_ww "crypto" crypto ;
    test_ww "crypto (mligo)" crypto_mligo ;
    test_ww "crypto (religo)" crypto_religo ;
    test_ww "crypto (jsligo)" crypto_jsligo ;
    (* t_west "set_arithmetic" set_arithmetic ; *)
    test_ww "set_arithmetic (mligo)" set_arithmetic_mligo ;
    test_ww "set_arithmetic (religo)" set_arithmetic_religo ;
    test_ww "set_arithmetic (jsligo)" set_arithmetic_jsligo ;
    test_ww "unit" unit_expression ;
    test_ww "string" string_expression ;
    test_ww "option" option ;
    test_ww "option (mligo)" moption ;
    test_ww "option (religo)" reoption ;
    test_ww "option (jsligo)" jsoption ;

    test_ww "map" map ;
    test_ww "map (mligo)" mmap ;
    (* t_west "map (religo)" remap ; *)
    test_ww "map (jsligo)" jsmap;
    test_ww "big_map" big_map ;
    test_ww "big_map (mligo)" mbig_map ;
    test_ww "big_map (religo)" rebig_map ;
    test_ww "big_map (jsligo)" jsbig_map ;
    test_ww "list" list ;
    test_ww "loop1" loop1 ;
    test_ww "loop2" loop2 ;
    test_ww "loop3" loop3 ;
    test_ww "loop4" loop4 ;
    test_ww "loop5" loop5 ;
    test_ww "loop6" loop6 ;
    test_ww "loop7" loop7 ;
    test_ww "loop8" loop8 ;
    test_ww "loop9" loop9 ;
    test_ww "loop10" loop10 ;
    test_ww "loop11" loop11 ;
    test_ww "loop12" loop12 ;
    test_ww "loop13" loop13 ;
    test_ww "loop14" loop14 ;
    test_ww "loop15" loop15 ;
    test_ww "loop16" loop16 ;
    test_ww "loop17" loop17 ;
    test_ww "loop18" loop18 ;
    test_ww "loop" loop ;
    test_ww "loop (mligo)" loop_mligo ;
    test_ww "loop (religo)" loop_religo ;
    test_ww "loop (jsligo)" loop_jsligo ;
    test_ww "loop2 (jsligo)" loop2_jsligo ;

    test_ww "matching" matching ;
    test_ww "declarations" declarations ;
    test_ww "quote declaration" quote_declaration ;
    test_ww "quote declarations" quote_declarations ;

    test_ww "#include directives" include_ ;
    test_ww "#include directives (mligo)" include_mligo ;
    test_ww "#include directives (religo)" include_religo ;
    test_ww "#include directives (jsligo)" include_jsligo ;

    test_ww "counter contract" counter_contract ;
    test_ww "counter contract (mligo)" counter_mligo ;
    test_ww "counter contract (religo)" counter_religo ;
    test_ww "counter contract (jsligo)" counter_jsligo ;
    test_ww "super counter contract" super_counter_contract ;
    test_ww "super counter contract" super_counter_contract_mligo ;
    test_ww "super counter contract (reasonligo)" super_counter_contract_religo ;
    test_ww "super counter contract (jsligo)" super_counter_contract_jsligo ;
    test_ww "dispatch counter contract" dispatch_counter_contract ;
    test_ww "basic (mligo)" basic_mligo ;
    test_ww "basic (religo)" basic_religo ;

    test_ww "let-in (mligo)" let_in_mligo ;
    test_ww "let-in (religo)" let_in_religo ;
    test_ww "let-in (jsligo)" let_in_jsligo ;
    test_ww "local type declaration (ligo)" local_type_decl_ligo;
    test_ww "local type declaration (mligo)" local_type_decl_mligo;
    test_ww "local type declaration (religo)" local_type_decl_religo;
    test_ww "local type declaration (jsligo)" local_type_decl_jsligo;
    test_ww "match variant (mligo)" match_variant ;
    test_ww "match variant (religo)" match_variant_re ;
    test_ww "match variant (jsligo)" match_variant_js ;
    test_ww "match variant 2 (mligo)" match_matej ;
    test_ww "match variant 2 (religo)" match_matej_re ;
    test_ww "match variant 2 (jsligo)" match_matej_js ;
    test_ww "list matching (mligo)" mligo_list ;
    test_ww "list matching (religo)" religo_list ;
    test_ww "list matching (jsligo)" jsligo_list ;
    test_ww "failwith ligo" failwith_ligo ;
    test_ww "failwith jsligo" failwith_jsligo ;
    test_ww "failwith mligo" failwith_mligo ;
    test_ww "assert mligo" assert_mligo ;
    test_ww "assert jsligo" assert_jsligo ;
    test_ww "recursion (ligo)" recursion_ligo ;
    test_ww "recursion (mligo)" recursion_mligo ;
    test_ww "recursion (religo)" recursion_religo ;
    test_ww "recursion (jsligo)" recursion_jsligo ;
    (* t_west "guess string mligo" guess_string_mligo ; WIP? *)
    test_ww "lambda mligo" lambda_mligo ;
    test_ww "lambda religo" lambda_religo ;
    test_ww "lambda jsligo" lambda_jsligo ;
    test_ww "lambda ligo" lambda_ligo ;
    test_ww "tez (ligo)" tez_ligo ;
    test_ww "tez (mligo)" tez_mligo ;

    test_ww "lambda2 mligo" lambda2_mligo ;
    test_ww "lambda2 religo" lambda2_religo ;
    test_ww "lambda2 jsligo" lambda2_jsligo ;
    (* t_west "fibo (mligo)" fibo_mligo ; *)
    (* t_west "fibo2 (mligo)" fibo2_mligo ; *)
    (* t_west "fibo3 (mligo)" fibo3_mligo ; *)
    (* t_west "fibo4 (mligo)" fibo4_mligo ; *)
    test_ww "michelson inserion ligo" michelson_insertion_ligo;
    test_ww "michelson inserion mligo" michelson_insertion_mligo;
    test_ww "michelson inserion religo" michelson_insertion_religo;
    test_ww "michelson inserion jsligo" michelson_insertion_jsligo;
    test_ww "website1 ligo" website1_ligo ;
    test_ww "website2 ligo" website2_ligo ;
    test_ww "website2 (mligo)" website2_mligo ;
    test_ww "website2 (religo)" website2_religo ;
    test_ww "website2 (jsligo)" website2_jsligo ;
    test_ww "let multiple (mligo)" mligo_let_multiple ;
    test_ww "let multiple (religo)" religo_let_multiple ;
    test_ww "let multiple (jsligo)" jsligo_let_multiple ;
    test_ww "balance constant" balance_constant ;
    test_ww "balance constant (mligo)" balance_constant_mligo ;
    test_ww "balance constant (religo)" balance_constant_religo ;
    test_ww "balance constant (jsligo)" balance_constant_jsligo ;
    test_ww "amount" amount ;
    test_ww "amount (mligo)" amount_mligo ;
    test_ww "amount (religo)" amount_religo ;
    test_ww "amount (jsligo)" amount_jsligo ;
    test_ww "address" address ;
    test_ww "address (mligo)" address_mligo ;
    test_ww "address (religo)" address_religo ;
    test_ww "address (jsligo)" address_jsligo ;
    test_ww "self address" self_address ;
    test_ww "self address (mligo)" self_address_mligo ;
    test_ww "self address (religo)" self_address_religo ;
    test_ww "self address (jsligo)" self_address_jsligo ;
    test_ww "implicit account" implicit_account ;
    test_ww "implicit account (mligo)" implicit_account_mligo ;
    test_ww "implicit account (religo)" implicit_account_religo ;
    test_ww "implicit account (jsligo)" implicit_account_jsligo ;

    test_ww "set delegate" set_delegate ;
    test_ww "set delegate (mligo)" set_delegate_mligo ;
    test_ww "set delegate (religo)" set_delegate_religo ;
    test_ww "set delegate (jsligo)" set_delegate_jsligo ;
    test_ww "is_nat" is_nat ;
    test_ww "is_nat (mligo)" is_nat_mligo ;
    test_ww "is_nat (religo)" is_nat_religo ;
    test_ww "is_nat (jsligo)" is_nat_jsligo ;
    test_ww "tuples_sequences_functions (religo)" tuples_sequences_functions_religo ;
    test_ww "tuples_sequences_functions (jsligo)" tuples_sequences_functions_jsligo ;

    test_ww "simple_access (ligo)" simple_access_ligo;
    test_ww "deep_access (ligo)" deep_access_ligo;
    test_ww "get_contract (ligo)" get_contract_ligo;
    test_ww "entrypoints (ligo)" entrypoints_ligo ;

    test_ww "curry (mligo)" curry ;
    test_ww "type tuple destruct (mligo)" type_tuple_destruct ;
    test_ww "attributes (ligo)" attributes_ligo;
    test_ww "attributes (mligo)" attributes_mligo;
    test_ww "attributes (religo)" attributes_religo;
    test_ww "attributes (jsligo)" attributes_jsligo;
    test_ww "let in multi-bind (mligo)" let_in_multi_bind ;
    test_ww "tuple param destruct (mligo)" tuple_param_destruct ;
    test_ww "tuple param destruct (religo)" tuple_param_destruct_religo ;
    test_ww "empty case" empty_case ;
    test_ww "empty case (mligo)" empty_case_mligo ;
    test_ww "empty case (religo)" empty_case_religo ;
    test_ww "empty case (jsligo)" empty_case_jsligo ;
    test_ww "tuple type (mligo)" tuple_type_mligo ;
    test_ww "tuple type (religo)" tuple_type_religo ;
    test_ww "tuple type (jsligo)" tuple_type_jsligo ;
    test_ww "no semicolon (religo)" no_semicolon_religo ;
    test_ww "no semicolon (jsligo)" no_semicolon_jsligo ;
    test_ww "loop_bugs (ligo)" loop_bugs_ligo ;
    test_ww "tuple_list (religo)" tuple_list_religo ;
    test_ww "no semicolon (jsligo)" no_semicolon_jsligo ;
    test_ww "single_record_expr (religo)" single_record_expr_religo ;
    test_ww "shadowing (mligo)" shadowing;
  ]
