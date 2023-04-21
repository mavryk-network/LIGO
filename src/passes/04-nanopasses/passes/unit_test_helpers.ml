open Simple_utils.Trace
open Simple_utils.Function
open Ast_unified

let try_with f in_ =
  try_with
    (fun ~raise ~catch:_ ->
      let _v = f ~raise in_ in
      print_endline "This test should have failed")
    (fun ~catch:_ e ->
      Format.fprintf
        Format.std_formatter
        "Err : %a"
        Errors.(error_ppformat ~display_format:Dev ~no_colour:false)
        e)


module Dummies = struct
  type t = string * string

  let lst : t list =
    let loc = Location.generated in
    [ ( "<TY_EXPR>"
      , Format.asprintf "%a" PP.ty_expr (t_var ~loc (Ty_variable.of_input_var ~loc "x")) )
    ; ( "<EXPR>"
      , Format.asprintf "%a" PP.expr (e_variable ~loc (Variable.of_input_var ~loc "#")) )
    ; ( "<EXPR1>"
      , Format.asprintf "%a" PP.expr (e_variable ~loc (Variable.of_input_var ~loc "#1")) )
    ; ( "<EXPR2>"
      , Format.asprintf "%a" PP.expr (e_variable ~loc (Variable.of_input_var ~loc "#2")) )
    ]


  let in_output ((dummy, sexp) : t) = dummy, sexp
  let in_input ((dummy, sexp) : t) = sexp, dummy

  let replace : (t -> t) -> string -> string =
   fun direction init ->
    List.fold lst ~init ~f:(fun acc dummy ->
        let with_, pattern = direction dummy in
        String.substr_replace_all acc ~pattern ~with_)


  let replace_in_input = replace in_input
  let replace_in_output = replace in_output
end

let expected_failure_fwd_
    (selector : 'a Pass_type.Selector.t)
    (input : 'a)
    (pass : raise:_ -> Pass_type.pass)
    : unit
  =
  let f ~raise =
    let pass = pass ~raise in
    (selector pass).forward
  in
  try_with f input


let expected_failure_bwd_
    (selector : 'a Pass_type.Selector.t)
    (input : 'a)
    (pass : raise:_ -> Pass_type.pass)
    : unit
  =
  let f ~raise =
    let pass = pass ~raise in
    (selector pass).backward
  in
  try_with f input


let expected_sucess_fwd_
    (selector : 'a Pass_type.Selector.t)
    (input : 'a)
    (pass : Pass_type.pass)
    (to_str : 'a -> string)
    : unit
  =
  Format.printf "%s" (to_str @@ (selector pass).forward input)


let expected_sucess_bwd_
    (selector : 'a Pass_type.Selector.t)
    (input : 'a)
    (pass : Pass_type.pass)
    (to_str : 'a -> string)
    : unit
  =
  Format.printf "%s" (to_str @@ (selector pass).backward input)


module type S = sig
  type a

  val selector : a Pass_type.Selector.t
  val of_str : string -> a
  val to_str : a -> string
end

module Make (X : S) = struct
  let raise : (Errors.t, unit) raise = raise_failwith "test"
  let expected_failure_fwd i pass = expected_failure_fwd_ X.selector (X.of_str i) pass
  let expected_failure_bwd i pass = expected_failure_bwd_ X.selector (X.of_str i) pass

  let expected_sucess_fwd i pass =
    expected_sucess_fwd_ X.selector (X.of_str i) pass X.to_str


  let expected_sucess_bwd i pass =
    expected_sucess_bwd_ X.selector (X.of_str i) pass X.to_str


  let ( |-> ) = expected_sucess_fwd
  let ( <-| ) = expected_sucess_bwd
  let ( |->! ) = expected_failure_fwd
  let ( !<-| ) = expected_failure_bwd
end

module Expr = Make (struct
  type a = expr

  let selector = Pass_type.Selector.expr
  let of_str = S_exp.expr_of_sexp <@ Sexp.of_string <@ Dummies.replace_in_input
  let to_str x = Dummies.replace_in_output @@ Format.asprintf "%a" PP.expr x
end)

module Ty_expr = Make (struct
  type a = ty_expr

  let selector = Pass_type.Selector.ty_expr
  let of_str = S_exp.ty_expr_of_sexp <@ Sexp.of_string <@ Dummies.replace_in_input
  let to_str x = Dummies.replace_in_output @@ Format.asprintf "%a" PP.ty_expr x
end)

module Program = Make (struct
  type a = program

  let selector = Pass_type.Selector.program
  let of_str = S_exp.program_of_sexp <@ Sexp.of_string <@ Dummies.replace_in_input
  let to_str x = Dummies.replace_in_output @@ Format.asprintf "%a" PP.program x
end)

module Pattern = Make (struct
  type a = pattern

  let selector = Pass_type.Selector.pattern
  let of_str = S_exp.pattern_of_sexp <@ Sexp.of_string <@ Dummies.replace_in_input
  let to_str x = Dummies.replace_in_output @@ Format.asprintf "%a" PP.pattern x
end)