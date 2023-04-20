open Simple_utils.Trace
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
    (pp : 'a -> unit)
    : unit
  =
  pp @@ (selector pass).forward input


let expected_sucess_bwd_
    (selector : 'a Pass_type.Selector.t)
    (input : 'a)
    (pass : Pass_type.pass)
    (pp : 'a -> unit)
    : unit
  =
  pp @@ (selector pass).backward input


module type S = sig
  type a

  val selector : a Pass_type.Selector.t
  val of_str : string -> a
  val pp : a -> unit
end

module Make (X : S) = struct
  let raise : (Errors.t, unit) raise = raise_failwith "test"

  let expected_failure_fwd i pass = expected_failure_fwd_ X.selector (X.of_str i) pass
  let expected_failure_bwd i pass = expected_failure_bwd_ X.selector (X.of_str i) pass
  let expected_sucess_fwd i pass = expected_sucess_fwd_ X.selector (X.of_str i) pass X.pp
  let expected_sucess_bwd i pass = expected_sucess_bwd_ X.selector (X.of_str i) pass X.pp
  let ( |-> ) = expected_sucess_fwd
  let ( <-| ) = expected_sucess_bwd
  let ( |->! ) = expected_failure_fwd
  let ( !<-| ) = expected_failure_bwd
end

module Ty_expr = Make (struct
  type a = ty_expr

  let selector = Pass_type.Selector.ty_expr
  let of_str input = input |> Sexp.of_string |> S_exp.ty_expr_of_sexp
  let pp x = Format.printf "%a" PP.ty_expr x
end)

module Program = Make (struct
  type a = program

  let selector = Pass_type.Selector.program
  let of_str input = input |> Sexp.of_string |> S_exp.program_of_sexp
  let pp x = Format.printf "%a" PP.program x
end)

module Pattern = Make (struct
  type a = pattern

  let selector = Pass_type.Selector.pattern
  let of_str input = input |> Sexp.of_string |> S_exp.pattern_of_sexp
  let pp x = Format.printf "%a" PP.pattern x
end)