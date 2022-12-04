module I = Ast_unified
module O = Ast_core
module Passes = Passes
module Pass_example = Pass_example
open Passes

let trivial_compile_program : I.program -> O.program = fun _ -> failwith "TODO12"
let trivial_compile_expression : I.expr -> O.expression = fun _ -> failwith ""

let compile_with_passes : type a. a sub_pass list -> a -> a =
 fun passes prg ->
  let f : a * a dyn_reduction_check list -> a sub_pass -> a * a dyn_reduction_check list =
   fun (prg, checks) pass ->
    let prg = pass.forward prg in
    (* checking all the reductions so far *)
    let checks = pass.forward_check :: checks in
    (combine_checks checks) prg;
    prg, checks
  in
  let prg, _ = List.fold passes ~init:(prg, []) ~f in
  prg


let passes ~raise ~options = [ Remove_t_arg.pass ]

let compile_program ~raise ~options : I.program -> O.program =
 fun prg ->
  let passes = passes ~raise ~options in
  let prg = compile_with_passes (List.map ~f:(fun x -> x.program) passes) prg in
  trivial_compile_program prg


let compile_expression ~raise ~options : I.expr -> O.expression =
 fun expr ->
  let passes = passes ~raise ~options in
  let expr = compile_with_passes (List.map ~f:(fun x -> x.expression) passes) expr in
  trivial_compile_expression expr
