module I = Ast_unified
module O = Ast_core
open Passes.Pass_type
module Errors = Passes.Errors

let trivial_compile_program : I.program -> O.program =
 fun _ ->
  failwith "TODO: Everything is fine"


let trivial_compile_expression : I.expr -> O.expression =
 fun _ -> failwith "TODO : Everything is fine"


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


let passes
    ~(raise : (Passes.Errors.t, _) Simple_utils.Trace.raise)
    ~(syntax : Syntax_types.t)
  =
  let open Passes in
  [ T_arg.pass
  ; Type_abstraction_declaration.pass ~raise
  ; Named_fun.pass ~raise
  ; Freeze_operators.pass ~raise ~syntax
  ; List_as_function.pass ~raise ~syntax
  ; Match_as_function.pass ~raise ~syntax
  ; Hack_literalize_jsligo.pass ~raise ~syntax
  ; Restrict_t_app.pass ~raise
  ; T_app_michelson_types.pass ~raise
  ; Reduce_switch.pass ~raise
  ; E_rev_app.pass ~raise
  ]


let compile_program ~raise ~syntax : I.program -> O.program =
 fun prg ->
  let passes = passes ~raise ~syntax in
  print_endline (Format.asprintf "%a" (Sexp.pp_hum_indent 2) (I.S_exp.sexp_of_program prg));
  let prg = compile_with_passes (List.map ~f:(fun x -> x.program) passes) prg in
  trivial_compile_program prg


let compile_expression ~raise ~syntax : I.expr -> O.expression =
 fun expr ->
  let passes = passes ~raise ~syntax in
  let expr = compile_with_passes (List.map ~f:(fun x -> x.expression) passes) expr in
  trivial_compile_expression expr
