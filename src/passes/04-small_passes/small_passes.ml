module I = Ast_unified
module O = Ast_core
open Passes.Pass_type

let trivial_compile_program : I.program -> O.program = fun _ -> failwith ""
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


let passes
    ~(raise : (Passes.Errors.t, _) Simple_utils.Trace.raise)
    ~(options : Compiler_options.t)
  =
  ignore (raise, options);
  let open Passes in
  let syntax = options.frontend.syntax in
  [ T_arg.pass
  ; Type_abstraction_declaration.pass ~raise
  ; Named_fun.pass ~raise
  ; Freeze_operators.pass ~raise ~syntax
  ; List_as_function.pass ~raise ~syntax
  ]


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
