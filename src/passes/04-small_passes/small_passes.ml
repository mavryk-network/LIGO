module I = Ast_unified
module O = Ast_core

type syntax = unit (* TODO *)

type 'a pass =
  { name : string
  ; compile : 'a -> 'a
  ; decompile : syntax -> 'a -> 'a
  ; check_reductions : 'a -> bool (* mostly useful for debugging *)
  }

type 'a check =
  { name : string
  ; f : 'a -> unit
  }

let trivial_compile_program : I.program -> O.program =
  (*
    should be as trivial as:

    match x with
    | I.Node_that_should_have_been_reducted _ -> failwith "impossible"
    | I.Node_final_form x -> O.Final_form x    
  *)
  failwith "TODO12"


let trivial_compile_expression : I.expression -> O.expression = failwith ""

let compile_with_passes : type a. a pass list -> a check list -> a -> a =
 fun passes checks prg ->
  let f : int -> a -> a pass -> a =
   fun i prg pass ->
    let prg = pass.compile prg in
    if pass.check_reductions prg
    then
      failwith
        (Format.asprintf "pass number %d(%s) did not fully reduce" i pass.name);
    prg
  in
  let prg = List.foldi passes ~init:prg ~f in
  List.iter checks ~f:(fun check -> check.f prg);
  prg


let compile ~raise : I.program -> O.program =
 fun x ->
  let x = compile_with_passes [] [] x in
  (* TODO:
    at this point, all the "passes" reductions must have happened and the compilation to the targetted AST should be trivial
    add a [@final] on AST nodes that we accept as "trivially compiled" to detect errors
  *)
  trivial_compile_program x


let compile_expression ~raise : I.expression -> O.expression =
 fun x ->
  let x = compile_with_passes [] [] x in
  trivial_compile_expression x