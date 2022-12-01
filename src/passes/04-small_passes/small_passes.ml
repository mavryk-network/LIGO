module I = Ast_unified
module O = Ast_core

(*
I --p1-> I --p2--> I --p3--> I --p4--> I --trivial--> O 
*)

type syntax = unit (* TODO *)

type 'a pass =
  { name : string (* useful ? *)
  ; compile : syntax -> 'a -> 'a
  ; decompile : syntax -> 'a -> 'a
  ; check_reductions : 'a -> bool (* mostly useful for debugging *)
  }

type 'a check =
  { name : string
  ; f : syntax -> 'a -> unit
  }

let trivial_compile_program : I.program -> O.program =
 fun _ ->
  (*
    should be as trivial as:

    match x with
    | I.Node_that_should_have_been_reducted _ -> failwith "impossible"
    | I.Node_final_form x -> O.Final_form x    
  *)
  failwith "TODO12"


let trivial_compile_expression : I.expr -> O.expression = fun _ -> failwith ""

let compile_with_passes
    : type a. syntax_todo:syntax -> a pass list -> a check list -> a -> a
  =
 fun ~syntax_todo passes checks prg ->
  let f : int -> a -> a pass -> a =
   fun i prg pass ->
    let prg = pass.compile syntax_todo prg in
    if not (pass.check_reductions prg)
    then failwith (Format.asprintf "pass number %d(%s) did not fully reduce" i pass.name);
    prg
  in
  let prg = List.foldi passes ~init:prg ~f in
  List.iter checks ~f:(fun check -> check.f syntax_todo prg);
  List.iteri passes ~f:(fun i pass ->
      if pass.check_reductions prg
      then ()
      else
        failwith (Format.asprintf "pass number %d(%s) did not fully reduce" i pass.name));
  prg


let compile ~syntax_todo ~raise : I.program -> O.program =
  let () = ignore raise in
  fun x ->
    let x = compile_with_passes ~syntax_todo [] [] x in
    (* TODO:
    at this point, all the "passes" reductions must have happened and the compilation to the targetted AST should be trivial
    add a [@final] on AST nodes that we accept as "trivially compiled" to detect errors
  *)
    trivial_compile_program x


let compile_expression ~syntax_todo ~raise : I.expr -> O.expression =
  let () = ignore raise in
  fun x ->
    let x = compile_with_passes ~syntax_todo [] [] x in
    trivial_compile_expression x
