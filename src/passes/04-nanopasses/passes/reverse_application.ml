open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
open Unit_test_helpers

(* 
  The reverse application operator '|>' is syntactic sugar for function application.
  This pass unsugars the E_rev_app operator into normal function application E_application
*)

let compile =
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = e.location in
    let return_self () = make_e ~loc e.wrap_content in
    match Location.unwrap e with
    | E_rev_app { x; f } -> e_call ~loc f (Location.wrap ~loc:(get_e_loc f) [ x ])
    | _ -> return_self ()
  in
  `Cata { idle_cata_pass with expr = pass_expr }


let reduction ~raise =
  let expr : _ expr_ -> unit =
   fun e ->
    match Location.unwrap e with
    | E_rev_app _ -> raise.error (wrong_reduction __MODULE__)
    | _ -> ()
  in
  { Iter.defaults with expr }


let pass ~raise ~syntax:_ =
  morph ~name:__MODULE__ ~compile ~decompile:`None ~reduction_check:(reduction ~raise)


let%expect_test _ =
  Expr.(
    {|
      (E_rev_app ((x (EXPR1)) (f (EXPR2))))
    |} |-> pass;
    [%expect {|
      (E_call (EXPR2) ((EXPR1)))
    |}])
