open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
open Unit_test_helpers
module Location = Simple_utils.Location

(* Contract nodes are internally understood as a module access to variable '$contract'
   which could not be written by user *)

let compile =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_contract module_path ->
      e_module_access
        ~loc
        Mod_access.
          { module_path
          ; field = Variable.of_input_var ~loc "$contract"
          ; field_as_open = false
          }
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let decompile =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_module_access { module_path; field; field_as_open = false }
      when Variable.is_name field "$contract" -> e_contract ~loc module_path
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_contract _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise ~syntax:_ =
  morph ~name:__MODULE__ ~compile ~decompile ~reduction_check:(reduction ~raise)


let%expect_test _ =
  Expr.(
    {| (E_contract (A B)) |} |-> pass ~raise;
    [%expect
      {|
      (E_module_access
       ((module_path (A B)) (field $contract) (field_as_open false)))|}])
