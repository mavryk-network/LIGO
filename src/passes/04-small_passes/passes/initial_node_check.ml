open Ast_unified
open Pass_type
module Location = Simple_utils.Location

(* This is a temporary dynamic check that the unification pass do not emit
node that are reserved for the nano passes *)

let reduction ~raise =
  ignore raise;
  let check f x = if f (Location.unwrap x) then failwith "Unification emit forbidden nodes" in
  { Iter.defaults with
    expr = check expr_is_not_initial
  ; ty_expr = check ty_expr_is_not_initial
  ; pattern = check pattern_is_not_initial
  ; statement = check statement_is_not_initial
  ; mod_expr = check mod_expr_is_not_initial
  ; instruction = check instruction_is_not_initial
  ; declaration = check declaration_is_not_initial
  ; program = (fun x -> if program_entry_is_not_initial x then failwith "l")
  }


let pass ~raise =
  cata_morph
    ~name:__MODULE__
    ~compile:`None
    ~decompile:`None
    ~reduction_check:(reduction ~raise)