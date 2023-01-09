open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

(*
  warns about unreachable code and detect meaningless return statements
*)

let has_returned : statement -> bool = failwith "wait"
let unreachable_code ~raise : statement Simple_utils.List.Ne.t -> unit = failwith "lol"

let compile =
  let instruction : _ instruction_ -> instruction = function
    | { wrap_content =
          ( I_While { block; _ }
          | I_For { block; _ }
          | I_ForIn (ForMap { block; _ })
          | I_ForIn (ForSetOrList { block; _ })
          | I_Cond { ifso = ClauseBlock block; _ }
          | I_Cond { ifnot = Some (ClauseBlock block); _ } )
      ; location = loc
      } -> failwith "no return .."
    | { wrap_content = I_ForOf _one_statmeent; location = loc } ->
      failwith "one statement only mmh"
    | { wrap_content = I_Case _special; location = loc } -> failwith "mmh"
    | { wrap_content = I_Switch _special; location = loc } -> failwith "mmh"
    | { wrap_content; location = loc } -> make_i ~loc wrap_content
  in
  `Cata { idle_cata_pass with instruction }