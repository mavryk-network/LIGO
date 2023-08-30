open Ast_unified
open Pass_type
module Location = Simple_utils.Location

(* This pass should forbid break/continue outside switch statements
*)
include Flag.No_arg ()


let compile ~raise:_ = Nothing


let name = __MODULE__
let decompile ~raise:_ = Nothing
let reduction ~raise:_ = Iter.defaults
