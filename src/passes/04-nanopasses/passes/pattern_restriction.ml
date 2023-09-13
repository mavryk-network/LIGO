open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* Throw errors on currently unsupported patterns (literals, rests) *)
include Flag.No_arg ()

let is_literal_string (x : Literal_value.t) =
  match x with
  | Literal_string s -> true
  | _ -> false


let compile ~raise =
  let pattern : _ pattern_ -> pattern = function
    | { wrap_content = P_unit; location = loc } ->
      (* For some reason we need to annotate unit pattern with unit type.
        see https://gitlab.com/ligolang/ligo/-/issues/1700
      *)
      p_typed ~loc (tv_unit ~loc ()) (p_unit ~loc)
    | { wrap_content = P_literal l; _ } as p when not (is_literal_string l) ->
      raise.error (unsupported_pattern_type p)
    | { wrap_content = P_rest _; _ } as p -> raise.error (unsupported_pattern_type p)
    | e -> make_p ~loc:(Location.get_location e) (Location.unwrap e)
  in
  Fold { idle_fold with pattern }


let name = __MODULE__
let decompile ~raise:_ = Nothing
let reduction ~raise:_ = Iter.defaults
