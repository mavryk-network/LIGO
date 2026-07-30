type 'a return = operation list * 'a

[@entry]
let main (() : unit) (s : int) : int return = ([] : operation list), s

(* self forbidden anyway *)

[@view]
let bad_view2 (() : unit) (_ : int) : unit contract =
  let x : unit contract = Mavryk.self "%default" in
  x
