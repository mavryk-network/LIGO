(* should return a constant function *)
let f1 (x : unit) : unit -> mav =
  let amt : mav = Mavryk.get_amount () in
  fun (x : unit) -> amt

(* should return an impure function *)
let f2 (x : unit) : unit -> mav =
  fun (x : unit) -> Mavryk.get_amount ()

let main (b,s : bool * (unit -> mav)) : operation list * (unit -> mav) =
  (([] : operation list), (if b then f1 () else f2 ()))
