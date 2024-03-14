(* should return a constant function *)

let f1 (x : unit) : unit -> mav =
  let amt : mav = Tezos.get_amount () in
  fun (x : unit) -> amt

(* should return an impure function *)

let f2 (x : unit) : unit -> mav = fun (x : unit) -> Tezos.get_amount ()

[@entry]
let main (b : bool) (s : (unit -> mav)) : operation list * (unit -> mav) =
  (([] : operation list), (if b then f1 () else f2 ()))
