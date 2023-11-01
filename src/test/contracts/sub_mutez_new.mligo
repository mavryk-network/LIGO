let sub (store, delta : mav * mav) : mav option = store - delta

let main (_, store : unit * mav) : operation list * mav =
  ([] : operation list), Option.unopt (sub (store, 1mav))