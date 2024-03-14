let sub (store : mav) (delta : mav) : mav option = store - delta

let main (_ : unit) (store : mav) : operation list * mav =
  ([] : operation list), Option.unopt (sub store 1mav)