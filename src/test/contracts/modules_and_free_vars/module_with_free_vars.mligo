let x = 1mav

module Tezo = struct
  let amoun = x
end

let balanc = 2mav
let size = 10
let bal = balanc + 1mav
let amt = Tezo.amoun + 1mav


type parameter =
  Increment
| Decrement

type storage = mav

type return = (operation) list * storage

let main (action, _ : parameter * storage) : operation list * storage =
  (([]: operation list),
   (match action with
      Increment -> bal
    | Decrement -> amt))