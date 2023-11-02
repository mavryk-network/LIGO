module Tezo = struct
  module X = struct
    module Y = struct
      let amoun = 1mav
    end
  end
end

let balanc = 2mav
let size = 10
let bal = balanc + 1mav
let amt = Tezo.X.Y.amoun + 1mav


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