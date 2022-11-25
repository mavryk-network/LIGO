module A = struct
  type parameter = unit
  module B = struct
    let toto = 2
  end
end

open A

type storage = int
type return = operation list * storage

let main ((),_ : parameter * storage) : return =
	[],B.toto
