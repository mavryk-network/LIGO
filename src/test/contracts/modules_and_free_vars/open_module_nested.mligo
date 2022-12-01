module A1 = struct
  module B = struct
    let foo = 1
  end
end

module A2 = struct
  module B = struct
    let foo = 1
  end
end

open A1
open A2

type parameter = unit
type storage = int
type return = operation list * storage

let main ((),_ : parameter * storage) : return =
	[], B.foo

