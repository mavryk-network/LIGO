module A = struct
	let toto = 2
end

open A

type parameter = unit
type storage = int
type return = operation list * storage

let main ((),_ : parameter * storage) : return =
	[],toto
