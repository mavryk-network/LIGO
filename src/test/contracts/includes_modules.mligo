module B = struct
	module A = struct
		let toto = 2
	end
	include A
end

include B

type parameter = unit
type storage = int
type return = operation list * storage

let main ((),_ : parameter * storage) : return =
	[],toto
