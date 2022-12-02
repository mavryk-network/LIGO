module B = struct
	let foo = "toto"
    let foo = 1
end

include B

let bar = foo + 2


let toto = B.foo + 2
