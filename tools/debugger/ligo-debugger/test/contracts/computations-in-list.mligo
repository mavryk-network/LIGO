let unsafeCompute (_ : unit) : int = failwith "bruh"

let zero : int = 0

let main (_, _ : unit * int list) : operation list * int list =
  let s = [1; 2; 3; zero; unsafeCompute(); unsafeCompute(); zero; 4] in
  (([] : operation list), s)
