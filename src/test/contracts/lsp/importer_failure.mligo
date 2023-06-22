// #import "imported_failure.mligo" "Failure"
// #import "imported_failure_copy.mligo" "Failure2"

// type alias = Failure.failure

module M = struct
  // type failure = list list
  // let failure = 0 + ""

  let x = 1 + "" // x : forall a . a
end

// let test (type a) : a = failwith "aaaaa"

// let main (_, _ : int * int) : operation list * int =
//   [], (M.failure)


let x : string = M.x
let y : int = M.x

//let main2 (_, _ : int * int) : operation list * int =
//  [], Failure2.failure
