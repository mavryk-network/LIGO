// #import "imported_failure.mligo" "Failure"
// #import "imported_failure_copy.mligo" "Failure2"

// type alias = Failure.failure

module M = struct
  // type failure = list list
  // let failure = 0 + ""

  let x = 1 + "" // x : forall a . a

  let y = x
end

// let special_thing = 1 + ""

// let test (type a) : a = failwith "aaaaa"

// let main (_, _ : int * int) : operation list * int =
//   [], (M.failure)


// let x : string = special_thing
let y : int = M.y

// let a = 1

//let main2 (_, _ : int * int) : operation list * int =
//  [], Failure2.failure
