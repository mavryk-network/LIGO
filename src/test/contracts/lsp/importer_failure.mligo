// #import "imported_failure.mligo" "Failure"
// #import "imported_failure_copy.mligo" "Failure2"

// type alias = Failure.failure

module M = struct 
  let x = 1

  // type failure = list list
  let failure : string = 0
end

let main (_, _ : int * int) : operation list * int =
  [], M.failure


//let main2 (_, _ : int * int) : operation list * int =
//  [], Failure2.failure
