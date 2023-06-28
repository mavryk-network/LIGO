#import "imported_failure.mligo" "Failure"

let main2 (_, _ : int * int) : operation list * int =
  [], Failure.failure
