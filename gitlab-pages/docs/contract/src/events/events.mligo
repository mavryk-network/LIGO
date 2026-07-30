[@entry]
let emitEvents (_ : unit) (storage : int) : operation list * int =
  let event1 : operation = Mavryk.Next.Operation.emit "%emitEvents" "hi" in
  let event2 : operation = Mavryk.Next.Operation.emit "%emitEvents" 6 in
  [event1; event2], storage