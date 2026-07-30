type storage = int
type parameter = int

type remote_param = Sub of int

[@entry]
let main (_ : parameter) (s : storage): operation list * storage =
  let contract_addr =
    Mavryk.get_entrypoint
      "%sub" // Corresponds to the `Sub` variant of `remote_param`.
      ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)
  in [Mavryk.transaction (Sub 2) 2mumav contract_addr], s