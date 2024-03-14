type storage = int

type parameter = int

type x = Left of int

[@entry]
let main (p : parameter) (s : storage): operation list * storage =
  let contract =
    match Tezos.get_entrypoint_opt "%left" ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe": address) with
    | Some c -> c
    | None -> failwith "contract does not match"
  in
  [Tezos.transaction (Left 2) 2mumav contract], s