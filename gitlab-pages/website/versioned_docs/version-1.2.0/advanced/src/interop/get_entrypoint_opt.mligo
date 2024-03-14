type storage = int

type parameter = int

type x = Left of int

[@entry]
let main (p : parameter) (s : storage): operation list * storage =
  let contract =
    match Tezos.get_entrypoint_opt "%left" ("mv1XJ6kbMgDvXvvtw8KBG2Ne2ngNHxLfuUvE": address) with
    | Some c -> c
    | None -> failwith "contract does not match"
  in
  [Tezos.transaction (Left 2) 2mumav contract], s