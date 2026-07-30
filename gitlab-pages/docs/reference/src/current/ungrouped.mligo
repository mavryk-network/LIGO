let check (p,s : unit * mav) = [], Mavryk.get_balance()
let threshold (p : unit) = if Mavryk.get_amount () = 100mv then 42 else 0
let check (p : unit) = Mavryk.get_sender ()
let check (p : key_hash) =
  let c = Mavryk.implicit_account p
  in Mavryk.address c
let check (p : unit) = Mavryk.get_self_address ()
let check (p : unit) = Mavryk.self("%default")
let check (kh : key_hash) = Mavryk.implicit_account kh
let check (p : unit) = Mavryk.get_source ()
type storage = bytes

[@entry]
let main (_ignore : unit) (store : storage) =
  let packed = Bytes.pack (Mavryk.get_chain_id ()) in
  if (store <> packed) then
    (failwith "wrong chain" : (operation list * storage))
  else
    ([], (packed: storage))