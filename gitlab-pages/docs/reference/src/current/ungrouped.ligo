function check (const p : unit; const s : mav) : list (operation) * mav is
  ((nil : list (operation)), Mavryk.get_balance ())
function threshold (const p : unit) : int is
  if Mavryk.get_amount () = 100mav then 42 else 0
function check (const p : unit) : address is Mavryk.get_sender ()
function check (const p : key_hash) : address is block {
  const c = Mavryk.implicit_account (p)
} with Mavryk.address (c)
function check (const p : unit) : address is Mavryk.get_self_address ()
function check (const p : unit) : contract (unit) is Mavryk.self ("%default")
function check (const kh : key_hash) : contract (unit) is Mavryk.implicit_account (kh)
function check (const p : unit) : address is Mavryk.get_source ()
type storage is bytes

[@entry]
function main (const _ignore : unit; const store : storage) : list (operation) * storage is
  block {
    const packed = Bytes.pack (Mavryk.get_chain_id ())
  } with
    if store =/= packed then
      (failwith ("wrong chain") : list (operation) * storage)
    else
      ((nil : list (operation)), packed)