// proxy.ligo

type parameter is
  Increment of int
| Decrement of int
| Reset of unit

type storage is unit

type result is list (operation) * storage

const dest : address = "KT19wgxcuXG9VH4Af5Tpm1vqEKdaMFpznXT3"

[@entry]
function proxy (const action : parameter; const store : storage) : result is
  block {
    const counter : contract (parameter) = Mavryk.get_contract_with_error (dest, "not found");
    const op = Mavryk.transaction (Increment (5), 0mav, counter);
  } with (list [op], store)