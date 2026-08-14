type parameter is address

type storage is unit

[@entry]
function main (const destination_addr : parameter; const _s : storage) : list (operation) * storage is
  block {
    const maybe_contract = Mavryk.get_contract_opt (destination_addr);
    const destination_contract = case maybe_contract of [
      Some (contract) -> contract
    | None -> failwith ("Contract does not exist")
    ];
    const op = Mavryk.transaction (unit, Mavryk.get_amount (), destination_contract)
  } with (list [op], unit)
type t is record [hello : int; l : nat; i : bytes; g : string; o : address]
(* examples/contracts/ligo/CreateAndCall.ligo *)

// Here we create two operations: the one that will originate
// the contract, and an operation to self, that will continue
// the execution after the contract is originated.

function create_and_call (const storage : list (address)) is
  block {
    const (create_op, addr) = Mavryk.create_contract (
      function (const p : int; const s : int) : list (operation) * int is
        ((nil : list (operation)), p + s),
      (None : option (key_hash)),
      0mav,
      1
    );
    const call_op = Mavryk.transaction ((addr, 41), 0mav, Mavryk.self ("%callback"))
  } with (list [create_op; call_op], addr # storage)