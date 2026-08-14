(* examples/contracts/ligo/AccessController.ligo *)

type storage is record [senders_whitelist : set (address)]

[@entry]
function call (const op : unit -> operation; const s : storage) : list (operation) * storage is
  if Set.mem (Mavryk.get_sender (), s.senders_whitelist)
  then (list [op (unit)], s)
  else failwith ("Sender is not whitelisted")

[@entry]
function iswhitelisted (const arg : address * contract (bool); const s : storage) : list (operation) * storage is
  block {
    const (addr, callback_contract) = arg;
    const whitelisted = Set.mem (addr, s.senders_whitelist);
    const op = Mavryk.transaction (whitelisted, 0mumav, callback_contract)
  } with (list [op], s)