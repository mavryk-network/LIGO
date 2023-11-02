type storage is unit
type return is list (operation) * storage

function cb (const s : storage) : return is {
  const c : contract (unit) = Mavryk.get_contract (Mavryk.get_sender())
} with (list [Mavryk.transaction (unit, 0mav, c)], s)


function cbo (const s : unit) : return is
  {
    const c : contract (unit) =
      case (Mavryk.get_contract_opt (Mavryk.get_sender()) : option (contract (unit))) of [
        Some (contract) -> contract
      | None -> (failwith ("contract not found") : contract (unit))
      ]
  } with (list [Mavryk.transaction (unit, 0mav, c)], s)
