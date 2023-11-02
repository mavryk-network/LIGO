type storage is unit
type return is list (operation) * storage

function cb (const s : storage) : return is {
  const c : contract (unit) = Mavryk.get_contract_with_error (Mavryk.get_sender(), "error")
} with (list [Mavryk.transaction (unit, 0mav, c)], s)


function cbo (const s : unit) : return is
  {
    const c : contract (unit) = Mavryk.get_contract_with_error(Mavryk.get_sender(), "contract not found")
  } with (list [Mavryk.transaction (unit, 0mav, c)], s)
