type storage is unit

type return is list (operation) * storage

function cb (const a : address; const s : storage) : return is
  {
    const c : contract (unit) = Mavryk.get_entrypoint ("%cb", a)
  } with (list [Mavryk.transaction (unit, 0mav, c)], s)


function cbo (const a : address; const s : storage) : return is
  {
    const c : contract (unit) =
      case (Mavryk.get_entrypoint_opt ("%cbo", a) : option (contract (unit))) of [
        Some (c) -> c
      | None -> (failwith ("cbo: Entrypoint not found.") : contract (unit))
      ]
  } with (list [Mavryk.transaction (unit, 0mav, c)], s)
