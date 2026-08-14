type storage is int

type parameter is int

type x is Left of int

[@entry]
function main (const p : parameter; const s : storage) : list (operation) * storage is
  block {
    const contract : contract (x) =
      case Mavryk.get_entrypoint_opt ("%left", ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)) of [
        Some (c) -> c
      | None -> (failwith ("contract does not match") : contract (x))
      ]
  } with (list [Mavryk.transaction (Left (2), 2mumav, contract)], s)