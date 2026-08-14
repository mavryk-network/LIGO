[@entry] function main (const p : bool; const s : unit) : list (operation) * unit is
  block {
    const u : unit = assert (p)
  } with ((nil : list (operation)), s)

[@entry] function some (const o : option (unit); const s : unit) : list (operation) * unit is
  block {
    const u : unit = assert_some (o)
  } with ((nil : list (operation)), s)