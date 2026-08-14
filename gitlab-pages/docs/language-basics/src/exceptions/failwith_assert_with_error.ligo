[@entry] function main (const p : bool; const s : unit) : list (operation) * unit is
  block {
    const u : unit = assert_with_error (p, "My custom error message.")
  } with ((nil : list (operation)), s)