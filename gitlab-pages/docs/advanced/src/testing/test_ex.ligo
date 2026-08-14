module C is {
  [@entry] function main (const p : int * int; const _s : unit) : list (operation) * unit is
    (list [Mavryk.emit ("%foo", p); Mavryk.emit ("%foo", p.0)], unit)
}

const test_foo =
  block {
    const orig = Test.originate (contract_of C, unit, 0mav);
    const _r = Test.transfer_exn (orig.addr, Main (1, 2), 0mav);
  } with (
    (Test.get_last_events_from (orig.addr, "foo") : list (int * int)),
    (Test.get_last_events_from (orig.addr, "foo") : list (int))
  )