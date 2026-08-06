module C is {
  [@entry] function main (const p : int * int; const _s : unit) : list (operation) * unit is
    (list [Mavryk.emit ("%foo", p); Mavryk.emit ("%foo", p.0)], unit)
}

const test_foo =
  block {
    const orig = Test.Next.Originate.contract (contract_of C, unit, 0mav);
    const _r : nat = Test.Next.Typed_address.transfer_exn (orig.taddr, Main (1, 2), 0mav);
  } with (
    (Test.Next.State.last_events (orig.taddr, "foo") : list (int * int)),
    (Test.Next.State.last_events (orig.taddr, "foo") : list (int))
  )