[@inline] function compare_equal <k> (const a : k; const b : k) : bool is
  block {
    const f : (k * k -> bool) = [%Michelson ({|{ UNPAIR; COMPARE; EQ }|} : k * k -> bool)];
  } with f ((a, b))