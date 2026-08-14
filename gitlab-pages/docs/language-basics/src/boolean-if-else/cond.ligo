type magnitude is
  Small of unit
| Large of unit  // See variant types.

function compare (const n : nat) : magnitude is
  if n < 10n then Small (unit) else Large (unit)