recursive function iter (const x : nat; const y : nat) : nat is
  if y = 0n then x else iter (y, x mod y)

function gcd (const x : nat; const y : nat) : nat is
  if x < y then iter (y, x) else iter (x, y)