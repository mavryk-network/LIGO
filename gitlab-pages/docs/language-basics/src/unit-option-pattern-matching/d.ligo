function div (const a : nat; const b : nat) : option (nat) is
  if b = 0n then None else Some (a/b)