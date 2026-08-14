[@inline]
function fst (const p : nat * nat) : nat is p.0

[@entry]
function main (const p : nat * nat; const s : nat * nat) : list (operation) * (nat * nat) is
  ((nil : list (operation)), (fst ((p.0, p.1)), fst ((s.1, s.0))))