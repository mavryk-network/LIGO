function michelson_add (const n : nat * nat) : nat is
  ([%Michelson ({| { UNPAIR ; ADD } |} : nat * nat -> nat)]) (n)