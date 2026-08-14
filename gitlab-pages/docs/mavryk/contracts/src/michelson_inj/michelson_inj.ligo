function michelson_add (const n : nat * nat) : nat is block {
  const f : (nat * nat -> nat) = [%Michelson ({| { UNPAIR ; ADD } |} : nat * nat -> nat)];
} with f (n)
[@entry]
function main (const param : unit; const _s : unit) : list (operation) * unit is
  block {
    const cc =
      [%create_contract_of_file "gitlab-pages/docs/mavryk/contracts/src/compiled.mv"];
    const op_addr = cc ((None : option (key_hash)), 1mav, param);
  } with (list [op_addr.0], Unit)