let michelson_add n =
  [%Michelson ({| { UNPAIR ; ADD } |} : nat * nat -> nat)] n
[@entry]
let main (param : unit) () : operation list * unit =
  let op, _addr =
    [%create_contract_of_file "gitlab-pages/docs/mavryk/contracts/src/compiled.tz"]
    None 1mav param
  in [op], ()