function create_and_call (const st : list (address)) is {
  const create_contract_result = 
      Mavryk.create_contract(
          (function (const p : int; const s : int) is
            ((list [] : list (operation)), p + s)),
          (None : option (key_hash)),
          0mumav,
          1
      );
  const create_op = create_contract_result.0;
  const addr = create_contract_result.1;
  const call_op =
      Mavryk.transaction(
          (addr, 41),
          0mumav,
          (Mavryk.self ("%callback") : contract (address * int))
      )
} with (list [create_op; call_op], (addr # st))

function call_counter (const addr : address; const n : int) is {
  assert (Mavryk.get_sender() = Mavryk.get_self_address());
  const callee_opt : option (contract (int)) = Mavryk.get_contract_opt (addr);
  const callee =
      case callee_opt of [
      | Some (contract) -> contract
      | None -> (failwith ("Could not find contract") : contract (int))
      ]
} with Mavryk.transaction (n, 0mumav, callee)

type parameter is
| Callback of address * int
| CreateAndCall

function main (const param : parameter; const st : list (address)) is
  case param of [
  | CreateAndCall -> create_and_call (st)
  | Callback (vs) -> (list [call_counter (vs)], st)
  ]
