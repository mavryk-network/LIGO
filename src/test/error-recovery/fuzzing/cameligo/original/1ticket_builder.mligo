(*
Modelled after:

  https://gitlab.com/tezos/tezos/-/blob/95a072715b/tests_python/contracts_alpha/mini_scenarios/ticket_builder_fungible.mv

Goes with ticket_wallet.mligo.
*)

type mint_parameter =
  [@layout comb]
  {destination : unit ticket contract;
   amount : nat}

type parameter =
  | Burn of unit ticket
  | Mint of mint_parameter

type storage =
  [@layout comb]
  {admin : address}

let main (arg : parameter * storage) : operation list * storage =
  begin
    assert (Mavryk.get_amount () = 0mumav);
    let (p,s) = arg in
    match p with
    | Burn ticket ->
      begin
        let ((ticketer, _), ticket) = (Mavryk.read_ticket ticket : (address * (unit * nat)) * unit ticket) in
        assert (ticketer = Mavryk.get_self_address ());
        (([] : operation list), s)
      end
    | Mint mint ->
      begin
        assert (Mavryk.get_sender () = s.admin);
        let ticket = Mavryk.create_ticket () mint.amount in
        let op = Mavryk.transaction ticket 0mumav mint.destination in
        ([op], s)
      end
  end
