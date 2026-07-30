(*
Modelled after:

  https://gitlab.com/mavos/mavos/-/blob/95a072715b/tests_python/contracts_alpha/mini_scenarios/ticket_builder_fungible.mv

Goes with ticket_wallet.mligo.
*)

module Mavryk = Mavryk.Next

type mint_parameter =
  [@layout comb]
  {
   destination : unit ticket contract;
   amount : nat
  }

type parameter =
| Burn of unit ticket
| Mint of mint_parameter

type storage = [@layout comb] {admin : address}

[@entry]
let main (p : parameter) (s : storage) : operation list * storage =
  begin
    Assert.assert (Mavryk.get_amount () = 0mumav);
    match p with
      Burn ticket ->
        begin
          let ((ticketer, _), ticket) =
            (Mavryk.Ticket.read ticket : (address * (unit * nat)) * unit ticket) in
          Assert.assert (ticketer = Mavryk.get_self_address ());
          (([] : operation list), s)
        end
    | Mint mint ->
        begin
          Assert.assert (Mavryk.get_sender () = s.admin);
          let ticket =
            Option.value_with_error
              "option is None" (Mavryk.Ticket.create () mint.amount) in
          let op = Mavryk.Operation.transaction ticket 0mumav mint.destination in
          ([op], s)
        end
  end
