#import "tezos-ligo-fa2/lib/fa2/asset/single_asset.mligo" "FA2"

type t = address

let get_transfer_entrypoint (addr : address) : FA2.transfer contract =
    match (Mavryk.get_entrypoint_opt "%transfer" addr : FA2.transfer contract option) with
        None -> failwith "receiver_not_found"
        | Some c -> c

let transfer (token_addr, from_, to_, amount_: t * address * address * nat) : operation =
    let dest = get_transfer_entrypoint (token_addr) in
    let transfer_requests = ([
      ({from_=from_; tx=([{to_=to_;amount=amount_}] : FA2.atomic_trans list)});
    ] : FA2.transfer) in
    Mavryk.transaction transfer_requests 0mumav dest

let get_total_supply (token_addr : t) : nat option = Mavryk.call_view "total_supply" unit token_addr
