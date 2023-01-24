export const ml = `
  module Tezos = struct

    let get_amount (_u : unit) : tez = [%Michelson ({| { DROP ; AMOUNT } |} : unit -> tez)] ()
    let get_now (_u : unit) : timestamp = [%Michelson ({| { DROP ; NOW } |} : unit -> timestamp)] ()
    let get_sender (_u : unit) : address = [%Michelson ({| { DROP ; SENDER } |} : unit -> address)] ()
    let get_source (_u : unit) : address = [%Michelson ({| { DROP ; SOURCE } |} : unit -> address)] ()
    let get_level (_u : unit) : nat = [%Michelson ({| { DROP ; LEVEL } |} : unit -> nat)] ()
    let get_self_address (_u : unit) : address = [%Michelson ({| { DROP ; SELF_ADDRESS } |} : unit -> address)] ()
    let get_chain_id (_u : unit) : chain_id = [%Michelson ({| { DROP ; CHAIN_ID } |} : unit -> chain_id)] ()
    let get_total_voting_power (_u : unit) : nat = [%Michelson ({| { DROP ; TOTAL_VOTING_POWER } |} : unit -> nat)] ()
    let get_min_block_time (_u : unit) : nat = [%Michelson ({| { DROP; MIN_BLOCK_TIME } |} : unit -> nat) ] ()
    let voting_power (kh : key_hash) : nat = [%Michelson ({| { VOTING_POWER } |} : key_hash -> nat)] kh
    let address (type a) (c : a contract) : address = [%Michelson ({| { ADDRESS } |} : a contract -> address)] c

  end

  let main (_action, _store : unit * int) : operation list * int =
   [],    // No operations
   0
`;

export const js = `
type storage = int

type parameter =
| ["Increment", int]
| ["Decrement", int]
| ["Reset"];

/* Two entrypoints */

const add = (store: storage, delta: int) => store + delta;
const sub = (store: storage, delta: int) => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */

const main = (action: parameter, store: storage) : [ list<operation> , storage ] => {
 return [
   list([]),    // No operations
   (match (action, {
    Increment: n => add (store, n),
    Decrement: n => sub (store, n),
    Reset:     ()  => 0}))
  ]
};
`;
