#import "@ligo/dao/src/token.mligo" "Token"

type storage = nat option

let main (_,_ : unit * storage) : operation list * storage 
  = [], Token.get_total_supply (Tezos.get_sender ())