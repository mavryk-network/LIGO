module TacoShop = struct
  type taco_supply =
    {
     current_stock : nat;
     max_price : tez
    }

  type taco_shop_storage = (nat, taco_supply) map

  [@entry]
  let buy_taco (taco_kind_index : nat) (taco_shop_storage : taco_shop_storage) : operation list * taco_shop_storage =
    [], taco_shop_storage

end 
let default_storage: TacoShop.taco_shop_storage  = Map.literal [
  (1n, { current_stock = 50n ; max_price = 50tez }) ;
  (2n, { current_stock = 20n ; max_price = 75tez }) ;
]