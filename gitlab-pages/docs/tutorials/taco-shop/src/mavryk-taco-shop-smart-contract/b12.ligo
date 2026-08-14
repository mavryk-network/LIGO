module TacoShop is {
  type taco_supply is record [ current_stock : nat ; max_price : mav ]

  type taco_shop_storage is map (nat, taco_supply)

  [@entry]
  function buy_taco (const taco_kind_index : nat; const taco_shop_storage : taco_shop_storage) : list (operation) * taco_shop_storage is
    ((nil : list (operation)), taco_shop_storage)
}