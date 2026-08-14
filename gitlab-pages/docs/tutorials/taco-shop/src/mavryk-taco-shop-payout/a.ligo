module TacoShop is {
  type taco_supply is
    record [
      current_stock : nat;
      max_price     : mav
    ]

  type taco_shop_storage is map (nat, taco_supply)

  [@entry]
  function buy_taco (const taco_kind_index : nat; const taco_shop_storage : taco_shop_storage)
    : list (operation) * taco_shop_storage is
    block {
      // Retrieve the taco_kind from the contract's storage or fail
      const taco_kind : taco_supply =
        case Map.find_opt (taco_kind_index, taco_shop_storage) of [
          Some (k) -> k
        | None -> (failwith ("Unknown kind of taco") : taco_supply)
        ];
      const current_purchase_price : mav =
        taco_kind.max_price / taco_kind.current_stock;
      // We won't sell tacos if the amount is not correct
      const _check : unit =
        if (Mavryk.get_amount ()) =/= current_purchase_price
        then (failwith ("Sorry, the taco you are trying to purchase has a different price") : unit)
        else unit;
      // Update the storage decreasing the stock by 1n
      const taco_shop_storage =
        Map.update (
          taco_kind_index,
          Some (taco_kind with record [ current_stock = abs (taco_kind.current_stock - 1n) ]),
          taco_shop_storage)
    } with ((nil : list (operation)), taco_shop_storage)
}

const default_storage : TacoShop.taco_shop_storage =
  Map.literal (list [
    (1n, record [ current_stock = 50n; max_price = 50000000mumav ]);
    (2n, record [ current_stock = 20n; max_price = 75000000mumav ])
  ])