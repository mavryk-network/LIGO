type taco_supply is
  record [
    current_stock : nat;
    max_price     : mav
  ]

type taco_shop_storage is map (nat, taco_supply)

type return is list (operation) * taco_shop_storage

const ownerAddress : address = ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV" : address)
const donationAddress : address = ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)

function buy_taco (const taco_kind_index : nat; var taco_shop_storage : taco_shop_storage) : return is {
    // Retrieve the taco_kind from the contract's storage or fail
    const taco_kind : taco_supply =
      case taco_shop_storage[taco_kind_index] of [
        Some (kind) -> kind
      | None -> (failwith ("Unknown kind of taco.") : taco_supply)
      ];

    const current_purchase_price : mav =
      taco_kind.max_price / taco_kind.current_stock;

    if Mavryk.get_amount() =/= current_purchase_price then
      // We won't sell tacos if the amount is not correct
      failwith ("Sorry, the taco you are trying to purchase has a different price");

    // Decrease the stock by 1n, because we have just sold one
    taco_kind.current_stock := abs (taco_kind.current_stock - 1n);

    // Update the storage with the refreshed taco_kind
    taco_shop_storage[taco_kind_index] := taco_kind;

    const receiver : contract (unit) =
      case (Mavryk.get_contract_opt (ownerAddress): option(contract (unit))) of [
        Some (contract) -> contract
      | None -> (failwith ("Not a contract") : contract (unit))
      ];

    const donationReceiver : contract (unit) =
      case (Mavryk.get_contract_opt (donationAddress): option(contract (unit))) of [
        Some (contract) -> contract
      | None  -> (failwith ("Not a contract") : contract (unit))
      ];

    const donationAmount : mav = Mavryk.get_amount() / 10n;

    const operations : list (operation) = list [
      Mavryk.transaction (unit, Mavryk.get_amount() - donationAmount, receiver);
      Mavryk.transaction (unit, donationAmount, donationReceiver);
    ]
  } with (operations, taco_shop_storage)
