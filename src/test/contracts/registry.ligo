// PascaLIGO contract exercising records, maps, options, currency and views
// (Mavryk dialect, 0.73 grammar).

type storage is
  record [
    ledger : map (nat, mav);
    total  : mav;
  ]

[@entry] function deposit (const key : nat; const store : storage) : list(operation) * storage is
  block {
    const amount : mav = Mavryk.get_amount();
    var ledger : map (nat, mav) := store.ledger;
    const prev : mav =
      case ledger[key] of [
        Some (v) -> v
      | None -> 0mumav
      ];
    ledger[key] := prev + amount;
  } with ((nil : list(operation)), record [ ledger = ledger; total = store.total + amount ])

[@view] function balance_of (const key : nat; const store : storage) : option (mav) is
  store.ledger[key]

[@view] function total_supply (const _u : unit; const store : storage) : mav is
  store.total
