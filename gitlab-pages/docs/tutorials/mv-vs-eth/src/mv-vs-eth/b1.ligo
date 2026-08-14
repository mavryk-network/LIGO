type storage is record [ rewardsLeft : mav; beneficiaryAddress : address ]

function treasury (const _p : unit; const s : storage) : list (operation) * storage is
  block {
    // We do our computations first
    const newStorage : storage = s with record [ rewardsLeft = 0mumav ];

    // Then we find our beneficiary's `handleRewards` entrypoint:
    const beneficiaryOpt = Mavryk.get_entrypoint_opt ("%handleTransfer", s.beneficiaryAddress);
    const beneficiary =
      case beneficiaryOpt of [
        Some (c) -> c
      | None -> (failwith ("Beneficiary does not exist") : contract (unit))
      ];

    // Then we prepare the internal operation we want to perform
    const operation = Mavryk.transaction (unit, s.rewardsLeft, beneficiary)

    // ...and return both the operations and the updated storage
  } with (list [ operation ], newStorage)