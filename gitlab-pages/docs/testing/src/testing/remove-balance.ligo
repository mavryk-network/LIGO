// This is remove-balance.ligo

type balances is map (address, mav)

function remove_balances_under (const b : balances; const threshold : mav) : balances is
  Map.fold (
    function (const acc_kv : balances * (address * mav)) : balances is
      block { const (acc, (k, v)) = acc_kv; } with (if v < threshold then Map.remove (k, acc) else acc),
    b, b)