type balances is map (address, mav)

function balances_under (const b : balances ; const threshold : mav) is {
  const f =
    function (const x : balances * (address * mav)) is {
      const (acc, (k, v)) = x;
    } with if v < threshold then Map.remove (k, acc) else acc;
} with Map.fold (f, b, b)
