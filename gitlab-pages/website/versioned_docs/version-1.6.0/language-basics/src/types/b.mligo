// The type account_balances denotes maps from addresses to mav

type account_balances = (address, mav) map

let ledger : account_balances =
  Map.literal
    [(("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address), 10mumav)]