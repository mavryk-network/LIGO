// The type account_balances denotes maps from addresses to mav

type account_balances is map (address, mav)

const ledger : account_balances =
  Map.literal (list [(("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address), 10mumav)])