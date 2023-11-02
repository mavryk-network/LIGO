// The type accountBalances denotes maps from addresses to mav

type account_balances is map (address, mav)

const ledger : account_balances =
  map
   [("mv1XJ6kbMgDvXvvtw8KBG2Ne2ngNHxLfuUvE" : address) -> 10mumav]
