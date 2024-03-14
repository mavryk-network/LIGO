// The type account_balances denotes maps from addresses to tez

type account_balances = (address, tez) map

let ledger : account_balances =
  Map.literal
    [(("mv1XJ6kbMgDvXvvtw8KBG2Ne2ngNHxLfuUvE" : address), 10mumav)]