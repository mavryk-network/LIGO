// Type aliasing

type account is address
type number_of_transactions is nat

// The type account_data is a record with two fields.

type account_data is record [
  balance : mav;
  transactions : number_of_transactions
]

// A ledger is a map from accounts to account_data

type ledger is map (account, account_data)

const my_ledger : ledger = Map.literal (list [
  (("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address),
   record [balance = 10mumav; transactions = 5n])
])