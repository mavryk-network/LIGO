// Type aliasing

type account = address
type number_of_transactions = nat

// The type account_data is a record with two fields.

type account_data = {
  balance : mav;
  transactions : number_of_transactions
}

// A ledger is a map from accounts to account_data

type ledger = (account, account_data) map

let my_ledger : ledger = Map.literal
  [(("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address),
    {balance = 10mumav; transactions = 5n})]