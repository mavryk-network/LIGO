#include "./gitlab-pages/docs/advanced/src/testing/remove-balance.mligo"
let _u = Test.reset_state 5n ([] : mav list)
let balances : balances =
  let a1, a2, a3 = Test.nth_bootstrap_account 1, Test.nth_bootstrap_account 2, Test.nth_bootstrap_account 3
  in Map.literal [(a1, 10mv); (a2, 100mv); (a3, 1000mv)]