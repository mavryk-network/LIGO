#include "./gitlab-pages/docs/advanced/src/testing/remove-balance.mligo"
let _u = Test.reset_state 5n ([] : mav list)
let balances : balances =
  let a1, a2, a3 = Test.nth_bootstrap_account 1, Test.nth_bootstrap_account 2, Test.nth_bootstrap_account 3
  in Map.literal [(a1, 10mv); (a2, 100mv); (a3, 1000mv)]
let test =
  List.iter
    (fun ((threshold , expected_size) : mav * nat) ->
      let tester (balances, threshold : balances * mav) = Map.size (remove_balances_under balances threshold) in
      let size = Test.run tester (balances, threshold) in
      let expected_size = Test.eval expected_size in
      let () = Test.log ("expected", expected_size) in
      let () = Test.log ("actual",size) in
      assert (Test.michelson_equal size expected_size)
    )
    [(15mav,2n);(130mav,1n);(1200mav,0n)]