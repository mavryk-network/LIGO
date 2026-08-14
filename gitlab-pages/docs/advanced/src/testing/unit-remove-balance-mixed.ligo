#include "./gitlab-pages/docs/advanced/src/testing/remove-balance.ligo"
const _u = Test.reset_state (5n, (nil : list (mav)))
const balances : balances =
  block {
    const a1 = Test.nth_bootstrap_account (1);
    const a2 = Test.nth_bootstrap_account (2);
    const a3 = Test.nth_bootstrap_account (3);
  } with Map.literal (list [(a1, 10mav); (a2, 100mav); (a3, 1000mav)])
const test =
  List.iter (
    function (const kv : mav * nat) : unit is
      block {
        const (threshold, expected_size) = kv;
        const tester = function (const bt : balances * mav) : nat is
          block { const (bals, thr) = bt; } with Map.size (remove_balances_under (bals, thr));
        const size = Test.run (tester, (balances, threshold));
        const expected_size_ = Test.eval (expected_size);
        const _u1 = Test.log (("expected", expected_size_));
        const _u2 = Test.log (("actual", size));
      } with assert (Test.michelson_equal (size, expected_size_)),
    list [(15mav, 2n); (130mav, 1n); (1200mav, 0n)])