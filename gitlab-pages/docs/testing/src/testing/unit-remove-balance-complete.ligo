#include "./gitlab-pages/docs/testing/src/testing/remove-balance.ligo"

const test_remove_balance =
  block {
    const _u = Test.Next.State.reset (5n, (nil : list (mav)));
    const balances : balances =
      block {
        const a1 = Test.Next.Account.address (1n);
        const a2 = Test.Next.Account.address (2n);
        const a3 = Test.Next.Account.address (3n);
      } with Map.literal (list [(a1, 10mav); (a2, 100mav); (a3, 1000mav)]);
  } with List.iter (
    function (const kv : mav * nat) : unit is
      block {
        const (threshold, expected_size) = kv;
        const tester = function (const bt : balances * mav) : nat is
          block { const (bals, thr) = bt; } with Map.size (remove_balances_under (bals, thr));
        const size = Test.Next.Michelson.run (tester, (balances, threshold));
        const expected_size_ = Test.Next.Michelson.eval (expected_size);
        const _u1 = Test.Next.IO.log (("expected", expected_size_));
        const _u2 = Test.Next.IO.log (("actual", size));
      } with Assert.assert (Test.Next.Compare.eq (size, expected_size_)),
    list [(15mav, 2n); (130mav, 1n); (1200mav, 0n)])