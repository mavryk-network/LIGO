const test_accounts =
  block {
    const initial_balances : list (mav) = list [];
    const _u1 = Test.Next.State.reset (3n, initial_balances);
    const admin_account = Test.Next.Account.address (0n);
    const user_account1 = Test.Next.Account.address (1n);
    const user_account2 = Test.Next.Account.address (2n);

    const _u2 = Test.Next.IO.log (Test.Next.Address.get_balance (admin_account));
    // 3800000000000mumav
    const _u3 = Test.Next.IO.log (Test.Next.Address.get_balance (user_account1));
    // 3800000000000mumav
  } with Test.Next.IO.log (Test.Next.Address.get_balance (user_account2))
    // 3800000000000mumav