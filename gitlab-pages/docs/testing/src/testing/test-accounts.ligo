module Counter is {
  type storage is int * address
  type return_type is list (operation) * storage

  [@entry] function increment (const n : int; const storage : storage) : return_type is
    block {
      const (number, admin_account) = storage;
    } with ((nil : list (operation)), (number + n, admin_account))

  [@entry] function sub (const n : int; const storage : storage) : return_type is
    block {
      const (number, admin_account) = storage;
    } with ((nil : list (operation)), (number - n, admin_account))

  [@entry] function reset (const _u : unit; const storage : storage) : return_type is
    block {
      const (_number, admin_account) = storage;
    } with
      if Mavryk.get_sender () = admin_account then
        ((nil : list (operation)), (0, admin_account))
      else
        (failwith ("Only the owner can call this entrypoint") : return_type)
}
const test_admin =
  block {
    const admin_account = Test.Next.Account.address (0n);
    const user_account = Test.Next.Account.address (1n);

    // Originate the contract with the admin account in storage
    const initial_storage = (10, admin_account);
    const orig = Test.Next.Originate.contract (contract_of Counter, initial_storage, 0mav);

    // Try to call the reset entrypoint as the user and expect it to fail
    const _u1 = Test.Next.State.set_source (user_account);
    const result = Test.Next.Contract.transfer (Test.Next.Typed_address.get_entrypoint ("reset", orig.taddr), unit, 0mav);
    const _u2 =
      case result of [
        Fail (_err) -> Test.Next.IO.log ("Test succeeded")
      | Success (_s) -> failwith ("User should not be able to call reset")
      ];

    // Call the reset entrypoint as the admin and expect it to succeed
    const _u3 = Test.Next.State.set_source (admin_account);
    const _n : nat = Test.Next.Contract.transfer_exn (Test.Next.Typed_address.get_entrypoint ("reset", orig.taddr), unit, 0mav);

    const (newNumber, _admin_account2) = Test.Next.Typed_address.get_storage (orig.taddr);
  } with Assert.assert (newNumber = 0)