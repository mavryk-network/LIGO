// This is mycontract-test.ligo

#import "gitlab-pages/docs/testing/src/testing/mycontract.ligo" "MyContract"

const run_test1 =
  block {
    const initial_storage = 10;
    const orig = Test.Next.Originate.contract (contract_of MyContract.MyContract, initial_storage, 0mav);
    const _u = Assert.assert (Test.Next.Typed_address.get_storage (orig.taddr) = initial_storage);
    const _n : nat = Test.Next.Contract.transfer_exn (Test.Next.Typed_address.get_entrypoint ("increment", orig.taddr), 32, 0mav);
  } with Assert.assert (Test.Next.Typed_address.get_storage (orig.taddr) = initial_storage + 32)