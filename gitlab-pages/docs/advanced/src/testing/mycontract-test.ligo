// This is mycontract-test.ligo

#import "gitlab-pages/docs/advanced/src/testing/mycontract.ligo" "MyContract"
type param is parameter_of MyContract.C

const test1 =
  block {
    const initial_storage = 42;
    const orig = Test.originate (contract_of MyContract.C, initial_storage, 0mav);
  } with assert (Test.get_storage (orig.addr) = initial_storage)
// This continues mycontract-test.ligo

const test2 =
  block {
    const initial_storage = 42;
    const orig = Test.originate (contract_of MyContract.C, initial_storage, 0mav);
    const gas_cons = Test.transfer_exn (orig.addr, Increment (1), 1mumav);
    const _u = Test.log (("gas consumption", gas_cons));
  } with assert (Test.get_storage (orig.addr) = initial_storage + 1)