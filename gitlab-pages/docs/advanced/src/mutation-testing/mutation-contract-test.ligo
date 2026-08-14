// This is mutation-contract-test.ligo

#import "gitlab-pages/docs/advanced/src/mutation-testing/mutation-contract.ligo" "MutationContract"

type storage is MutationContract.C.storage
type param is parameter_of MutationContract.C
const initial_storage = 7

function tester (const taddr : typed_address (param, storage); const _c : michelson_contract (param, storage); const _i : int) : unit is
  block {
    const _t = Test.transfer_exn (taddr, Add (7), 1mumav)
  } with assert (Test.get_storage (taddr) = initial_storage + 7)

const test_original = block {
  const orig = Test.originate (contract_of MutationContract.C, initial_storage, 0mav)
} with tester (orig.addr, orig.code, orig.size)
function tester_add_and_sub (const taddr : typed_address (param, storage); const _c : michelson_contract (param, storage); const _i : int) : unit is
  block {
    const _t1 = Test.transfer_exn (taddr, Add (7), 1mumav);
    const _a = assert (Test.get_storage (taddr) = initial_storage + 7);
    const _t2 = Test.transfer_exn (taddr, Sub (3), 1mumav)
  } with assert (Test.get_storage (taddr) = initial_storage + 4)