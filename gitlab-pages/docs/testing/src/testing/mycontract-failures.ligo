module MyContract is {
  type storage is int
  type result is list (operation) * storage

  [@entry] function increment (const delta : int; const storage : storage) : result is
    if abs (delta) <= 5n then ((nil : list (operation)), storage + delta) else failwith ("Pass 5 or less")
  [@entry] function decrement (const delta : int; const storage : storage) : result is
    if abs (delta) <= 5n then ((nil : list (operation)), storage - delta) else failwith ("Pass 5 or less")
  [@entry] function reset (const _u : unit; const _storage : storage) : result is
    ((nil : list (operation)), 0)
}
const test_failure =
  block {
    const initial_storage = 10;
    const orig = Test.Next.Originate.contract (contract_of MyContract, initial_storage, 0mav);
    const result : test_exec_result = Test.Next.Contract.transfer (Test.Next.Typed_address.get_entrypoint ("increment", orig.taddr), 50, 0mav);
  } with case result of [
    Fail (_x) -> Test.Next.IO.log ("Failed as expected")
  | Success (_s) -> failwith ("This should not succeed")
  ]