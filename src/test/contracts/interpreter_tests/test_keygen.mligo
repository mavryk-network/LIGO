
let test =
  let (pk, pkh) = Test.keygen () in
  let chash = Test.run (fun (_ : unit) -> Crypto.hash_key pk) () in
  let () = assert (Test.michelson_equal chash (Test.eval pkh)) in
  let data = Bytes.pack "Bonjour le monde !" in
  let sgn = Test.sign pkh data in
  let cchk = Test.run (fun (_ : unit) -> Crypto.check pk sgn data) () in
  assert (Test.michelson_equal cchk (Test.eval true))
