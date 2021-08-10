
let test_string =
  let data = "Coucou" in
  let packed = Bytes.pack data in
  let unpacked : string option = Bytes.unpack packed in
  assert (Some data = unpacked)

let test_int =
  let data = 42 in
  let packed = Bytes.pack data in
  let unpacked : int option = Bytes.unpack packed in
  assert (Some data = unpacked)
