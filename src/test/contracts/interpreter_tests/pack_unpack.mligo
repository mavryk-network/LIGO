
let test_string =
  let data = "Coucou" in
  let packed = Bytes.pack data in
  let unpacked : string = Bytes.unpack packed in
  assert (data = unpacked)

let test_int =
  let data = 42 in
  let packed = Bytes.pack data in
  let unpacked : int = Bytes.unpack packed in
  assert (data = unpacked)
