let f (i:int) : unit = failwith i

let assert_fail_int (type a b) (f : a -> b) (arg : a) (expected : int) : unit =
  match Test.run f arg with
  | Ok _ -> Test.failwith "expected failure"
  | Failed v ->
    if expected = (Test.decompile v : int) then
      ()
    else
      Test.failwith "unexpected value"

let test_my = assert_fail_int f 2 2