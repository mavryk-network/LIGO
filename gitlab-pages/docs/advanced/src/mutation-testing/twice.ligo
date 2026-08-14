function twice (const x : int) : int is x + x
function simple_tests (const f : int -> int) : unit is block {
  // Test 1
  const _t1 = assert (Test.michelson_equal (Test.run (f, 0), Test.eval (0)));
  // Test 2
  const _t2 = assert (Test.michelson_equal (Test.run (f, 2), Test.eval (4)))
} with unit

const test = simple_tests (twice)