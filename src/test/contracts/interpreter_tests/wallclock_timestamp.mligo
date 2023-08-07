let rec fib (n : nat) : nat =
  if n <= 1n then
    n
  else
    fib (abs (n - 1)) + fib (abs (n - 2))

let test =
  let t = Test.get_real_timestamp () in
  let _ = fib 21n in
  let () =
    if (Test.get_real_timestamp () - t) > 0 then
      Test.println "Took more than 1sec"
    else
      () in
  ()
