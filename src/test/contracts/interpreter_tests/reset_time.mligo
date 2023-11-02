let test_x =
  let t1 = Mavryk.get_now () in
  let () = Test.reset_state_at ("2012-02-02t10:10:10Z" : timestamp) 2n ([] : mav list) in
  let t2 = Mavryk.get_now () in
  (t1,t2)
