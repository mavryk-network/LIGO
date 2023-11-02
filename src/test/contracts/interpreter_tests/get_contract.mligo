type parameter = Foo of int | Bar of nat

let main ((p, s) : parameter * int) : operation list * int =
  let k = match p with | Foo i -> i | Bar n -> n * 1 in
  [], s + k

let test =
  let (ta, _, _) = Test.originate main 0 0mav in
  let c = Test.to_contract ta in
  let a = Mavryk.address c in
  let () = assert_some (Mavryk.get_entrypoint_opt "%foo" a : (int contract) option) in
  let _ = (Mavryk.get_entrypoint "%foo" a : (int contract)) in
  let () = assert_some (Mavryk.get_contract_opt a : (parameter contract) option) in
  let _ = (Mavryk.get_contract a : (parameter contract)) in
  let _ = (Mavryk.get_contract_with_error a "foo" : (parameter contract)) in
  ()
