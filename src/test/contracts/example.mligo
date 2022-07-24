let plus (x : nat) (y : nat) : nat = x + y
let external_example (type a) (x : a) : a = [%external ("EXAMPLE", x)]

(*
$ ligo compile expression --init-file src/test/contracts/example.mligo cameligo 'plus 1n (example 42n)'

Invalid type(s).
Expected: "nat", but got: "'a". 
*)

(*
$ ligo compile expression --init-file src/test/contracts/example.mligo cameligo 'plus 1n (external_example 42n)'
43
*)
