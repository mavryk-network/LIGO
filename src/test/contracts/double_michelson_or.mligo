type storage = (int,"foo",string,"bar") michelson_or 
type foobar = (int,"baz", int, "fooo" ) michelson_or

type return = operation list * storage

let main (_action, _store : unit * storage) : return =
  let foo = (M_right ("one") : storage) in
  let _bar = (M_right 1 : foobar) in
  (([] : operation list), (foo: storage))
