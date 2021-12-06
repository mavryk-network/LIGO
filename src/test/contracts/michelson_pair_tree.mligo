type inner_storage = (int,"one",nat,"two") michelson_pair
type storage = (int,"three",inner_storage,"four") michelson_pair

type return = operation list * storage

let main (_action, _store : unit * storage) : return =
  let foo = { michelson_three = 3 ; michelson_four = {michelson_one = 1 ; michelson_two = 2n}} in
  (([] : operation list), (foo: storage))