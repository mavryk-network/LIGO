type inner_storage is michelson_pair(int,"one",nat,"two")
type storage is michelson_pair (string,"three",inner_storage,"four")

type return is list(operation) * storage

function main (const _action : unit; const _store : storage) : return is block {
  const foo : storage = record [
      michelson_three = "foo" ;
      michelson_four = record [ michelson_one = 1 ; michelson_two = 2n ] ;
  ] ;
} with ((nil : list(operation)), (foo: storage))