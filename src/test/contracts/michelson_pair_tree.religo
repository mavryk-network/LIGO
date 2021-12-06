type inner_storage = michelson_pair(int,"one",nat,"two");
type storage = michelson_pair(int,"three",inner_storage,"four");

type return = (list (operation) , storage);

let main = ((_action, _store) : (unit , storage)) : return => {
  let foo = { michelson_three : 3 , michelson_four : { michelson_one : 1, michelson_two : 2n} } ;
  (([] : list(operation)), (foo: storage))
};