type storage = int;

type parameter = list (int);

type return = (list (operation), storage);

let hd = (x : list (int)) : int =>
  switch x {
    | [] => => -1
    | [x, ...xs] => x
  };

let main = (a, b : (parameter, storage)) : return =>
  ([] : list (operation), hd (a) + (b + 8) * 11);
