let fst = ((f, _) : (int, int)) : int => f

let main = ((_, s) : (unit, int)) : (list(operation), int) => {
  let pair1 = (1, s);
  let pair2 = (3, s);
  (([] : list (operation)), s + fst(pair1) + fst(pair2))
};
