function id<a> (const x : a) : a is x
const three_i : int = id (3);
const three_s : string = id ("three");
function rev<a> (const xs : list (a)) : list (a) is {
  var acc := (nil : list (a));
  for x in list xs { acc := x # acc; };
} with acc
const lint : list (int) = rev (list [1; 2; 3]);
const lnat : list (nat) = rev (list [1n; 2n; 3n]);