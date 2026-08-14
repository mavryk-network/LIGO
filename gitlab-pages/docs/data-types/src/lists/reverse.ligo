function rev<a> (const xs : list (a)) : list (a) is {
  var acc := (nil : list (a));
  for x in list xs { acc := x # acc; };
} with acc
const ints : list (int) = rev (list [1; 2; 3])
const nats : list (nat) = rev (list [1n; 2n; 3n])