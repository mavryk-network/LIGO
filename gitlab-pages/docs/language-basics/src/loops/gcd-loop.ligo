function gcd (const a : nat; const b : nat) : nat is {
  var x : nat := a;
  var y : nat := b;                     // we will modify x and y
  if x < y then {
    const z : nat = x;
    x := y;
    y := z
  };
  var r : nat := 0n;
  while y =/= 0n {
    r := x mod y;
    x := y;
    y := r
  }
} with x