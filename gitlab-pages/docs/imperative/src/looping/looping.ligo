recursive function iter (const x : nat; const y : nat) : nat is
  if y = 0n then x else iter (y, x mod y)

function gcd (const x : nat; const y : nat) : nat is
  if x < y then iter (y, x) else iter (x, y)
function gcd (const a : nat; const b : nat) : nat is
  block {
    var x : nat := a;
    var y : nat := b;
    if x < y then {
      const z : nat = x;
      x := y;
      y := z              // Swapping x and y
    };
    var r : nat := 0n;
    while y =/= 0n {
      r := x mod y;
      x := y;
      y := r
    }
  } with x
function get_char (const s : string; const idx : nat) : string is
  String.sub (idx, 1n, s)

function is_palindrome (const s : string) : bool is
  block {
    var p : string := "";
    const length : nat = String.length (s);
    for i := int (length) - 1 to 0 step -1 {
      p := p ^ get_char (s, abs (i))
    }
  } with p = s