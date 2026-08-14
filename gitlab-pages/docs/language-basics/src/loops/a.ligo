function get_char (const s : string; const idx : nat) : string is
  String.sub (idx, 1n, s)

function is_palindrome (const s : string) : bool is {
  var p : string := "";
  const length : nat = String.length (s);
  for i := 0 to int (length) - 1 {
    p := get_char (s, abs (i)) ^ p
  }
} with p = s