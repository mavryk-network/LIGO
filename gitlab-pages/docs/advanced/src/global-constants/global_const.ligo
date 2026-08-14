function helper (const (s, x) : string * int) : int is
  String.length (s) + x * 3 + 2

[@entry]
function main (const p : string; const s : int) : list (operation) * int is
  ((nil : list (operation)), helper ((p, s)))