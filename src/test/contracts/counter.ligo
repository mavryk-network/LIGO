type t is int

function main (const (p, s) : int * t) : list (operation) * int is
  block {
    skip
  } // skip is a do nothing instruction, needed for empty blocks
  with ((nil : list (operation)), p+s)
