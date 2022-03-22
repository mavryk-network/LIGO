type storage_ is timestamp

function main (const (p, s) : unit * storage_) :
  list (operation) * storage_ is ((nil: list (operation)), now)
