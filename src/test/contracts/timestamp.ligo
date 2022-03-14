type storage_ is timestamp

function main (const _p : unit; const _s : storage_) :
  list (operation) * storage_ is ((nil: list (operation)), Tezos.now)
