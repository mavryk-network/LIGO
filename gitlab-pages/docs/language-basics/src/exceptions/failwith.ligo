type storage is unit
type result is list (operation) * storage

[@entry] function main (const _param : unit; const _store : storage) : result is
  failwith ("This contract always fails.")