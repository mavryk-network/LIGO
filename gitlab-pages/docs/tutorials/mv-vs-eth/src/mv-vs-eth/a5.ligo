type storage is int
type result is list (operation) * storage

function doMultiplyBy2 (const store : storage) : int is store * 2

function doMultiplyBy4 (const store : storage) : int is doMultiplyBy2 (doMultiplyBy2 (store))

[@entry] function multiplyBy4 (const _u : unit; const s : storage) : result is ((nil : list (operation)), doMultiplyBy4 (s))
[@entry] function multiplyBy16 (const _u : unit; const s : storage) : result is ((nil : list (operation)), doMultiplyBy4 (doMultiplyBy4 (s)))