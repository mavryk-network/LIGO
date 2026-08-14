type storage is unit

[@entry]
function main (const param : int * int; const _s : storage) : list (operation) * storage is
  (list [Mavryk.emit ("%foo", param); Mavryk.emit ("%bar", param.0)], Unit)