type player is string
type abscissa is int
type ordinate is int
type move is abscissa * ordinate
type game is map (player, move)

function horizontal_offset (const g : game) : int is
  block {
    function folded (const p : int * (player * move)) : int is
      p.0 + p.1.1.0
  } with Map.fold (folded, g, 0)