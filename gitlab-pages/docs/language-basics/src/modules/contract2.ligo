module type FA0_SIG is sig
  type t
  [@entry] const transfer : unit -> t -> list (operation) * t
end

module type FA0Ext_SIG is sig
  include FA0_SIG
  [@entry] const transfer2 : unit -> t -> list (operation) * t
end
module FA0 : FA0_SIG is {
  type t is unit
  [@entry] function transfer (const _p : unit; const _s : t) : list (operation) * t is ((nil : list (operation)), Unit)
}

module FA0Ext : FA0Ext_SIG is {
  type t is unit
  [@entry] function transfer (const _p : unit; const _s : t) : list (operation) * t is ((nil : list (operation)), Unit)
  [@entry] function transfer2 (const a : unit; const b : t) : list (operation) * t is transfer (a, b)
}