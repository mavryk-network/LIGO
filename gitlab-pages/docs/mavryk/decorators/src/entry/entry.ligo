type storage is int
type return is list (operation) * storage

module Foo is {
  [@entry] function decrement (const param : int; const s : storage) : return is
    ((nil : list (operation)), s - param)

  [@entry] function increment (const param : int; const s : storage) : return is
    ((nil : list (operation)), s + param)

  [@entry] function reset (const _u : unit; const _s : storage) : return is
    ((nil : list (operation)), 0)

  [@view] function get_storage (const _u : unit; const s : storage) : storage is s
}