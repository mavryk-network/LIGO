// PascaLIGO dynamic entrypoints (Mavryk dialect, 0.73 grammar).
// The punned `dynamic_entrypoints` field wires the dynamic-entrypoint big_map into storage;
// compile with `-m C`.

type storage is record [ storage : int; dynamic_entrypoints ]
type return_ is list(operation) * storage

module C is {
  [@dyn_entry] function foo (const _u : unit; const s : storage) : return_ is
    ((nil : list(operation)), s with record [ storage = 42 ])
  [@entry] function bar (const _u : unit; const s : storage) : return_ is
    ((nil : list(operation)), s with record [ storage = -1 ])
}
