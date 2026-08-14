// An importable PascaLIGO module, consumed cross-syntax by pascaligo_interop.mligo.
module C is {
  type storage is int
  [@entry] function add (const n : int; const s : storage) : list(operation) * storage is
    ((nil : list(operation)), s + n)
}
