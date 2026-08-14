// PascaLIGO M3 feature showcase: module signatures, module-type annotation,
// and dynamic entrypoints (Mavryk dialect, 0.73 grammar). See PASCALIGO_SYNTAX_RFC.md.

module type STORE is sig
  type t
  const empty : t
end

module IntStore : STORE is {
  type t is int
  const empty : int = 0
}

type storage is int

[@entry] function add (const n : int; const s : storage) : list(operation) * storage is
  ((nil : list(operation)), s + n)

[@entry] function reset (const _u : unit; const _s : storage) : list(operation) * storage is
  ((nil : list(operation)), IntStore.empty)
