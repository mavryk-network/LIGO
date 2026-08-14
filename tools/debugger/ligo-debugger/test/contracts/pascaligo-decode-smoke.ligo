// MAVRYK: PascaLIGO. Runtime-verifies the PascaLigoCST.hs decoder end-to-end
// (ligo info dump-cst --format msgpack -> MessagePack decode -> Skeleton AST) over a
// broad core-language subset. Every construct below decodes to a non-empty AST,
// including record construction/update fields (E_Record / E_Update `record [f = e]`).
type storage is record [ count : int; owner : address ]
type action is Inc of int | Reset of unit
type ids is list (nat)
type ledger is map (address, nat)
type maybe is option (int)

const forty_two : int = 42

const empty : list (int) = nil

function add (const a : int; const b : int) : int is a + b

function handle (const s : int) : int is
  block {
    var c : int := s;
    const l : int = c
  } with c

function classify (const p : action) : string is
  case p of [ Inc (n) -> "i" | Reset (_u) -> "r" ]

function get_count (const s : storage) : int is s.count

function bump (const s : storage) : storage is s with record [ count = 42 ]

function unwrap (const s : storage) : int is
  case s of [ record [ count = c; owner = _o ] -> c ]

function noop (const _u : unit) : unit is
  block { skip } with unit

module Util is {
  const one : int = 1
}
