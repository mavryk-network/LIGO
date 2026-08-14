const y : int = -(-1) // In PascaLIGO
type t is list (int)
const x : t = list [42]
const res : t = 43 # x
type storage is string
type result is list (operation) * storage

[@entry] function hello (const _u : unit; const _store : storage) : result is
  ((nil : list (operation)), "hello")

[@entry] function big (const _u : unit; const store : storage) : result is
  ((nil : list (operation)), store ^ " big")

type planet is
  Earth of unit
| Mars of unit
| Earth2 of unit

[@view] function world (const p : planet; const store : storage) : string is
  block {
    const desc : string =
      case p of [
        Earth (_u)  -> " pale blue dot"
      | Mars (_u)   -> " pale red dot"
      | Earth2 (_u) -> failwith ("backup planet not found")
      ]
  } with store ^ " " ^ desc