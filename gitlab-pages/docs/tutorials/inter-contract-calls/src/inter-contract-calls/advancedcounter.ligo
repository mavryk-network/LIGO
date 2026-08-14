(* examples/contracts/ligo/AdvancedCounter.ligo *)

type parameter is
  Set of int | Add of int | Subtract of int | Multiply of int | Reset of unit

[@entry]
function main (const param : parameter; const storage : int) : list (operation) * int is
  block {
    const nop : list (operation) = nil
  } with
    case param of [
      Set (n) -> (nop, n)
    | Add (n) -> (nop, storage + n)
    | Subtract (n) -> (nop, storage - n)
    | Multiply (n) -> (nop, storage * n)
    | Reset (_u) -> (nop, 0)
    ]