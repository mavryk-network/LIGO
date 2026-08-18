// MAVRYK: restored as a real file (was a symlink into the retired version-1.6.0 doc snapshot;
// see Counter.mligo). Original mv-vs-eth tutorial contract, kept for contract_test coverage.
type parameter = Compute of int -> int | Set of int

type storage = int

let main (p, s : parameter * storage) =
  match p with
    Compute func -> ([] : operation list), func s
  | Set n -> ([] : operation list), n
