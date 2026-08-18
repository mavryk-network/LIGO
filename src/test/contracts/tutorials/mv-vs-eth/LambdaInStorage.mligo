// MAVRYK: restored as a real file (was a symlink into the retired version-1.6.0 doc snapshot;
// see Counter.mligo). Original mv-vs-eth tutorial contract, kept for contract_test coverage.
type storage = {fn : (int -> int) option; value : int}

type parameter = SetFunction of (int -> int) | CallFunction

let call (fn, value : (int -> int) option * int) =
  match fn with
    Some f -> f value
  | None -> (failwith "Lambda is not set" : int)

let main (p, s : parameter * storage) =
  let newStorage =
    match p with
      SetFunction fn -> {s with fn = Some fn}
    | CallFunction -> {s with value = call (s.fn, s.value)} in
  ([] : operation list), newStorage
