type storage is int
type parameter is int

type remote_param is Sub of int

[@entry]
function main (const _p : parameter; const s : storage) : list (operation) * storage is
  block {
    const contract_addr : contract (remote_param) =
      Mavryk.get_entrypoint (
        "%sub", // Corresponds to the `Sub` variant of `remote_param`.
        ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address))
  } with (list [Mavryk.transaction (Sub (2), 2mumav, contract_addr)], s)