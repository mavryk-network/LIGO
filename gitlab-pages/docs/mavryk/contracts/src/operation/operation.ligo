type return is list (operation) * string

[@entry]
function main (const _p : string; const storage : string) : return is
  block {
    function entrypoint (const _n : nat; const st : string) : list (operation) * string is
      ((nil : list (operation)), st);
    const (op, _addr) : operation * address =
      Mavryk.create_contract (entrypoint, (None : option (key_hash)), 300000000mumav, "one");
  } with (list [op], storage)