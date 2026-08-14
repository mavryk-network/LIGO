type return is list (operation) * int

module A is {
  type storage is int

  [@entry]
  function add (const delta : int; const storage : storage) : return is
    ((nil : list (operation)), storage + delta)
}

module B is {
  type storage is int

  [@entry]
  function increment (const _param : unit; const storage : storage) : return is
    block {
      const contract_addr : contract (parameter_of A) =
        Mavryk.get_contract (("KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5" : address));
      const operation : operation =
        Mavryk.transaction ((Add (1) : parameter_of A), 0mav, contract_addr);
    } with (list [operation], storage)
}