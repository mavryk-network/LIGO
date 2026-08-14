module MyContract is {
  type storage_type is map (nat, string)
  type return_type is list (operation) * storage_type

  [@entry]
  function update (const param : nat * string; const storage : storage_type) : return_type is
    block {
      const (index, value) = param;
      const updated_map = Map.add (index, value, storage);
    } with ((nil : list (operation)), updated_map)
}